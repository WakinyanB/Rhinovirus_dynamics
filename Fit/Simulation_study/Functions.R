extract_parms_rv <- function(parms, init){
  
  names(parms)[names(parms)=="phi_iav"] <- "phi"
  parms <- parms[names(parms)!="phi_rv"]
  parms <- parms[-grep('iav', names(parms))]
  names(parms) <- gsub("k_rv", "kappa", names(parms))
  names(parms) <- gsub("_rv", "", names(parms))
  
  parms$S0 <- init$S/parms$pop
  parms$I0 <- init$I/parms$pop
  
  return(parms)
}

summary_posterior <- function(posterior, parm_names){
  
  return_beta <- FALSE
  return_phi <- FALSE
  return_tab <- FALSE
  
  if("beta" %in% parm_names){
    beta <- data.frame("week"=1:52,
                       "mean"=apply(posterior$beta, 2, mean),
                       "median"=apply(posterior$beta, 2, median),
                       "CI95_lower"=apply(posterior$beta, 2, quantile, probs=0.025),
                       "CI95_upper"=apply(posterior$beta, 2, quantile, probs=0.975))
    
    parm_names <- parm_names[-which(parm_names=="beta")]
    return_beta <- TRUE
  }
  if(length(parm_names)!=0){
    tab <- parm_names %>% lapply(function(x){
      i <- which(names(posterior)==x)
      if(length(i)!=0){
        return(data.frame("parm"=names(posterior[i]),
                          "mean"=mean(posterior[[i]]),
                          "median"=median(posterior[[i]]),
                          "CI95_lower"=quantile(posterior[[i]], probs=0.025),
                          "CI95_upper"=quantile(posterior[[i]], probs=0.975),
                          row.names=NULL))
      }
    }) %>% bind_rows
    return_tab <- TRUE
  }
  if(return_beta & !return_tab){
    return(beta)
  }else if(return_tab & !return_beta){
    return(tab)
  }else{
    return(list("beta"=beta, "other_parms"=tab))
  }
}

Sojourn_time_R <- function(s_omega, dt=1){
  
  DR <- dt/(1-exp(-s_omega[,-which(colnames(s_omega)=="parm")]*dt))
  to_switch <- c(which(colnames(DR)=="CI95_lower"), which(colnames(DR)=="CI95_upper"))
  DR[,to_switch] <- DR[,rev(to_switch)]
  
  return(data.frame(parm="DR", DR))
}

Relative_error <- function(s, real_parms, dt=1){
  
  real_parms$DR <- 1/real_parms$omega
  s[which(s$parm=="omega"),] <- s[which(s$parm=="omega"),] %>% Sojourn_time_R
  
  return(s %>% ddply(~parm, function(X){
    return(X[,-which(colnames(X)=="parm")]/real_parms[[match(X$parm, names(real_parms))]]-1)
  })
  )
}

Plot_rel_err <- function(s_sdlow, s_npi_sdlow, s_vi_sdlow, s_npi_vi_sdlow,
                         s_sdhigh, s_npi_sdhigh, s_vi_sdhigh, s_npi_vi_sdhigh,
                         real_parms, dt=1){
  rel_err <- rbind(
    data.frame(sd_obs="low", npi="0", vi="0", Relative_error(s_sdlow, real_parms, dt=dt)),
    data.frame(sd_obs="low", npi="1", vi="0", Relative_error(s_npi_sdlow, real_parms, dt=dt)),
    data.frame(sd_obs="low", npi="0", vi="1", Relative_error(s_vi_sdlow, real_parms, dt=dt)),
    data.frame(sd_obs="low", npi="1", vi="1", Relative_error(s_npi_vi_sdlow, real_parms, dt=dt)),
    
    data.frame(sd_obs="high", npi="0", vi="0", Relative_error(s_sdhigh, real_parms, dt=dt)),
    data.frame(sd_obs="high", npi="1", vi="0", Relative_error(s_npi_sdhigh, real_parms, dt=dt)),
    data.frame(sd_obs="high", npi="0", vi="1", Relative_error(s_vi_sdhigh, real_parms, dt=dt)),
    data.frame(sd_obs="high", npi="1", vi="1", Relative_error(s_npi_vi_sdhigh, real_parms, dt=dt))
  ) %>%
    mutate(parm=factor(parm, levels=c("kappa", "DR", "rho", "S0", "I0")))
  
  pd <- position_dodge(width=0.5)
  label_col <- c('0 (fixed)', 'estimated')
  
  return(
    rel_err %>%
      mutate(npi=factor(npi, levels=c("0","1"),
                        labels=c("Prepandemic", "Whole~dataset")),
             sd_obs=factor(sd_obs, levels=c("low","high"),
                           labels=c("Low~obs.~noise~(sigma==0.08)", "High~obs.~noise~(sigma==0.4)"))) %>%
      ggplot(aes(x=parm, y=median, fill=vi)) +
      geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
      facet_grid(npi~sd_obs, labeller=label_parsed) +
      labs(y="Relative error", col=expression(phi), fill=expression(phi)) +
      geom_segment(aes(y=CI95_lower, yend=CI95_upper, col=vi), linewidth=0.4, position=pd) +
      geom_point(pch=21, size=1.5, stroke=0.1, position=pd) +
      scale_x_discrete(labels=c(expression(hat(kappa)), expression(hat(Omega)), expression(hat(rho)),
                                expression(hat(S)[0]), expression(hat(I)[0]))) +
      scale_y_continuous(labels=percent_format()) +
      scale_color_manual(values=c("#800080", "#FFA500"), labels=label_col) +
      scale_fill_manual(values=c("#800080", "#FFA500"), labels=label_col) +
      theme_test() +
      theme(axis.title.x=element_blank(), axis.text.x=element_text(size=11))
  )
}

R0_estim <- function(beta_estim, gamma, mu, dt=1){
  return(data.frame("week"=1:52, beta_estim[,-1]*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu))
}

Plot_R0 <- function(beta_sdlow, beta_npi_sdlow, beta_vi_sdlow, beta_npi_vi_sdlow,
                    beta_sdhigh, beta_npi_sdhigh, beta_vi_sdhigh, beta_npi_vi_sdhigh,
                    real_parms, custom_beta=FALSE, dt=1){
  
  gamma <- -log(1-real_parms$gamma*dt)/dt
  
  R0_estim_tab <- rbind(
    
    data.frame(sd_obs="low", npi="0", vi="0", R0_estim(beta_sdlow, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="low", npi="1", vi="0", R0_estim(beta_npi_sdlow, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="low", npi="0", vi="1", R0_estim(beta_vi_sdlow, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="low", npi="1", vi="1", R0_estim(beta_npi_vi_sdlow, gamma, real_parms$mu, dt)),
    
    data.frame(sd_obs="high", npi="0", vi="0", R0_estim(beta_sdhigh, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="high", npi="1", vi="0", R0_estim(beta_npi_sdhigh, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="high", npi="0", vi="1", R0_estim(beta_vi_sdhigh, gamma, real_parms$mu, dt)),
    data.frame(sd_obs="high", npi="1", vi="1", R0_estim(beta_npi_vi_sdhigh, gamma, real_parms$mu, dt))
  )
  
  label_col <- c('0 (fixed)', 'estimated')
  
  if(custom_beta){
    seasonal_beta <- real_parms$beta
  }else{
    seasonal_beta <- real_parms$b0*(1+real_parms$a1*cos(4*pi*((1:52)/52-real_parms$d1))+
                                  real_parms$a2*cos(2*pi*((1:52)/52-(real_parms$d1+real_parms$d2))))
  }
  
  R0_estim_tab %>%
    mutate(npi=factor(npi, levels=c("0","1"), labels=c("Prepandemic", "Whole~dataset")),
           sd_obs=factor(sd_obs, levels=c("low","high"),
                         labels=c("Low~obs.~noise~(sigma==0.08)", "High~obs.~noise~(sigma==0.4)"))) %>%
    ggplot(aes(x=week)) +
    facet_grid(npi~sd_obs, labeller=label_parsed) +
    geom_ribbon(aes(ymin=CI95_lower, ymax=CI95_upper, col=vi, fill=vi), lwd=0.15, alpha=0.25) +
    geom_line(aes(y=median, col=vi)) +
    labs(y="Basic reproduction number", col=expression(phi), fill=expression(phi)) +
    geom_line(data=data.frame("week"=1:52,
                              "R0_real"=seasonal_beta/(real_parms$gamma+real_parms$mu)), aes(y=R0_real), lty='dashed', lwd=0.2) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=c("#800080", "#FFA500"), labels=label_col) +
    scale_fill_manual(values=c("#800080", "#FFA500"), labels=label_col) +
    theme_test() +
    theme(legend.text=element_text(size=11), legend.title=element_text(size=17))
}

Plot_phi <- function(p_vi_sdlow, p_npi_vi_sdlow, p_vi_sdhigh, p_npi_vi_sdhigh,
                     obs_iav, SIR, real_parms){
  
  n_prepand <- ncol(p_vi_sdlow$cases)
  
  I_IAV <- subset(SIR,pathogen=="IAV")$I
  
  range_vi_real <- rbind(
    data.frame("npi"="Prepandemic",
               "vi_min"=min(real_parms$phi*I_IAV[1:n_prepand]/real_parms$pop),
               "vi_max"=max(real_parms$phi*I_IAV[1:n_prepand]/real_parms$pop)),
    data.frame("npi"="Whole dataset",
               "vi_min"=min(real_parms$phi*I_IAV/real_parms$pop),
               "vi_max"=max(real_parms$phi*I_IAV/real_parms$pop))
  )
  
  IAV_sdlow <- obs_iav$smooth_cases_noisy_sdlow/max(obs_iav$smooth_cases_noisy_sdlow)
  IAV_sdhigh <- obs_iav$smooth_cases_noisy_sdhigh/max(obs_iav$smooth_cases_noisy_sdhigh)
  
  phi_distrib <- rbind(
    data.frame("phi"=p_vi_sdlow$phi*max(IAV_sdlow[1:n_prepand]), npi="Prepandemic", sd_obs="0.08"),
    data.frame("phi"=p_npi_vi_sdlow$phi*max(IAV_sdlow), npi="Whole dataset", sd_obs="0.08"),
    data.frame("phi"=p_vi_sdhigh$phi*max(IAV_sdhigh[1:n_prepand]), npi="Prepandemic", sd_obs="0.4"),
    data.frame("phi"=p_npi_vi_sdhigh$phi*max(IAV_sdhigh), npi="Whole dataset", sd_obs="0.4")
  )
  
  return(
    phi_distrib %>% ddply(~npi+sd_obs, function(X){
      return(
        data.frame("median"=median(X$phi),
                   "CI95_lower"=quantile(X$phi, probs=0.025),
                   "CI95_upper"=quantile(X$phi, probs=0.975),
                   row.names=NULL)
        )
      }) %>% bind_rows %>%
      ggplot() +
      facet_wrap(~npi, nrow=2) +
      geom_hline(yintercept=0, linewidth=0.6, col="grey70") +
      geom_hline(data=range_vi_real, aes(yintercept=vi_min), linewidth=0.3, lty='dashed') +
      
      labs(x=expression(sigma),
           y="Max. change due to viral interaction") +
      geom_violin(data=phi_distrib, aes(x=sd_obs, y=phi, fill=sd_obs), trim=FALSE, col=NA, alpha=0.5) +
      geom_segment(aes(x=sd_obs, y=CI95_lower, yend=CI95_upper), linewidth=0.25) +
      geom_point(aes(x=sd_obs, y=median, fill=sd_obs), shape=21, col="black", size=1.5) +
      scale_y_continuous(labels=percent_format()) +
      scale_fill_manual(values=c("#0F3F87", "#AED4E7")) +
      theme_test() +
      theme(axis.title.x=element_text(size=15),
            legend.position='none')
    )
  }

summary_LL <- function(posterior, first_index_pandemic=NA){
  
  colnames <- c("mean", "CI95_lower", "CI95_upper")
  data_id <- c("All", "Prepandemic", "(Post)pandemic")
  LL_all <- posterior$log_lik %>% apply(1, sum)
  LL_all_summary <- c(mean(LL_all), quantile(LL_all, probs=c(0.025,0.975)))
  
  if(is.na(first_index_pandemic)){
    return(LL_all_summary %>% t %>% as.data.frame %>% setNames(colnames) %>%
             data.frame("data"=factor("All", levels=data_id),.))
  }else{
    LL_pre <- posterior$log_lik[,1:(first_index_pandemic-1)] %>% apply(1, sum)
    LL_pre_summary <- c(mean(LL_pre), quantile(LL_pre, probs=c(0.025,0.975)))
    
    LL_post <- posterior$log_lik[,first_index_pandemic:ncol(posterior$log_lik)] %>% apply(1, sum)
    LL_post_summary <- c(mean(LL_post), quantile(LL_post, probs=c(0.025,0.975)))
    
    LL_summary <- rbind(LL_all_summary, LL_pre_summary, LL_post_summary) %>%
      as.data.frame(row.names=NA) %>%
      setNames(colnames)
    LL_summary$data <- factor(data_id, levels=data_id)
    return(LL_summary[,c(4,1:3)])
  }
}

Plot_LL <- function(p_sdlow, p_npi_sdlow, p_vi_sdlow, p_npi_vi_sdlow,
                    p_sdhigh, p_npi_sdhigh, p_vi_sdhigh, p_npi_vi_sdhigh,
                    
                    p_sdlow_kick, p_npi_sdlow_kick, p_vi_sdlow_kick, p_npi_vi_sdlow_kick,
                    p_sdhigh_kick, p_npi_sdhigh_kick, p_vi_sdhigh_kick, p_npi_vi_sdhigh_kick,
                    
                    p_sdlow_shift, p_npi_sdlow_shift, p_vi_sdlow_shift, p_npi_vi_sdlow_shift,
                    p_sdhigh_shift, p_npi_sdhigh_shift, p_vi_sdhigh_shift, p_npi_vi_sdhigh_shift,
                    
                    colors=c("#800080", "#FFA500")){
  
  i <- ncol(p_sdlow$cases) + 1 # first_index_pandemic
  
  LL <- rbind(
    
    # Endemic attractor
    data.frame(id="endemic", sd_obs="low", npi=0, vi="0", summary_LL(p_sdlow)),
    data.frame(id="endemic", sd_obs="low", npi=1, vi="0", summary_LL(p_npi_sdlow, first_index_pandemic=i)),
    data.frame(id="endemic", sd_obs="low", npi=0, vi="1", summary_LL(p_vi_sdlow)),
    data.frame(id="endemic", sd_obs="low", npi=1, vi="1", summary_LL(p_npi_vi_sdlow, first_index_pandemic=i)),
    
    data.frame(id="endemic", sd_obs="high", npi=0, vi="0", summary_LL(p_sdhigh)),
    data.frame(id="endemic", sd_obs="high", npi=1, vi="0", summary_LL(p_npi_sdhigh, first_index_pandemic=i)),
    data.frame(id="endemic", sd_obs="high", npi=0, vi="1", summary_LL(p_vi_sdhigh)),
    data.frame(id="endemic", sd_obs="high", npi=1, vi="1", summary_LL(p_npi_vi_sdhigh, first_index_pandemic=i)),
    
    # IAV perturbation
    data.frame(id="kick", sd_obs="low", npi="0", vi="0", summary_LL(p_sdlow_kick)),
    data.frame(id="kick", sd_obs="low", npi="1", vi="0", summary_LL(p_npi_sdlow_kick, first_index_pandemic=i)),
    data.frame(id="kick", sd_obs="low", npi="0", vi="1", summary_LL(p_vi_sdlow_kick)),
    data.frame(id="kick", sd_obs="low", npi="1", vi="1", summary_LL(p_npi_vi_sdlow_kick, first_index_pandemic=i)),
    
    data.frame(id="kick", sd_obs="high", npi="0", vi="0", summary_LL(p_sdhigh_kick)),
    data.frame(id="kick", sd_obs="high", npi="1", vi="0", summary_LL(p_npi_sdhigh_kick, first_index_pandemic=i)),
    data.frame(id="kick", sd_obs="high", npi="0", vi="1", summary_LL(p_vi_sdhigh_kick)),
    data.frame(id="kick", sd_obs="high", npi="1", vi="1", summary_LL(p_npi_vi_sdhigh_kick, first_index_pandemic=i)),
    
    # NPI shift
    data.frame(id="shift", sd_obs="low", npi="0", vi="0", summary_LL(p_sdlow_shift)),
    data.frame(id="shift", sd_obs="low", npi="1", vi="0", summary_LL(p_npi_sdlow_shift, first_index_pandemic=i)),
    data.frame(id="shift", sd_obs="low", npi="0", vi="1", summary_LL(p_vi_sdlow_shift)),
    data.frame(id="shift", sd_obs="low", npi="1", vi="1", summary_LL(p_npi_vi_sdlow_shift, first_index_pandemic=i)),
    
    data.frame(id="shift", sd_obs="high", npi="0", vi="0", summary_LL(p_sdhigh_shift)),
    data.frame(id="shift", sd_obs="high", npi="1", vi="0", summary_LL(p_npi_sdhigh_shift, first_index_pandemic=i)),
    data.frame(id="shift", sd_obs="high", npi="0", vi="1", summary_LL(p_vi_sdhigh_shift)),
    data.frame(id="shift", sd_obs="high", npi="1", vi="1", summary_LL(p_npi_vi_sdhigh_shift, first_index_pandemic=i))
  )
  
  LL$period <- NA
  LL$period[LL$data=="All" & LL$npi=="0"] <- "Prepandemic"
  LL$period[LL$data=="All" & LL$npi=="1"] <- "Whole~dataset"
  LL$period[LL$data=="Prepandemic"] <- "Whole~dataset~(years~1-6)"
  LL$period[LL$data=="(Post)pandemic"] <- "Whole~dataset~(years~7-10)"
  
  pd <- position_dodge(width=0.5)
  
  return(
    LL %>%
      # subset(sd_obs=="low") %>%
      mutate(sd_obs=factor(sd_obs, levels=c("low","high"),
                           labels=c("Low~obs.~noise~(sigma==0.08)", "High~obs.~noise~(sigma==0.4)")),
             id=factor(id, levels=c("endemic", "shift", "kick"),
                       labels=c("Endemic attractor", "6-month shift", "With IAV perturbation"))) %>%
      ggplot(aes(x=id)) +
      facet_grid(sd_obs~period, labeller=label_parsed, scales='free_y') +
      labs(y="Log-likelihood\n") +
      geom_segment(aes(y=CI95_lower, yend=CI95_upper, col=vi), position=pd) +
      geom_point(aes(y=mean, fill=vi), pch=21, cex=1.5, position=pd) +
      scale_color_manual(values=colors) +
      scale_fill_manual(values=colors) +
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_text(size=7, angle=30, hjust=1))
    )
}

summary_cor <- function(posterior, data, first_index_pandemic=NA, log_scale=TRUE){
  
  if(log_scale){
    data <- log(data)
    posterior$cases <- log(posterior$cases)
  }
  
  n <- ncol(posterior$cases)
  data <- data[1:n]
  colnames <- c("mean", "CI95_lower", "CI95_upper")
  data_id <- c("All", "Prepandemic", "(Post)pandemic")
  cor_all <- posterior$cases %>% apply(1, cor, y=data)
  cor_all_summary <- c(mean(cor_all), quantile(cor_all, probs=c(0.025,0.975)))
  
  if(is.na(first_index_pandemic)){
    return(cor_all_summary %>% t %>% as.data.frame %>% setNames(colnames) %>%
             data.frame("data"=factor("All", levels=data_id),.))
  }else{
    
    cor_pre <- posterior$cases[,1:(first_index_pandemic-1)] %>% apply(1, cor, y=data[1:(first_index_pandemic-1)])
    cor_pre_summary <- c(mean(cor_pre), quantile(cor_pre, probs=c(0.025,0.975)))
    
    cor_post <- posterior$cases[,first_index_pandemic:n] %>% apply(1, cor, y=data[first_index_pandemic:n])
    cor_post_summary <- c(mean(cor_post), quantile(cor_post, probs=c(0.025,0.975)))
    
    cor_summary <- rbind(cor_all_summary, cor_pre_summary, cor_post_summary) %>%
      as.data.frame(row.names=NA) %>% setNames(colnames)
    cor_summary$data <- factor(data_id, levels=data_id)
    return(cor_summary[,c(4,1:3)])
  }
}

Plot_cor <- function(data, data_kick, data_shift,
                     p_sdlow, p_npi_sdlow, p_vi_sdlow, p_npi_vi_sdlow,
                     p_sdhigh, p_npi_sdhigh, p_vi_sdhigh, p_npi_vi_sdhigh,
                     
                     p_sdlow_kick, p_npi_sdlow_kick, p_vi_sdlow_kick, p_npi_vi_sdlow_kick,
                     p_sdhigh_kick, p_npi_sdhigh_kick, p_vi_sdhigh_kick, p_npi_vi_sdhigh_kick,
                     
                     p_sdlow_shift, p_npi_sdlow_shift, p_vi_sdlow_shift, p_npi_vi_sdlow_shift,
                     p_sdhigh_shift, p_npi_sdhigh_shift, p_vi_sdhigh_shift, p_npi_vi_sdhigh_shift,
                     
                     colors=c("#800080", "#FFA500"), log_scale=TRUE, simple=FALSE){
  
  i <- ncol(p_sdlow$cases) + 1 # first_index_pandemic
  col_labels <- c('0 (fixed)', 'estimated')
  
  tab_cor <- rbind(
    
    # Endemic attractor
    data.frame(id="endemic", sd_obs="low", npi="0", vi="0", summary_cor(p_sdlow, data$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="low", npi="1", vi="0", summary_cor(p_npi_sdlow, data$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="low", npi="0", vi="1", summary_cor(p_vi_sdlow, data$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="low", npi="1", vi="1", summary_cor(p_npi_vi_sdlow, data$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    
    data.frame(id="endemic", sd_obs="high", npi="0", vi="0", summary_cor(p_sdhigh, data$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="high", npi="1", vi="0", summary_cor(p_npi_sdhigh, data$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="high", npi="0", vi="1", summary_cor(p_vi_sdhigh, data$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="endemic", sd_obs="high", npi="1", vi="1", summary_cor(p_npi_vi_sdhigh, data$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale)),
    
    # IAV perturbation
    data.frame(id="kick", sd_obs="low", npi="0", vi="0", summary_cor(p_sdlow_kick, data_kick$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="low", npi="1", vi="0", summary_cor(p_npi_sdlow_kick, data_kick$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="low", npi="0", vi="1", summary_cor(p_vi_sdlow_kick, data_kick$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="low", npi="1", vi="1", summary_cor(p_npi_vi_sdlow_kick, data_kick$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    
    data.frame(id="kick", sd_obs="high", npi="0", vi="0", summary_cor(p_sdhigh_kick, data_kick$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="high", npi="1", vi="0", summary_cor(p_npi_sdhigh_kick, data_kick$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="high", npi="0", vi="1", summary_cor(p_vi_sdhigh_kick, data_kick$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="kick", sd_obs="high", npi="1", vi="1", summary_cor(p_npi_vi_sdhigh_kick, data_kick$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale)),
    
    # NPI shift
    data.frame(id="shift", sd_obs="low", npi="0", vi="0", summary_cor(p_sdlow_shift, data_shift$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="low", npi="1", vi="0", summary_cor(p_npi_sdlow_shift, data_shift$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="low", npi="0", vi="1", summary_cor(p_vi_sdlow_shift, data_shift$cases_noisy_sdlow, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="low", npi="1", vi="1", summary_cor(p_npi_vi_sdlow_shift, data_shift$cases_noisy_sdlow, first_index_pandemic=i, log_scale=log_scale)),
    
    data.frame(id="shift", sd_obs="high", npi="0", vi="0", summary_cor(p_sdhigh_shift, data_shift$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="high", npi="1", vi="0", summary_cor(p_npi_sdhigh_shift, data_shift$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="high", npi="0", vi="1", summary_cor(p_vi_sdhigh_shift, data_shift$cases_noisy_sdhigh, log_scale=log_scale)),
    data.frame(id="shift", sd_obs="high", npi="1", vi="1", summary_cor(p_npi_vi_sdhigh_shift, data_shift$cases_noisy_sdhigh, first_index_pandemic=i, log_scale=log_scale))
  )
  
  tab_cor$period <- NA
  tab_cor$period[tab_cor$data=="All" & tab_cor$npi=="0"] <- "Prepandemic"
  tab_cor$period[tab_cor$data=="All" & tab_cor$npi=="1"] <- "Whole~dataset"
  
  if(simple){
    tab_cor <- tab_cor[-which(is.na(tab_cor$period)),]
  }else{
    tab_cor$period[tab_cor$data=="Prepandemic"] <- "Whole~dataset~(years~1-6)"
    tab_cor$period[tab_cor$data=="(Post)pandemic"] <- "Whole~dataset~(years~7-10)"
  }
  
  pd <- position_dodge(width=0.5)
  
  return(
    tab_cor %>%
      # subset(sd_obs=="low") %>%
      mutate(sd_obs=factor(sd_obs, levels=c("low","high"),
                           labels=c("Low~obs.~noise~(sigma==0.08)", "High~obs.~noise~(sigma==0.4)")),
             id=factor(id, levels=c("endemic", "shift", "kick"),
                       labels=c("A) Original simulation", "B) NPI shift", "C) IAV kick"))) %>%
      ggplot(aes(x=id)) +
      facet_grid(sd_obs~period, labeller=label_parsed, scales='free_y') +
      labs(y="Pearson correlation", col=expression(phi), fill=expression(phi)) +
      geom_segment(aes(y=CI95_lower, yend=CI95_upper, col=vi), position=pd) +
      geom_point(aes(y=mean, fill=vi), pch=21, cex=2, position=pd) +
      scale_color_manual(values=colors, labels=col_labels) +
      scale_fill_manual(values=colors, labels=col_labels) +
      theme_bw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_text(size=9, angle=40, hjust=1),
            legend.text=element_text(size=11), legend.title=element_text(size=17))
  )
}

summary_fitted_values <- function(obs_cases, posterior_cases, shift=0){
  n <- ncol(posterior_cases)
  return(
    data.frame("time"=(1:n)/52+shift,
               "obs"=obs_cases[1:n],
               "mean"=apply(posterior_cases, 2, mean),
               apply(posterior_cases, 2, quantile, probs=c(0.025,0.5,0.975)) %>%
                 t %>% as.data.frame %>% setNames(c("lower", "median", "upper")))
    )
}

Plot_fit <- function(obs,
                     p_sdlow, p_npi_sdlow, p_vi_sdlow, p_npi_vi_sdlow,
                     p_sdhigh, p_npi_sdhigh, p_vi_sdhigh, p_npi_vi_sdhigh,
                     shift=0, colors=c("#800080", "#FFA500")){ 
  
  col_labels <- c('0 (fixed)', 'estimated')
  
  return(
    rbind(
      data.frame(sd_obs="low", npi="0", vi="0", summary_fitted_values(obs$cases_noisy_sdlow, p_sdlow$cases, shift=shift)),
      data.frame(sd_obs="low", npi="1", vi="0", summary_fitted_values(obs$cases_noisy_sdlow, p_npi_sdlow$cases, shift=shift)),
      data.frame(sd_obs="low", npi="0", vi="1", summary_fitted_values(obs$cases_noisy_sdlow, p_vi_sdlow$cases, shift=shift)),
      data.frame(sd_obs="low", npi="1", vi="1", summary_fitted_values(obs$cases_noisy_sdlow, p_npi_vi_sdlow$cases, shift=shift)),
      
      data.frame(sd_obs="high", npi="0", vi="0", summary_fitted_values(obs$cases_noisy_sdhigh, p_sdhigh$cases, shift=shift)),
      data.frame(sd_obs="high", npi="1", vi="0", summary_fitted_values(obs$cases_noisy_sdhigh, p_npi_sdhigh$cases, shift=shift)),
      data.frame(sd_obs="high", npi="0", vi="1", summary_fitted_values(obs$cases_noisy_sdhigh, p_vi_sdhigh$cases, shift=shift)),
      data.frame(sd_obs="high", npi="1", vi="1", summary_fitted_values(obs$cases_noisy_sdhigh, p_npi_vi_sdhigh$cases, shift=shift))
      ) %>%
      mutate(npi=factor(npi, levels=c("0","1"), labels=c("Prepandemic", "Whole~dataset")),
             sd_obs=factor(sd_obs, levels=c("low","high"),
                           labels=c("sigma==0.08", "sigma==0.4"))) %>%
      ggplot(aes(x=time)) +
      facet_grid(sd_obs~npi, labeller=label_parsed, scales='free') +
      labs(x="Time (years)", y="RV detections (log scale)", col=expression(phi), fill=expression(phi)) +
      geom_vline(xintercept=ncol(p_sdlow$cases)/52+shift, linewidth=0.4) +
      geom_point(aes(y=log(obs)), cex=0.4, stroke=0.01, pch=21) +
      # geom_point(aes(y=log(obs)), cex=0.1) +
      geom_ribbon(aes(ymin=log(lower), ymax=log(upper), fill=vi), alpha=0.25) +
      geom_line(aes(y=log(median), col=vi), linewidth=0.2) +
      scale_x_continuous(expand=c(0.001,0), breaks=0:9) +
      scale_y_continuous(expand=c(0,0), breaks=1:10) +
      scale_color_manual(values=colors, labels=col_labels) +
      scale_fill_manual(values=colors, labels=col_labels) +
      theme_test() +
      theme(axis.title.x=element_text(size=9),
            axis.title.y=element_text(size=9), axis.text.y=element_text(size=8))
  )
}
