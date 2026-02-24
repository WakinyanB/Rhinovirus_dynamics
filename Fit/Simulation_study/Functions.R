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

Plot_rel_err <- function(s, s_npi, s_vi, s_npi_vi, real_parms, dt=1){
  
  rel_err <- rbind(
    data.frame(npi="0", vi="0", Relative_error(s, real_parms, dt=dt)),
    data.frame(npi="1", vi="0", Relative_error(s_npi, real_parms, dt=dt)),
    data.frame(npi="0", vi="1", Relative_error(s_vi, real_parms, dt=dt)),
    data.frame(npi="1", vi="1", Relative_error(s_npi_vi, real_parms, dt=dt))
  ) %>%
    mutate(parm=factor(parm, levels=c("kappa", "DR", "rho", "S0", "I0")))
  
  pd <- position_dodge(width=0.5)
  label_col <- c('0 (fixed)', 'estimated')
  
  return(
    rel_err %>%
      mutate(npi=factor(npi, levels=c("0","1"),
                        labels=c("Pre-pandemic data", "Including\n(post-)pandemic data"))) %>%
      ggplot(aes(x=parm, y=median, fill=vi)) +
      geom_hline(yintercept=0, lty="dashed", linewidth=0.3) +
      facet_grid(~npi) +
      labs(y="Relative error", col=expression(phi), fill=expression(phi)) +
      geom_segment(aes(y=CI95_lower, yend=CI95_upper, col=vi), linewidth=0.4, position=pd) +
      geom_point(pch=21, size=1.5, stroke=0.1, position=pd) +
      scale_x_discrete(labels=c(expression(hat(kappa)), expression(hat(Omega)), expression(hat(rho)),
                                expression(hat(S)[0]), expression(hat(I)[0]))) +
      scale_y_continuous(labels=percent_format(), breaks=seq(-3,3,0.5)) +
      scale_color_manual(values=c("#FFA500", "#800080"), labels=label_col) +
      scale_fill_manual(values=c("#FFA500", "#800080"), labels=label_col) +
      theme_test() +
      theme(axis.title.x=element_blank(), axis.text.x=element_text(size=11),
            legend.text=element_text(size=11), legend.title=element_text(size=17))
  )
}

R0_estim <- function(beta_estim, gamma, mu, dt=1){
  return(data.frame("week"=1:52, beta_estim[,-1]*(1-exp(-mu*dt))/(1-exp(-(mu+gamma)*dt))/mu))
}

Plot_R0 <- function(beta, beta_npi, beta_vi, beta_npi_vi, real_parms, custom_beta=FALSE, dt=1){
  
  gamma <- -log(1-real_parms$gamma*dt)/dt
  
  R0_estim_tab <- rbind(
    
    data.frame(npi="0", vi="0", R0_estim(beta, gamma, real_parms$mu, dt)),
    data.frame(npi="1", vi="0", R0_estim(beta_npi, gamma, real_parms$mu, dt)),
    data.frame(npi="0", vi="1", R0_estim(beta_vi, gamma, real_parms$mu, dt)),
    data.frame(npi="1", vi="1", R0_estim(beta_npi_vi, gamma, real_parms$mu, dt))
  ) %>%
    mutate(vi=factor(vi, levels=c('1','0'), labels=c('estimated','0 (fixed)')))
  
  if(custom_beta){
    seasonal_beta <- real_parms$beta
  }else{
    seasonal_beta <- real_parms$b0*(1+real_parms$a1*cos(4*pi*((1:52)/52-real_parms$d1))+
                                      real_parms$a2*cos(2*pi*((1:52)/52-(real_parms$d1+real_parms$d2))))
  }
  
  R0_estim_tab %>%
    mutate(npi=factor(npi, labels=c("Pre-pandemic data", "Including\n(post-)pandemic data"))) %>%
    ggplot(aes(x=week)) +
    facet_grid(~npi) +
    geom_ribbon(aes(ymin=CI95_lower, ymax=CI95_upper, col=vi, fill=vi), lwd=0.15, alpha=0.25) +
    geom_line(aes(y=median, col=vi)) +
    labs(y="Basic repro-\nduction number", col=expression(phi), fill=expression(phi)) +
    geom_line(data=data.frame("week"=1:52,
                              "R0_real"=seasonal_beta/(real_parms$gamma+real_parms$mu)), aes(y=R0_real), lty='dashed', lwd=0.2) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_color_manual(values=c("#800080","#FFA500")) +
    scale_fill_manual(values=c("#800080", "#FFA500")) +
    theme_test() +
    theme(axis.title.y=element_text(size=10),
          legend.text=element_text(size=11), legend.title=element_text(size=17))
}

Plot_phi <- function(p_vi, p_npi_vi, real_max_vi){
  
  n_prepand <- ncol(p_vi$cases)
  
  phi_distrib <- rbind(
    data.frame(npi='0', phi=p_vi$phi),
    data.frame(npi='1', phi=p_npi_vi$phi)
  ) %>%
    mutate(npi=factor(npi, labels=c('Pre-pandemic data', 'Including (post-)\npandemic data')))
  
  return(
    phi_distrib %>%
      ggplot(aes(x=npi)) +
      geom_hline(yintercept=0, linewidth=0.6, col="grey70") +
      geom_hline(yintercept=real_max_vi, linewidth=0.3, lty='dashed') +
      labs(y="Max. change in force of infection\ndue to viral interaction") +
      geom_violin(aes(y=phi), fill="#800080", trim=FALSE, col=NA, alpha=0.4) +
      geom_boxplot(aes(y=phi),outliers=FALSE, linewidth=0.1, width=0.1) +
      scale_y_continuous(labels=percent_format(), limits=c(-0.5,0.5), breaks=seq(-0.6,0.6,0.15)) +
      theme_classic() +
      theme(axis.title.x=element_blank(), axis.title.y=element_text(size=10))
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
  data_id <- c("All", "Pre-pandemic", "(Post-)pandemic")
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

Plot_cor <- function(data, p, p_npi, p_vi, p_npi_vi, colors=c("#FFA500", "#800080"), log_scale=TRUE){
  
  i <- ncol(p$cases) + 1 # first_index_pandemic
  col_labels <- c('0 (fixed)', 'estimated')
  
  tab_cor <- rbind(
    # Endemic attractor
    data.frame(npi="0", vi="0", summary_cor(p, data$cases_noisy, log_scale=log_scale)),
    data.frame(npi="1", vi="0", summary_cor(p_npi, data$cases_noisy, first_index_pandemic=i, log_scale=log_scale)),
    data.frame(npi="0", vi="1", summary_cor(p_vi, data$cases_noisy, log_scale=log_scale)),
    data.frame(npi="1", vi="1", summary_cor(p_npi_vi, data$cases_noisy, first_index_pandemic=i, log_scale=log_scale))
  )
  
  tab_cor$period <- NA
  tab_cor$period[tab_cor$data=="All" & tab_cor$npi=="0"] <- "Pre-pandemic"
  tab_cor$period[tab_cor$data=="All" & tab_cor$npi=="1"] <- "Whole dataset\n(years 1-10)"
  tab_cor$period[tab_cor$data=="Pre-pandemic"] <- "Whole dataset\n(years 1-6)"
  tab_cor$period[tab_cor$data=="(Post-)pandemic"] <- "Whole dataset\n(years 7-10)"
  
  tab_cor$period <- factor(tab_cor$period, levels=c("Whole dataset\n(years 7-10)", "Whole dataset\n(years 1-6)",
                                                    "Whole dataset\n(years 1-10)", "Pre-pandemic"))
  pd <- position_dodge(width=0.5)
  
  return(
    tab_cor %>%
      ggplot(aes(y=period)) +
      labs(x="Pearson correlation (log scale)", col=expression(phi), fill=expression(phi)) +
      geom_segment(aes(x=CI95_lower, xend=CI95_upper, col=vi), position=pd) +
      geom_point(aes(x=mean, fill=vi), pch=21, cex=1.75, position=pd) +
      scale_color_manual(values=colors, labels=col_labels) +
      scale_fill_manual(values=colors, labels=col_labels) +
      theme_bw() +
      theme(axis.title.y=element_blank(), axis.text.x=element_text(size=9),
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

Plot_fit <- function(data, p, p_npi, p_vi, p_npi_vi, shift=0, colors=c("#FFA500", "#800080")){ 
  
  col_labels <- c('0 (fixed)', 'estimated')
  
  npi <- data.frame(npi="1", tmin=min(data$time[data$c!=0]/52+shift),
                    tmax=max(data$time[data$c!=0]/52+shift))
  
  return(
    rbind(
      data.frame(npi="0", vi="0", summary_fitted_values(data$cases_noisy, p$cases, shift=shift)),
      data.frame(npi="1", vi="0", summary_fitted_values(data$cases_noisy, p_npi$cases, shift=shift)),
      data.frame(npi="0", vi="1", summary_fitted_values(data$cases_noisy, p_vi$cases, shift=shift)),
      data.frame(npi="1", vi="1", summary_fitted_values(data$cases_noisy, p_npi_vi$cases, shift=shift))) %>%
      
      ggplot() +
      facet_grid(~npi, scales='free', labeller=labeller(npi=c("0"="Pre-pandemic data",
                                                              "1"="Including (post-)pandemic data"))) +
      labs(x="Time (years)", y="RV detections", col=expression(phi), fill=expression(phi)) +
      geom_vline(xintercept=ncol(p$cases)/52+shift, linewidth=0.4) +
      geom_rect(data=npi, aes(xmin=tmin, xmax=tmax, ymin=-Inf, ymax=+Inf), fill='grey85') +
      geom_point(aes(x=time, y=obs), cex=0.4, stroke=0.01, pch=21) +
      geom_ribbon(aes(x=time, ymin=lower, ymax=upper, fill=vi), alpha=0.25) +
      geom_line(aes(x=time, y=median, col=vi), linewidth=0.2) +
      scale_x_continuous(expand=c(0.001,0), breaks=0:9) +
      #scale_y_continuous(expand=c(0,0), breaks=1:10) +
      scale_color_manual(values=colors, labels=col_labels) +
      scale_fill_manual(values=colors, labels=col_labels) +
      theme_test() +
      theme(axis.title.x=element_text(size=10),
            axis.title.y=element_text(size=11), axis.text.y=element_text(size=8))
  )
}
