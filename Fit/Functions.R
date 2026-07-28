library(ggridges)

# Summary statistics

compute_median_95CI <- function(x){
  if(data.class(x)=='numeric'){
    return(quantile(x, probs=c(0.5, 0.025, 0.975)) %>%
             setNames(c('median', 'CI_lower', 'CI_upper')))
  }else{
    return(apply(x, 2, function(col){compute_median_95CI(col)}) %>% t %>% as.data.frame)
  }
}

summary_beta <- function(p){
  return(p$beta %>% compute_median_95CI %>% cbind("Week"=1:52,.))
}

summary_fit <- function(p, data){
  return(p$cases %>% compute_median_95CI %>%
           cbind(data.frame("date"=data$date, "obs"=data$RV_scaled_cases),.))
}

summary_SI <- function(p, data){
  
  N <- p$S[1,1]+p$I[1,1]+p$R[1,1]
  dates <- c(data$date[1]-7, data$date)
  
  S <- compute_median_95CI(p$S)/N
  I <- compute_median_95CI(p$I)/N
  
  return(
    rbind(data.frame("date"=dates, "var"="S", S),
          data.frame("date"=dates, "var"="I", I))
  )
}

summary_loglik <- function(posterior, i0_npi=NA){
  
  if(is.na(i0_npi)){
    return(posterior$log_lik %>% apply(1,sum) %>% compute_median_95CI)
    
  }else{
    
    loglik_prepand <- posterior$log_lik[,1:(i0_npi-1)] %>% apply(1,sum)
    loglik_postpand <- posterior$log_lik[,i0_npi:ncol(posterior$log_lik)] %>% apply(1,sum)
    loglik <- loglik_prepand + loglik_postpand
    
    return(
      rbind(compute_median_95CI(loglik_prepand),
            compute_median_95CI(loglik_postpand),
            compute_median_95CI(loglik)) %>% data.frame(period=c('Prepandemic', '(Post)pandemic', 'All'),.)
    )
  }
}

Summary_Re <- function(p, data, vi=FALSE, dt=1, mu=1/80/52, gamma=2){
  
  Re <- p$beta[,ifelse(data$week==53, yes=1, no=data$week)]* # beta (seasonal forcing)
    sapply(data$c, function(i){return(1+p$kappa*i)})* # NPI
    p$S[,-ncol(p$S)]/(p$S[1,1]+p$I[1,1]+p$R[1,1])* # S/N
    (1-exp(-mu*dt))/(1-exp(-(gamma+mu)*dt))/mu
  
  if(vi){
    iav <- smooth_cases(data)
    Re <- Re*sapply(iav/max(iav), function(i){return(1+p$phi*i)})
  }
  
  return(Re %>% compute_median_95CI %>% cbind('date'=data$date, 'c'=data$c,.))
}

# Plot

## Log-likelihood

Plot_loglik <- function(loglik, col="#29abd9"){
  return(
    loglik %>%
      mutate(vi=factor(vi, levels=c("phi=0", paste("lag",c(0,1,3)))),
             period=factor(period, levels=c("All", "Prepandemic", "(Post)pandemic"))) %>%
      ggplot(aes(x=vi)) +
      facet_wrap(~period, scales='free_y', ncol=1) +
      labs(y='Log-likelihood\n') +
      geom_segment(aes(y=CI_lower, yend=CI_upper), col=col) +
      geom_point(aes(y=median), col=col, cex=1.5) +
      scale_x_discrete(labels = c(expression(phi==0), expression(hat(phi)~'(lag 0)'),
                                  expression(hat(phi)~'(lag 1)'), expression(hat(phi)~'(lag 3)'))) +
      theme_bw() +
      theme(axis.title.x=element_blank())
  )
}

## Parameter posterior density distribution

plot_distribution <- function(p_list, parm, xlab, colors=NULL, scale=2, duration=FALSE){
  
  locations <- names(p_list)
  
  df <- p_list %>%
    lapply(function(p){return(p[which(names(p)==parm)])}) %>%
    imap_dfr(~data.frame(location=.y, value=.x)) %>%
    setNames(c("location", "value")) %>%
    mutate(location=factor(location, levels=rev(locations)))
  
  if(duration){df$value <- df$value %>% rate_to_duration}
  
  Plot <- ggplot(df, aes(x=value, y=location, fill=location)) +
    labs(x=xlab) +
    ggridges::geom_density_ridges(scale=scale, alpha=0.75, linewidth=0.15, rel_min_height=1e-3,
                                  quantiles=c(0.025,0.5,0.975), quantile_lines=TRUE) +
    theme_minimal()
  
  if(length(colors)!=0){Plot <- Plot + scale_fill_manual(values=rev(colors))}
  
  return(Plot)
}

## Transmission rate

Plot_beta <- function(beta, ylim, ncol, dt=1, mu=1/80/52, gamma=2, colors=NULL){
  
  Fig <- beta %>%
    ggplot(aes(x=Week)) +
    facet_wrap(~location, ncol=ncol) +
    labs(y=expression(paste(hat(beta), ", seasonal transmission rate (",week^{-1},")"))) +
    geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=location), col=NA, alpha=0.25) +
    geom_line(aes(y=median, col=location)) +
    scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
    scale_y_continuous(expand=c(0,0), limits=ylim,
                       sec.axis=sec_axis(
                         transform=~.*(1-exp(-mu*dt))/(1-exp(-(gamma+mu)*dt))/mu,
                         name=expression(paste(hat(R)[0], ", basic reproduction number")))) +
    theme_test() +
    theme(axis.title.y.left=element_text(margin=margin(r=10, unit="pt")),
          axis.title.y.right=element_text(margin=margin(l=10, unit="pt")))
  
  if(length(colors)==0){
    return(Fig)
  }else{
    return(Fig + scale_color_manual(values=colors) +
                 scale_fill_manual(values=colors))
  }
}

## Fitted values for S, I and cases (+ obs)

plot_SI_fit <- function(SI, fit, location, S_lim=c(0.2,1), S_breaks=seq(0.25,1,0.25),
                        I_lim=c(0,0.25), I_breaks=seq(0,0.2,0.1)){
  
  npi_period <- geom_rect(aes(xmin=ymd('2020-02-15'), xmax=ymd('2022-10-15'),
                              ymin=-Inf, ymax=+Inf), fill='grey85')
  
  vlines <- geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")), lty='dotted', col="grey40", lwd=0.3)
  
  my_theme <- theme_test() + theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_text(size=10),
                                   axis.text.y=element_text(size=8))
  return(
    plot_grid(
      ggplot(subset(SI, var=='S'), aes(x=date)) +
        labs(title=location, y='S/N') +
        npi_period + vlines +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill='#2EACFF', alpha=0.5) +
        geom_line(aes(y=median), lwd=0.3, col='blue') +
        scale_x_date(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0), limits=S_lim, breaks=S_breaks) +
        my_theme +
        theme(plot.title=element_text(size=10, face='bold'),
              axis.text.x=element_blank(), axis.ticks.x=element_blank()),
      
      ggplot(subset(SI, var=='I'), aes(x=date)) +
        labs(y='I/N') +
        npi_period + vlines +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill='#2EACFF', alpha=0.5) +
        geom_line(aes(y=median), lwd=0.3, col='blue') +
        scale_x_date(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0), limits=I_lim, breaks=I_breaks) +
        my_theme +
        theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
              axis.text.x=element_blank(), axis.ticks.x=element_blank()),
      
      ggplot(fit, aes(x=date)) +
        labs(y='Detections') +
        npi_period + vlines +
        geom_point(aes(y=obs), pch=21, stroke=0.005, size=1, alpha=0.8) +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper), fill='#2EACFF', alpha=0.5) +
        geom_line(aes(y=median), lwd=0.3, col='blue') +
        scale_x_date(expand=c(0,0), breaks=ymd(paste0(seq(2014,2024,2), '-01-01')), date_labels="%Y") +
        scale_y_continuous(expand=c(0,0)) +
        my_theme +
        theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"), axis.text.x=element_text(size=10)),
      
      ncol=1, align='v', rel_heights=c(0.35,0.25,0.4)))
}

plot_SI_fit2 <- function(SI, fit, Re, location,
                         S_lim=c(0.45,1), S_breaks=seq(0.5,1,0.1),
                         I_lim=c(0,0.08), I_breaks=seq(0,0.08,0.02),
                         col_vi=c('#FFA500', '#800080'), legend=TRUE){
  
  npi_period <- geom_rect(aes(xmin=ymd('2020-02-15'), xmax=ymd('2022-10-15'),
                              ymin=-Inf, ymax=+Inf), fill='grey85')
  
  vlines <- geom_vline(xintercept=ymd(paste0(2014:2025, "-01-01")), lty='dotted', col="grey40", lwd=0.3)
  
  label_col <- c('0 (fixed)', 'estimated')
  SI$vi <- factor(SI$vi, levels=c('0','1'), labels=label_col)
  fit$vi <- factor(fit$vi, levels=c('0','1'), labels=label_col)
  
  my_theme <- theme_test() + theme(plot.margin=unit(c(0.1,0.2,0.15,0.1),"cm"),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_text(size=9),
                                   axis.text.y=element_text(size=8))
  return(
    plot_grid(
      ggplot(subset(SI, var=='S'), aes(x=date)) +
        labs(title=location, y='Proportion susceptible', col=expression(phi), fill=expression(phi)) +
        npi_period + vlines +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=vi), col=NA, alpha=0.25) +
        geom_line(aes(y=median, col=vi), lwd=0.3) +
        scale_x_date(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0), limits=S_lim, breaks=S_breaks, labels=percent_format()) +
        scale_color_manual(values=col_vi, labels=label_col) +
        scale_fill_manual(values=col_vi, labels=label_col) +
        my_theme +
        theme(plot.title=element_text(size=15), legend.position=if(legend){c(0.05,0.9)}else{"none"},
              legend.title=element_text(size=11), legend.text=element_text(size=7),
              legend.direction='horizontal', legend.justification=c(0,1),
              axis.text.x=element_blank(), axis.ticks.x=element_blank()),
      
      ggplot(subset(SI, var=='I'), aes(x=date)) +
        labs(y='Proportion infected', col=expression(phi), fill=expression(phi)) +
        npi_period + vlines +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=vi), col=NA, alpha=0.25) +
        geom_line(aes(y=median, col=vi), lwd=0.3) +
        scale_x_date(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0), limits=I_lim, breaks=I_breaks, labels=percent_format()) +
        scale_color_manual(values=col_vi, labels=label_col) +
        scale_fill_manual(values=col_vi, labels=label_col) +
        my_theme +
        theme(legend.position='none', axis.text.x=element_blank(), axis.ticks.x=element_blank()),
      
      ggplot(fit, aes(x=date)) +
        labs(y='Detections', col=expression(phi), fill=expression(phi)) +
        npi_period + vlines +
        geom_point(aes(y=obs), pch=21, stroke=0.25, size=1, alpha=0.8) +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=vi), col=NA, alpha=0.2) +
        geom_line(aes(y=median, col=vi), lwd=0.3) +
        scale_x_date(expand=c(0,0), breaks=ymd(paste0(seq(2014,2024,2), '-01-01')), date_labels="%Y") +
        scale_y_continuous(expand=c(0,0), limits=c(0,max(fit$obs))) +
        scale_color_manual(values=col_vi, labels=label_col) +
        scale_fill_manual(values=col_vi, labels=label_col) +
        my_theme +
        theme(legend.position='none', axis.text.x=element_blank(), axis.ticks.x=element_blank()),
      
      ggplot(Re, aes(x=date)) +
        labs(y="Effective reproduction number",
             col="mean change\nin mobility", fill="mean change\nin mobility") +
        geom_rect(aes(xmin=date-1, xmax=date, ymin=-Inf, ymax=+Inf, fill=c, col=c)) +
        vlines +
        geom_hline(yintercept=1, lwd=0.4) +
        scale_x_date(expand=c(0,0), breaks=ymd(paste0(seq(2014,2024,2),'-01-01')), date_labels="%Y") +
        scale_y_continuous(expand=c(0,0), limits=c(0.6,1.9), breaks=seq(0.75,1.75,0.25)) +
        scale_fill_gradient2(low="#2C792D", mid="white", high="#90529C", midpoint=0, na.value=NA,
                             label=scales::percent, limits=c(-0.52, 0.06), breaks=seq(-0.5,0,0.25)) +
        scale_color_gradient2(low="#2C792D", mid="white", high="#90529C", midpoint=0, na.value=NA,
                              limits=c(-0.52, 0.06), label=scales::percent, guide='none') +
        new_scale_fill() +
        new_scale_color() +
        geom_ribbon(aes(ymin=CI_lower, ymax=CI_upper, fill=vi), col=NA, alpha=0.25) +
        geom_line(aes(y=median, col=vi), lwd=0.15) +
        scale_color_manual(values=col_vi, labels=label_col, guide = "none") +
        scale_fill_manual(values=col_vi, labels=label_col, guide = "none") +
        my_theme +
        theme(legend.position=if(legend){c(0.03,0.95)}else{"none"}, legend.direction='horizontal',
              axis.text.x=element_text(size=9), legend.justification=c(0,1), 
              legend.title=element_text(size=8), legend.text=element_text(size=7),
              legend.key.width=unit(0.22,"in")),
      
      ncol=1, align='v')
    )
}

# Miscellaneous

rate_to_duration <- function(x, dt=1){
  return(dt/(1-exp(-x*dt)))
}

smooth_cases <- function(X, eps=1E-9){
  smooth <- c()
  n <- nrow(X)
  for(i in 1:n){
    smooth[i] <- (subset(X, date>=X$date[i]-7 & date<=X$date[i]+7)$IAV_scaled_cases+eps) %>% log %>% mean %>% exp
  }
  return(smooth)
}
