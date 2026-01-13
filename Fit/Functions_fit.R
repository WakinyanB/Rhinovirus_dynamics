smooth_cases <- function(X, eps=1E-9){
  smooth <- c()
  n <- nrow(X)
  for(i in 1:n){
    smooth[i] <- (subset(X, date>=X$date[i]-7 & date<=X$date[i]+7)$IAV_scaled_cases+eps) %>% log %>% mean %>% exp
  }
  return(smooth)
}

data_stan <- function(data, mu, gamma, pop){
  return(
    list(
      n_week=nrow(data),
      week=ifelse(data$week==53, yes=1, no=data$week),
      obs=ifelse(data$RV_scaled_cases==0, 0.1, data$RV_scaled_cases),
      obs_IAV=data$IAV_scaled_cases_smooth/max(data$IAV_scaled_cases_smooth),
      gamma=gamma,
      # omega=omega,
      mu=mu,
      c=data$c,
      pop=pop
    )
  )
}

plot_fit <- function(posterior, data){
  return(
    data.frame("time"=data$date,
               "observed"=data$RV_scaled_cases,
               "fitted"=apply(posterior$cases,2,median),
               "lower"=apply(posterior$cases,2,quantile, probs=0.025),
               "upper"=apply(posterior$cases,2,quantile, probs=0.975)) %>%
      ggplot(aes(x=time)) +
      geom_point(aes(y=observed), pch=21, cex=1) +  # Observed cases
      geom_ribbon(aes(ymin=lower, ymax=upper), fill="red", alpha=0.5) +  # Credible interval
      geom_line(aes(y=fitted), color="red", linewidth=0.1) +  # Posterior median
      theme_minimal() +
      theme(axis.title.x=element_blank())
  )
}

plot_beta <- function(posterior, cos1=FALSE){

  return(
    posterior$beta %>% apply(2, function(X){
      return(data.frame("median"=median(X),
                        "CI95_lower"=quantile(X, probs=0.025),
                        "CI95_upper"=quantile(X, probs=0.975), row.names=NULL))
    }) %>% bind_rows %>% cbind("week"=1:52,.) %>%
      ggplot(aes(x=week)) +
      labs(y=expression(paste(beta, ", transmission rate (/week)"))) +
      geom_ribbon(aes(ymin=CI95_lower, ymax=CI95_upper), fill="red", col=NA, alpha=0.35) +
      geom_line(aes(y=median), col="red") +
      scale_x_continuous(expand=c(0,0), breaks=c(1,seq(10,50,10))) +
      theme_bw()
  )
}