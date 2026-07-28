rm(list=ls())

library(tidyverse)
library(plyr)
library(rstan)
library(lubridate)
library(cowplot)
library(bayesplot)

setwd(".../Data_and_Codes")

fit_us <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_us.RDS")
fit_ca <- readRDS("Fit/Output/fit_RV_SIRS_npi_vi_ca.RDS")

parms <- c('logbeta0', 'eps_beta[1]', 'eps_beta[2]', 'eps_beta[3]', 'sigma_beta', 'kappa',
           'phi', 'omega', 'rho', 'S0', 'I0', 'sigma')

p_us <- extract(fit_us, permuted=FALSE)
p_ca <- extract(fit_ca, permuted=FALSE)

p_us <- p_us[,,dimnames(p_us)$parameters %in% parms]
p_ca <- p_ca[,,dimnames(p_ca)$parameters %in% parms]

plot_posterior <- function(p, warmup=2000,
                           colors=c('#FCA50AFF', '#DD513AFF', '#932667FF', '#420A68FF')){
  
  df <- p %>% melt(varnames=c("iteration", "chain", "parameter"),
                   value.name="value") %>%
    mutate(chain=as.factor(gsub('chain:', '', chain)))
  
  mytheme <- theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.y=element_text(size=5),
          legend.position='none',
          strip.background = element_blank())
  
  return(
    plot_grid(
      
      df %>%
        ggplot(aes(x=iteration+warmup, y=value, col=chain)) +
        facet_wrap(~parameter, ncol=1, scales='free',
                   labeller = as_labeller(c(
                     "logbeta0"    = "ln(beta[0])",
                     "eps_beta[1]" = "z[1]",
                     "eps_beta[2]" = "z[2]",
                     "eps_beta[3]" = "z[3]",
                     "sigma_beta"  = "sigma[beta]",
                     "kappa"       = "kappa",
                     "phi"         = "phi",
                     "omega"       = "omega",
                     "S0"          = "S[0]",
                     "I0"          = "I[0]",
                     "rho"         = "rho",
                     "sigma"       = "sigma"), label_parsed)) +
        geom_line(lwd=0.2, alpha=0.6) +
        labs(x='iteration') +
        scale_x_continuous(expand=c(0,0), breaks=seq(2500,3500,500)) +
        scale_y_continuous(expand=c(0,0), labels = scales::scientific) +
        scale_color_manual(values=colors) +
        mytheme +
        theme(axis.title.x=element_text(size=7), axis.text.x=element_text(size=7),
              strip.text = element_text(size=11)),
      
      df %>%
        ggplot(aes(y=value, col=chain, fill=chain)) +
        facet_wrap(~parameter, ncol=1, scales='free') +
        geom_density(lwd=0.2, alpha=0.1) +
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0), labels = scales::scientific) +
        scale_color_manual(values=colors) +
        scale_fill_manual(values=colors) +
        mytheme +
        theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              axis.title.x=element_blank(), axis.line.x=element_blank(),
              strip.text=element_blank()),
      
      rel_widths=c(0.75, 0.25), align='hv')
    )
}

plot_grid(
  ggdraw() + draw_label("A) US (national)", size=15, x=0.05, hjust=0, fontface='bold'),
  ggdraw() + draw_label("B) Canada (national)", size=15, x=0.05, hjust=0, fontface='bold'),
  plot_posterior(p_us),
  plot_posterior(p_ca),
  rel_heights=c(0.02,0.98), ncol=2)

ggsave('MCMC_posterior_US_CA.pdf', width=8, height=11)
