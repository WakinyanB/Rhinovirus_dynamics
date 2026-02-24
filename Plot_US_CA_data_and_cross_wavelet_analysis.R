rm(list=ls())

library(tidyverse)
library(lubridate)
library(biwavelet)
library(cowplot)
library(ggpubr)
library(grid)
library(scico)
library(viridis)

setwd("C:/Users/wb9928/OneDrive - Princeton University/Desktop/RV/Data_and_Codes/")

# Data

load("Data/USA/Data_USA.RData") # USA
load("Data/Canada/data_canada_province.RData") # Canada

# Cross-wavelet analyses

wavelet_analysis <- function(data, sqrt_normalization=TRUE){
  
  time <- 1:nrow(data)
  rv <- data$RV_scaled_cases
  iav <- data$IAV_scaled_cases
  
  if(sqrt_normalization){
    
    rv <- sqrt(rv)
    rv <- (rv-mean(rv))/sd(rv)
    
    iav <- sqrt(iav)
    iav <- (iav-mean(iav))/sd(iav)
  }
  return(xwt(cbind(time, iav), cbind(time, rv)))
}

plot_wavelet <- function(res, date, title, zlim, palette=NA){
  
  years <- date %>% format("%Y") %>% unique
  tick_positions <- years[-1] %>% sapply(function(y){
    jan1 <- y %>% paste0("01-01") %>% ymd
    return((date-jan1) %>% abs %>% which.min)
  })
  if(any(is.na(palette))){
    plot(res, xaxt="n", xlab="", ylab="Period (weeks)", zlim=zlim,
         plot.coi=TRUE, plot.phase=TRUE)
  }else{
    plot(res, xaxt="n", xlab="", ylab="Period (weeks)", zlim=zlim,
         plot.coi=TRUE, plot.phase=TRUE, fill.cols=palette)
  }
  axis(1, at=tick_positions, labels=years[-1])
  mtext(title, side=3, line=0, at=0, adj=0)
}

res_us <- wavelet_analysis(data_us)
res1 <- wavelet_analysis(data_hhs1)
res2 <- wavelet_analysis(data_hhs2)
res3 <- wavelet_analysis(data_hhs3)
res4 <- wavelet_analysis(data_hhs4)
res5 <- wavelet_analysis(data_hhs5)
res6 <- wavelet_analysis(data_hhs6)
res7 <- wavelet_analysis(data_hhs7)
res8 <- wavelet_analysis(data_hhs8)
res9 <- wavelet_analysis(data_hhs9)
res10 <- wavelet_analysis(data_hhs10)

res_ca <- wavelet_analysis(data_ca)
res_at <- wavelet_analysis(data_at)
res_bc <- wavelet_analysis(data_bc)
res_on <- wavelet_analysis(data_on)
res_pr <- wavelet_analysis(data_pr)

max_zval <- list(res_us, res1, res2, res3, res4, res5,
                 res6, res7, res8, res9, res10,
                 res_ca, res_at, res_bc, res_on, res_pr) %>%
  lapply(function(i){
    max(log2(i$power.corr)/(i$d1.sigma*i$d2.sigma))
  })

zlim <- range(c(-1,1)*max(unlist(max_zval)))

#pdf("Wavelet_plots_US.pdf", width=12, height=10)
#png("Wavelet_plots_US2.png", width=12, height=10, units='in', res=1000)

par(mfrow=c(4,3), mar=c(2,4,2,1), oma=c(2,2,2,2))

plot_wavelet(res_us, date=data_us$date, title="US - National", zlim=zlim)
plot_wavelet(res1, date=data_hhs1$date, title="HHS 1", zlim=zlim)
plot_wavelet(res2, date=data_hhs2$date, title="HHS 2", zlim=zlim)
plot_wavelet(res3, date=data_hhs3$date, title="HHS 3", zlim=zlim)
plot_wavelet(res4, date=data_hhs4$date, title="HHS 4", zlim=zlim)
plot_wavelet(res5, date=data_hhs5$date, title="HHS 5", zlim=zlim)
plot_wavelet(res6, date=data_hhs6$date, title="HHS 6", zlim=zlim)
plot_wavelet(res7, date=data_hhs7$date, title="HHS 7", zlim=zlim)
plot_wavelet(res8, date=data_hhs8$date, title="HHS 8", zlim=zlim)
plot_wavelet(res9, date=data_hhs9$date, title="HHS 9", zlim=zlim)
plot_wavelet(res10, date=data_hhs10$date, title="HHS 10", zlim=zlim)

par(mfrow=c(1,1))

# dev.off()

# pdf("Wavelet_plots_Canada.pdf", width=9, height=7)
# png("Wavelet_plots_Canada.png", width=9, height=7, units='in', res=1000)

par(mfrow=c(3,2), mar=c(2,4,2,1), oma=c(2,2,2,2))

plot_wavelet(res_ca, date=data_ca$date, title="Canada - National", zlim=zlim)
plot_wavelet(res_at, date=data_at$date, title="Altantic", zlim=zlim)
plot_wavelet(res_bc, date=data_bc$date, title="British Columbia", zlim=zlim)
plot_wavelet(res_on, date=data_on$date, title="Ontario", zlim=zlim)
plot_wavelet(res_pr, date=data_pr$date, title="Prairies", zlim=zlim)

par(mfrow=c(1,1))

# dev.off()

plot_data <- function(data, title=NA, scale_factor=0.25,
                      breaks_RV=waiver(), breaks_IAV=waiver(),
                      points=TRUE, lwd=0.6, point_size=1, col_RV='#0D4ABA', col_IAV='#EE6251',
                      c_lim=c(-0.5,0), vline_lwd=0.4){
  
  years <- ymd(paste0(2014:2025, "-01-01"))
  
  Fig <- data %>%
    mutate(c, c=ifelse(c==0, yes=NA, no=c)) %>%
    ggplot() +
    geom_rect(aes(xmin=date-6, xmax=date, ymin=-Inf, ymax=+Inf, fill=c, col=c)) +
    geom_vline(xintercept=years, lty='dotted', linewidth=vline_lwd)
  
  if(points){
    Fig <- Fig +
      geom_line(aes(x=date, y=RV_scaled_cases), col=col_RV, linewidth=lwd) +
      geom_point(aes(x=date, y=RV_scaled_cases), col=col_RV, cex=point_size) +
      geom_line(aes(x=date, y=IAV_scaled_cases*scale_factor), col=col_IAV, linewidth=lwd) +
      geom_point(aes(x=date, y=IAV_scaled_cases*scale_factor), col=col_IAV, cex=point_size)
  }else{
    Fig <- Fig +
      geom_line(aes(x=date, y=RV_scaled_cases), col=col_RV, linewidth=lwd) +
      geom_line(aes(x=date, y=IAV_scaled_cases*scale_factor), col=col_IAV, linewidth=lwd)
  }
  return(
    Fig +
      labs(title=title, y="Rescaled RV/EV detections", fill="Mean change  \nin mobility") +
      scale_x_date(expand=c(0,0), breaks=years, date_labels="%Y") +
      scale_y_continuous(expand=c(0.005,0), breaks=breaks_RV,
                         sec.axis=sec_axis(~./scale_factor,
                                           name="Rescaled IAV detections",
                                           breaks=breaks_IAV)) +
      scale_fill_gradient2(low="#2C792D", mid="white", high="#90529C", midpoint=0, limits=c_lim,
                            na.value=NA, label=scales::percent) +
      scale_color_gradient2(low="#2C792D", mid="white", high="#90529C", midpoint=0, limits=c_lim,
                            na.value=NA, label=scales::percent) +
      theme_classic() +
      theme(axis.title.x=element_blank(), axis.text.x=element_text(size=8),
            legend.title=element_text(size=10), legend.text=element_text(size=8), 
            axis.line.y.left=element_line(color=col_RV, linewidth=0.75),
            axis.title.y.left=element_text(color=col_RV, size=10),
            axis.text.y.left=element_text(color=col_RV, size=7, angle=40),
            axis.line.y.right=element_line(color=col_IAV, linewidth=0.75),
            axis.title.y.right=element_text(color=col_IAV, size=10),
            axis.text.y.right=element_text(color=col_IAV, size=7, angle=40)) +
      guides(col='none')
  )
}

# low="steelblue", mid="white", high="tomato"

c_lim <- c(-0.52,0.057)

(P <- ggarrange(
  plot_data(data_us, title="US", c_lim=c_lim, breaks_RV=seq(0,7000,1000), breaks_IAV=seq(0,25000,5000)) +
    guides(fill=guide_colorbar(barwidth=13, barheight=1, label.position="top")),
  plot_data(data_ca, title="Canada", c_lim=c_lim, breaks_RV=seq(0,2500,500), breaks_IAV=seq(0,8000,2000)),
  common.legend=TRUE, legend="top", labels=LETTERS[1:2]))

vp <- viewport(height=unit(.5, "npc"), width=unit(0.98, "npc"), just=c("left","top"), y=1, x=0.01)

#pdf("TS_wavelet_plots_US_CA_v2.pdf", width=11, height=7)
# png("TS_wavelet_plots_US_CA_v2.png", width=11, height=7, units='in', res=1000)

#par(mfrow=c(2,2), mar=c(3,4,1,4))

plot.new()
plot.new()
plot_wavelet(res_us, date=data_us$date, title=NA, zlim=zlim, palette=magma(11))
plot_wavelet(res_ca, date=data_ca$date, title=NA, zlim=zlim, palette=magma(11))
print(P, vp=vp)

#par(mfrow=c(1,1))

#dev.off()

c_lim <- range(c(data_ca$c, data_at$c, data_bc$c, data_on$c, data_pr$c))
b <- 'blue'
r <- 'red'
lwd <- 0.5
vlwd <- 0.3

plot_grid(
  
  plot_data(data_ca, title="Canada - National", lwd=lwd, scale_factor=0.25,
            points=FALSE, point_size=pt_size, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position="none", axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_at, title="Atlantic", lwd=lwd, scale_factor=0.75,
            points=FALSE, point_size=pt_size, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position="none", axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_bc, title="British Columbia", lwd=lwd, scale_factor=0.4,
            points=FALSE, point_size=pt_size, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position="none", axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_on, title="Ontario", lwd=lwd, scale_factor=1,
            points=FALSE, point_size=pt_size, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position="none", axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_pr, title="Prairies", lwd=lwd, scale_factor=0.25,
            points=FALSE, point_size=pt_size, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position="none", axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  get_legend(plot_data(data_ca, c_lim=c_lim) + theme(legend.position="top") +
               guides(fill=guide_colorbar(barwidth=13, barheight=1, title = "Mean change in mobility",
                                          title.position="top", label.position = "bottom"))),
  
  nrow=3, ncol=2
) %>% print # 9 x 7

c_lim <- range(c(data_us$c, data_hhs1$c, data_hhs2$c, data_hhs3$c, data_hhs4$c, data_hhs5$c,
                 data_hhs6$c, data_hhs7$c, data_hhs8$c, data_hhs9$c, data_hhs10$c))
lwd <- 0.4
b <- 'blue'
r <- 'red'
vlwd <- 0.3

plot_grid(
  
  plot_data(data_us, title="US - National", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs1, title="HHS 1", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs2, title="HHS 2", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs3, title="HHS 3", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs4, title="HHS 4", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs5, title="HHS 5", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs6, title="HHS 6", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs7, title="HHS 7", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs8, title="HHS 8", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs9, title="HHS 9", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.right=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  plot_data(data_hhs10, title="HHS 10", lwd=lwd, points=FALSE, col_RV=b, col_IAV=r, c_lim=c_lim, vline_lwd=vlwd) +
    theme(legend.position='none', axis.title.y.left=element_blank(),
          axis.text.x=element_text(size=8, angle=40, hjust=1)),
  
  get_legend(plot_data(data_us, c_lim=c_lim) + theme(legend.position="top") +
               guides(fill=guide_colorbar(barwidth=13, barheight=1, title = "Mean change in mobility",
                                          title.position="top", label.position = "bottom"))),
  
  nrow=4, ncol=3
) # 11 x 8
