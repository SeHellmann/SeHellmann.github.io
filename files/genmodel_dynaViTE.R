#library(ggforce)
library(tidyverse)
library(ggpubr)
library(gridExtra)

#### Illustration dWEV model #######


#parameters 
n = 400
a <- 2
z <- 1
nu <- 1.5
tau <- 0.07*n
t0 = 0.1*n
st0= 0.1*n
sv <- 0.2
sz = 0.4

## Simulate path
for (seed in 10000) {
  set.seed(seed)
  X = rnorm(n, nu/n, 1/sqrt(n))
  V = rnorm(n, nu/n/2, 1/sqrt(2*n))
  S = rep(0, n)
  Vis = S
  S[1] = z
  V[1] = 0
  for (i in 2:n){
    S[i]=S[i-1]+X[i]
    Vis[i] = Vis[i-1] + V[i]
  }
}
seed   ## seed = 10000 worked
t <- min(which(S>=a))-0.5

## Draw plot

mean_angle <- atan2(y=nu, x = 1)
M <- 100
R<- 0.1
distdriftrates <- data.frame(r=0.2, x = 1, y=z, nus = seq(nu-5*sv,nu+5*sv, length.out=M)) %>%
  mutate(xend=1+n*R*cos(atan2(nus, x=1)),  yend = z+R*sin(atan2(nus, x=1)),
         alpha = dnorm(nus, nu, sd=2*sv))




S <- a-S
S[t]<-0
path_data <- data.frame(S=S, X=X, T = 1:n)[1:(ceiling(t)+tau),]
t0 <- 20
st0 <- 8
p <- ggplot(data=path_data)+
  geom_line(aes(x=T, y=S), linewidth=1.2, color="red")+
  scale_y_continuous(name="Decision evidence", 
                     breaks=c(0, z, a), labels=c(0, "z", "a"), 
                     limits = c(-1, a+1), expand = expansion(mult = c(0, 0)))+
  scale_x_continuous(name="", 
                     breaks=c(t, (t+tau),(t+tau+t0)), labels = c(expression(T[dec]), expression(T[dec]+ tau), expression(T[dec]+ tau+t[0])), 
                     limits = c(-1, t+tau+t0+st0+4), expand = expansion(mult = c(0, 0)),
                     guide="none")+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  geom_vline(xintercept = c(t, t+tau), colour="gray", linewidth=0.75)+
  #geom_hline(yintercept = c(0, z, a), colour="gray", size=0.75)+
  # geom_segment(data=data.frame(y = c(0,z,a),x=-3, xend=t), aes(x=x, xend=xend, y=y, yend=y),
  #              colour="gray", linewidth=0.75)+
  geom_segment(data=data.frame(y = c(S[(ceiling(t)+tau)]),x=t+tau, xend=t+tau+1), aes(x=x, xend=xend, y=y, yend=y),
               colour="gray", linewidth=0.75)+
  annotate("text", size=9/.pt, family="Times", x=c(t/2), y=c(a+0.8),hjust=0.5, vjust=1,
           label="Pre-decisional accumulation")+
  annotate("text", size=9/.pt, family="Times", x=c(t+tau/2), y=c(a+0.8),hjust=0.5, vjust=1,
           label="Post-decisional\naccumulation")+
  annotate("text",  size=9/.pt, family="Times", x=c(t+tau+13), y=c(S[(ceiling(t)+tau)]),
           label=expression(X(T[dec]+tau)))+
  geom_segment(aes(x=1, y=z, xend=20, yend=(z-80/n*nu)), linewidth = 0.4, arrow =arrow(length = unit(0.2, "cm")) )+
  #annotate("text", size=9/.pt, family="Times",x=22, y=(z-88/n*nu), label=c(expression(nu)),hjust=0)+
  
  #geom_segment(aes(x=1, y=z, xend=80, yend=(z+80/n*nu)), size = 0.5, arrow =arrow(length = unit(0.2, "cm")) )+
  annotate("rect", xmin=-1, xmax=1, ymin=z-sz/2, ymax = z+sz/2, fill="black", color="black")+
  annotate("segment", y= c(0, a), yend= c(0,  a), x=-1, xend= t,
           linetype="dashed", linewidth=0.75)+
  geom_segment(data=distdriftrates, aes(x=x, y=y, xend=xend/4, yend=-yend+2*z),
               linewidth = 0.5, linetype=1, lineend = "round") +
  #scale_alpha_continuous(range=c(0,0.1))+
  annotate("text", size=9/.pt, family="Times",x=22, y=(z-80/n*nu), label=c(expression(mean ~ drift ~ rate ~ nu)),hjust=0)+
  annotate("text", size=9/.pt, family="Times",x=0, y=c(z+sz/1.5+0.35,z+sz/1.5+0.1),
           label=c("range of starting", "points sz"), vjust=0, hjust=0)+
  annotate("text", size=9/.pt, family="Times",x=1.2, y=c(z-sz/2.2,z-sz/2.2-0.25,z-sz/2.2-0.5), 
           label=c("drift", "rate",expression(paste("variation ","s",nu))), vjust=1, hjust=0)+
  annotate("text", size=9/.pt, family="Times",x=c(1,1), y=c(a+a/8, -a/8), label=c("Evidence for 1", "Evidence for -1"), hjust=0, vjust=0.5)+
  theme(legend.position = "none",
        plot.margin = margin(0, 0.01, 0, 0, "cm"),
        text = element_text(size=9, family="Times"),
        axis.text = element_text(size=9, family="Times", color="black"),
        axis.text.y = element_text( angle=90, hjust = 0.5))

exp_Visdriftvariation = list("'drift rate'", "variation" ~ sigma[V])
pVis <- ggplot(data=data.frame(Vis=Vis, X=X, T = 1:n)[1:(ceiling(t)+tau),])+
  geom_line(aes(x=T, y=Vis), size=1.2, color="red")+
  scale_y_continuous(name="Visibility", 
                     breaks=c(0), labels=c(0), limits = c(-0.6, 1.8),
                     expand = expansion(mult = c(0.0, 0.1)))+
  scale_x_continuous(name="Time", 
                     breaks=c(t, (t+tau),(t+tau+t0)), labels = c(expression(T[dec]), expression(T[dec]+ tau), expression(T[dec]+ tau+t[0])), 
                     limits = c(-1, t+tau+t0+st0+4), expand = expansion(mult = c(0, 0)))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  geom_vline(xintercept = c(t, t+tau), colour="gray", size=0.75)+
  #geom_hline(yintercept = c(0), colour="gray", size=0.7)+
  geom_segment(data=data.frame(y = c(0),x=-3, xend=t+tau), aes(x=x, xend=xend, y=y, yend=y),
               colour="gray", size=0.75)+
  geom_segment(data=data.frame(y = c(Vis[(ceiling(t)+tau)]),x=t+tau, xend=t+tau+1), aes(x=x, xend=xend, y=y, yend=y),
               colour="gray", size=0.75)+
  annotate("text",  size=9/.pt, family="Times",  x=c(t+tau+13), y=c(Vis[(ceiling(t)+tau)]),
           label=expression(V(T[dec]+tau)))+
  annotate("text", size=9/.pt, family="Times",
           x=c(9,9,24.5,26.5),
           y=c(0.7, 0.50, 0.50, 0.50), 
           label = c("mean visibility", "drift rate |", expression(nu), "|"),
           hjust=0)+
  geom_segment(aes(x=1, y=0, xend=20, yend=(80/n*nu)), size = 0.4, arrow =arrow(length = unit(0.2, "cm")) )+
  # geom_vline(xintercept = t, linetype="dashed", size=0.75)+
  # geom_hline(yintercept = c(0, z, a), linetype="dashed", size=0.75)+
  annotate("text", size=9/.pt, family="Times",x=max(distdriftrates$xend/4), y=c(-0.03,-.23), 
           label=exp_Visdriftvariation, parse=TRUE,  
           vjust=1, hjust=0)+
  geom_segment(data=distdriftrates, aes(x=x, y=0, xend=xend/4, yend=yend-y),  
               size = 0.5, linetype=1, lineend = "round") +
  #scale_alpha_continuous(range=c(0,0.1))+
  annotate("text", size=9/.pt, family="Times",x=54, y=(2*95/n*nu)+0.01,
           label=expression(paste("process variability ", s[V])),hjust=0.5, vjust=0)+
  annotate("segment", x = 56, xend = 56, y = (2*95/n*nu)-0.07, yend = (2*95/n*nu),
           arrow = arrow(ends = "both", angle = 90, length = unit(.06,"cm")))+
  
  annotate("rect", xmin=(t+tau+t0-st0/2), xmax=(t+tau+t0+st0/2), ymin=-0.6, ymax=-0.5, fill="black", color="black")+
  annotate("text", size=9/.pt, family="Times",x=c(t+tau+t0,t+tau+t0,t+tau+t0), y=c(0, -0.2, -0.4), 
           label=c("range of", "non-decision", expression(time ~ st[0])), hjust=0.5)+
  theme(legend.position = "none",
        plot.margin = margin(0, 0.01, 0, 0, "cm"),
        text = element_text(size=9, family="Times"),
        axis.text = element_text(size=9, family="Times", color="black"),
        axis.text.y = element_text( angle=90, hjust = 0.5))

plot_conf_data <- rbind(expand.grid(x = c(0,0.5), y=seq(-0.5-1.2*(z-0.1+0.5)/6, a+0.5+(z-0.1+0.5)/6, length.out = 5), dec="1"),
                        expand.grid(x = c(0,0.8), y=seq(-0.5-0.8*(z-0.1+0.5)/6, a+0.2+(z-0.1+0.5)/6, length.out = 5), dec="-1"))
plot_conf_data[c(3, 4),"y"] <- plot_conf_data[c(3, 4),"y"]+0.4 

pConf <- ggplot(data=plot_conf_data)+
  geom_line(aes(x=x, y=y, col=dec, group=y))+
  scale_y_continuous(name="", 
                     breaks=NULL, labels=NULL, 
                     limits = c(-1, a+1), expand = expansion(mult = c(0, 0)))+
  scale_x_continuous(name="", 
                     breaks=NULL, labels = NULL, 
                     limits = c(0, 3.2), expand = expansion(mult = c(0, 0)))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  scale_color_manual(breaks=c("1", "-1"), values=c("gray60", "black"))+
  annotate("text", size=9/.pt, family="Times", x=c(0.55), y=unique(plot_conf_data[plot_conf_data$dec=="1", "y"]),
           label=as.expression(parse(text=paste("theta[1",as.character(1:5),"]",sep=""))), vjust=0.5, hjust=0, col="gray60")+
  annotate("text", size=9/.pt, family="Times", x=c(0.85), y=unique(plot_conf_data[plot_conf_data$dec=="-1", "y"]),
           label=as.expression(parse(text=paste("theta[-1",as.character(1:5),"]",sep=""))), vjust=0.5, hjust=0, col="black")+
  annotate("text", size=8/.pt, family="Times", x=0.25, y=2.1, 
           label= expression(wR~frac(X(T[dec]+tau), (T[dec]+tau)^{lambda}) + (1-w)~frac(V(T[dec]+tau), (T[dec]+tau)^{lambda})), 
           #label= expression(paste("wRX(",T[dec]+tau,") + (1-w)V(",T[dec]+tau,")")), 
           hjust=0, vjust=0.5)+
  geom_segment(data=data.frame(y = c(2.1),x=0, xend=0.2), aes(x=x, xend=xend, y=y, yend=y),
               size=1.2, color="red")+
  annotate("text", size=9/.pt, family="Times", x=1.6, y=z, label="Confidence", angle=-90, hjust=0.5, vjust=0.5)+
  theme(legend.position = "none",
        plot.margin = margin(0.5, 0, 0.8, 0.3, "cm"),
        text=element_text(size=9, family="Times"),
        axis.text = element_text(size=9, family="Times", color="black"))
pdWEV <- ggarrange(ggarrange(p, pVis, ncol=1, align="v"), pConf, 
                   widths=c(0.7, 0.3))
pdWEV
