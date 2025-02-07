source('em_algorithm.R')
source('mixing_prop.R')

set.seed(123)

v1<-1
v2<-4

mu1<-0.5
mu2<-4

eps<-1e-5
N<-1000

v1_0<-1
mu1_0<--1
v2_0<-1
mu2_0<-3.5
tau_0<-0.5

times<-100

tau<-seq(0.15,0.85,length.out=3)

par<-vector("list", length(tau))

for (i in 1:length(tau)){
  x<-mixing_prop(N,mu1,v1,mu2,v2,tau[i])
  par[[i]]<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)
}


xx<-seq(-5,10,0.01)

y_real_1<-f(xx,mu1,v1,mu2,v2,tau[1])
y_comp_1<-f(xx,par[[1]]$mu1,par[[1]]$v1,par[[1]]$mu2,par[[1]]$v2,par[[1]]$tau)

df_1<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g1<-ggplot(df_1)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.4)+
  scale_color_manual(values=c('Real'='#EEC0A1','Computed'='#D96860'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = "none"
  )+
  annotate("text", x = 7, y = 0.38, label = bquote(tau == .(tau[1])), size = 3)


y_real_2<-f(xx,mu1,v1,mu2,v2,tau[2])
y_comp_2<-f(xx,par[[2]]$mu1,par[[2]]$v1,par[[2]]$mu2,par[[2]]$v2,par[[2]]$tau)

df_2<-data.frame(x=xx,y=y_real_2,y2=y_comp_2)

g2<-ggplot(df_2)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.4)+
  scale_color_manual(values=c('Real'='#EEC0A1','Computed'='#D96860'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    legend.position = 'none'
  )+
  annotate("text", x = -1, y = 0.38, label = bquote(tau == .(tau[2])), size = 3)

y_real_3<-f(xx,mu1,v1,mu2,v2,tau[3])
y_comp_3<-f(xx,par[[3]]$mu1,par[[3]]$v1,par[[3]]$mu2,par[[3]]$v2,par[[3]]$tau)

df_3<-data.frame(x=xx,y=y_real_3,y2=y_comp_3)

g3<-ggplot(df_3)+
  ylim(0,0.4)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_color_manual(values=c('Real'='#EEC0A1','Computed'='#D96860'),
                     guide = guide_legend(title = NULL))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    legend.position = c(-0.1,0.8),
  )+
  annotate("text", x = 6, y = 0.38, label = bquote(tau == .(tau[3])), size = 3)

tau<-seq(0.1,0.8,length.out=10)

count<-rep(0,length(tau))

for(i in 1:length(count)){
  for(j in 1:times){
    x<-mixing_prop(N,mu1,v1,mu2,v2,tau[i])
    par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)
    count[i]<-count[i]+par$count
  }
}

count<-count/times

df_count<-data.frame(x=tau,y=count)

p<-ggplot(df_count, aes(x = x, y = y)) +
  geom_line(linewidth=1,color='#EEC0A1') +
  geom_point(size=2.5,color='#D96860') +
  labs(title = NULL,
       x = expression(tau),
       y = '# of iterations') +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ))

grid.arrange(g1,g2,g3,p,ncol=4,widths = c(0.22,0.22,0.22,0.34))
