source('em_algorithm.R')
source('mixing_prop.R')

tau<-0.5

v1<-1
v2<-4

eps<-1e-5
N<-1000

v1_0<-1
mu1_0<--1
v2_0<-1
mu2_0<-1
tau_0<-0.5

times<-100

mu1<--0.5
mu2<-seq(4,7,length.out=4)

par<-vector("list", length(mu2))

for (i in 1:length(mu2)){
  x<-mixing_prop(N,mu1,v1,mu2[i],v2,tau)
  par[[i]]<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)
}


xx<-seq(-5,10,0.01)

y_real_1<-f(xx,mu1,v1,mu2[1],v2,tau)
y_comp_1<-f(xx,par[[1]]$mu1,par[[1]]$v1,par[[1]]$mu2,par[[1]]$v2,par[[1]]$tau)

df_1<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g1<-ggplot(df_1)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.3)+
  scale_color_manual(values=c('Real'='#43B4C2','Computed'='#2D2D5E'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = "none"
  )+
  annotate("text", x = -1, y = 0.28, label = bquote(mu[2] == .(mu2[1])), size = 3)


y_real_2<-f(xx,mu1,v1,mu2[2],v2,tau)
y_comp_2<-f(xx,par[[2]]$mu1,par[[2]]$v1,par[[2]]$mu2,par[[2]]$v2,par[[2]]$tau)

df_2<-data.frame(x=xx,y=y_real_2,y2=y_comp_2)

g2<-ggplot(df_2)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.3)+
  scale_color_manual(values=c('Real'='#43B4C2','Computed'='#2D2D5E'))+
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
  annotate("text", x = -2.5, y = 0.28, label = bquote(mu[2] == .(mu2[2])), size = 3)

y_real_3<-f(xx,mu1,v1,mu2[3],v2,tau)
y_comp_3<-f(xx,par[[3]]$mu1,par[[3]]$v1,par[[3]]$mu2,par[[3]]$v2,par[[3]]$tau)

df_3<-data.frame(x=xx,y=y_real_3,y2=y_comp_3)

g3<-ggplot(df_3)+
  ylim(0,0.3)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_color_manual(values=c('Real'='#43B4C2','Computed'='#2D2D5E'),
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
  annotate("text", x = 8, y = 0.28, label = bquote(mu[2] == .(mu2[3])), size = 3)

y_real_4<-f(xx,mu1,v1,mu2[4],v2,tau)
y_comp_4<-f(xx,par[[4]]$mu1,par[[4]]$v1,par[[4]]$mu2,par[[4]]$v2,par[[4]]$tau)

df_4<-data.frame(x=xx,y=y_real_4,y2=y_comp_4)

g4<-ggplot(df_4)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.3)+
  scale_color_manual(values=c('Real'='#43B4C2','Computed'='#2D2D5E'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    legend.position ="none"
  )+
  annotate("text", x = -2.2, y = 0.28, label = bquote(mu[2] == .(mu2[4])), size = 3)  

mu2<-seq(4,7,length.out=6)

count<-rep(0,length(mu2))

for(i in 1:length(count)){
  for(j in 1:times){
    x<-mixing_prop(N,mu1,v1,mu2[i],v2,tau)
    par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)
    count[i]<-count[i]+par$count
  }
}

count<-count/times

df_count<-data.frame(x=mu2,y=count)

p<-ggplot(df_count, aes(x = x, y = y)) +
  geom_line(linewidth=1,color='#43B4C2') +
  geom_point(size=2.5,color='#2D2D5E') +
  labs(title = NULL,
       x = expression(mu[2]),
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

grid.arrange(g1,g2,g3,g4,p,ncol=5, widths = c(0.2,0.18,0.18,0.18,0.26))