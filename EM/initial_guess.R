source('em_algorithm.R')
source('mixing_prop.R')

set.seed(20)

v1<-1
v2<-4

mu1<--0.5
mu2<-4

tau<-0.5

eps<-1e-5
N<-1000

times<-100

k=c(-3,-2,-1,0,1,2,3)
xx<-seq(-5,10,0.01)

x<-mixing_prop(N,mu1,v1,mu2,v2,tau)
par<-em_algorithm(x,mu1+k[1],v1/abs(k[1]),mu2+k[1],v2/abs(k[1]),tau+k[1]*0.15,eps)


y_real_1<-f(xx,mu1,v1,mu2,v2,tau)
y_comp_1<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_1<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g1<-ggplot(df_1)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.25)+
  scale_color_manual(values=c('Real'='#E77B69','Computed'='#01494A'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = "none"
  )+
  annotate("text", x = 1, y = 0.245, label = bquote(k == .(k[1])), size = 3)

x<-mixing_prop(N,mu1,v1,mu2,v2,tau)
par<-em_algorithm(x,mu1,v1,mu2,v2,tau,eps)

y_real_2<-f(xx,mu1,v1,mu2,v2,tau)
y_comp_2<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_2<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g2<-ggplot(df_2)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.25)+
  scale_color_manual(values=c('Real'='#E77B69','Computed'='#01494A'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = "none"
  )+
  annotate("text", x = 1, y = 0.245, label = bquote(k == .(k[4])), size = 3)

x<-mixing_prop(N,mu1,v1,mu2,v2,tau)
par<-em_algorithm(x,mu1+k[7],v1^k[7],mu2+k[7],v2^k[7],tau+k[7]*0.15,eps)

y_real_3<-f(xx,mu1,v1,mu2,v2,tau)
y_comp_3<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_3<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g3<-ggplot(df_3)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.25)+
  scale_color_manual(values=c('Real'='#E77B69','Computed'='#01494A'),
                     guide = guide_legend(title = NULL))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    legend.position = c(-0.1,0.8)
  )+
  annotate("text", x = 1, y = 0.245, label = bquote(k == .(k[7])), size = 3)

count<-rep(0,length(k))

for (i in 1:length(k)){
  for(j in 1:times){
    if(k[i]!=0){
      x<-mixing_prop(N,mu1,v1,mu2,v2,tau)
      par<-em_algorithm(x,mu1+k[i],v1*abs(k[i])^sign(k[i]),mu2+k[i],v2*abs(k[i])^sign(k[i]),tau+0.15*k[i],eps)
      count[i]<-count[i]+par$count
    }
    else{
      x<-mixing_prop(N,mu1,v1,mu2,v2,tau)
      par<-em_algorithm(x,mu1,v1,mu2,v2,tau,eps)
      count[i]<-count[i]+par$count
    }
  }
}

count<-count/times

df_count<-data.frame(x=k,y=count)

p<-ggplot(df_count, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
  geom_line(linewidth=1,color='#E77B69') +
  geom_point(size=2.5,color='#01494A') +
  labs(title = NULL,
       x = 'k',
       y = '# of iterations') +
  theme_minimal()+
  xlim(-3.5,3.5)+
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
