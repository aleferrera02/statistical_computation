source('em_algorithm.R')
source('mixing_prop.R')

set.seed(123)

tau<-0.5

mu1<--0.5
mu2<-4

eps<-1e-5
N<-1000

v1_0<-1
mu1_0<--1
v2_0<-1
mu2_0<-1
tau_0<-0.5

times<-100

v1<-seq(1,9,length.out=5)
v2<-seq(1,9,length.out=5)

xx<-seq(-5,10,0.01)

x<-mixing_prop(N,mu1,v1[1],mu2,v2[1],tau)
par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)

y_real_1<-f(xx,mu1,v1[1],mu2,v2[1],tau)
y_comp_1<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_1<-data.frame(x=xx,y=y_real_1,y2=y_comp_1)

g1<-ggplot(df_1)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.25)+
  scale_color_manual(values=c('Real'='#F5C73D','Computed'='#492183'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = "none"
  )+
  annotate("text", x = 1, y = 0.24, label = bquote(sigma[1]^2 == .(v1[1])~ ", " ~  sigma[2]^2  == .(v2[1])), size = 2.8)
 

x<-mixing_prop(N,mu1,v1[3],mu2,v2[3],tau)
par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)

y_real_2<-f(xx,mu1,v1[3],mu2,v2[3],tau)
y_comp_2<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_2<-data.frame(x=xx,y=y_real_2,y2=y_comp_2)

g2<-ggplot(df_2)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  ylim(0,0.25)+
  scale_color_manual(values=c('Real'='#F5C73D','Computed'='#492183'))+
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
  annotate("text", x = 0, y = 0.24, label = bquote(sigma[1]^2  == .(v1[3])~ ", " ~  sigma[2]^2  == .(v2[3])), size = 2.8)

x<-mixing_prop(N,mu1,v1[5],mu2,v2[5],tau)
par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)

y_real_3<-f(xx,mu1,v1[5],mu2,v2[5],tau)
y_comp_3<-f(xx,par$mu1,par$v1,par$mu2,par$v2,par$tau)

df_3<-data.frame(x=xx,y=y_real_3,y2=y_comp_3)

g3<-ggplot(df_3)+
  ylim(0,0.25)+
  geom_line(aes(x=x,y=y,color='Real'),linewidth=1.4)+
  geom_line(aes(x=x,y=y2,color='Computed'),linewidth=0.6)+
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_color_manual(values=c('Real'='#F5C73D','Computed'='#492183'),
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
  annotate("text", x = 5, y = 0.24, label = bquote(sigma[1]^2  == .(v1[5])~ ", " ~  sigma[2]^2  == .(v2[5])), size = 2.8)


count_mat<-matrix(0,nrow=5,ncol=5)

for (i in 1:length(v1)){
  for (j in 1:length(v2)){
    for(k in 1:times){
      x<-mixing_prop(N,mu1,v1[i],mu2,v2[j],tau)
      par<-em_algorithm(x,mu1_0,v1_0,mu2_0,v2_0,tau_0,eps)
      count_mat[i,j]<-count_mat[i,j]+par$count
    }
  }
}

count_mat<-count_mat/times

colnames(count_mat) <- v2
rownames(count_mat) <- v1
mat_long <- melt(count_mat)
colnames(mat_long) <- c("v2", "v1", "iterations")

p<-ggplot(mat_long, aes(x = v2, y = v1, fill=iterations)) +
  geom_tile() + 
  scale_fill_gradient(low = "#F5C73D", high = "#492183",guide = guide_colorbar(title.position = "bottom") ) +  
  labs(title = NULL, x = expression(sigma[1]^2), y =  expression(sigma[2]^2),) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    ),
    legend.position = 'right',
  )


grid.arrange(g1,g2,g3,p,ncol=4,widths = c(0.22,0.22,0.22,0.34))

