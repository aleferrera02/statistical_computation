source("fun.R")
source("rejection_sampling.R")

set.seed(137)

N1<-2^5
N2<-2^9
N3<-2^13
N4<-2^16

M1<-2.47
M2<-1.71

ub1<-rejection_sampling(N1,p,upper_bound,rand_ub,M1)

d<-density(ub1$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df1<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p1<-ggplot(df1) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = 'Unique Upper Bound',
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"          
    ))


ubd1<-rejection_sampling(N1,p,upper_bound_divided,rand_ubd,M2)

d<-density(ubd1$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df1d<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p2<-ggplot(df1d) +
  geom_line(aes(x,y_true,color='True'),linewidth=1.5)+
  geom_line(aes(x,y_sample,color='Computed'),linewidth=0.6) +
  labs(title = 'Separated Upper Bound',
       x = NULL,
       y = NULL,
       color = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  scale_color_manual(values=c('Computed'='#49306C','True'='#BBA8EF'))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"         
    ),
    axis.text.y = element_blank(),
    legend.position = c(0.24,0.85))

g1<-arrangeGrob(p1,p2,ncol=2,top = textGrob(expression(N == 2^5) ,
                                         gp = gpar(fontsize = 10, fontface = "bold")))


ub2<-rejection_sampling(N2,p,upper_bound,rand_ub,M1)

d<-density(ub2$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df2<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p3<-ggplot(df2) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = 'Unique Upper Bound',
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"          
    ))


ubd2<-rejection_sampling(N2,p,upper_bound_divided,rand_ubd,M2)

d<-density(ubd2$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df2d<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p4<-ggplot(df2d) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = 'Separated Upper Bound',
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"         
    ),
    axis.text.y = element_blank())

g2<-arrangeGrob(p3,p4,ncol=2,top = textGrob(expression(N == 2^9) ,
                                             gp = gpar(fontsize = 10, fontface = "bold")))


ub3<-rejection_sampling(N3,p,upper_bound,rand_ub,M1)

d<-density(ub3$samples,kernel = 'gaussian')

x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df3<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p5<-ggplot(df3) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"          
    ))

ubd3<-rejection_sampling(N3,p,upper_bound_divided,rand_ubd,M2)

d<-density(ubd3$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df3d<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p6<-ggplot(df3d) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"         
    ),
    axis.text.y = element_blank())

g3<-arrangeGrob(p5,p6,ncol=2,top = textGrob(expression(N == 2^13) ,
                                             gp = gpar(fontsize = 10, fontface = "bold")))

ub4<-rejection_sampling(N4,p,upper_bound,rand_ub,M1)

d<-density(ub4$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df4<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p7<-ggplot(df4) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"          
    ))

ubd4<-rejection_sampling(N4,p,upper_bound_divided,rand_ubd,M2)

d<-density(ubd4$samples,kernel = 'gaussian')
x<-d$x
y_sample<-d$y
y_true<-sapply(x,p)

df4d<-data.frame(x=x,y_sample=y_sample,y_true=y_true)

p8<-ggplot(df4d) +
  geom_line(aes(x,y_true),color='#BBA8EF',linewidth=1.5)+
  geom_line(aes(x,y_sample),color='#49306C',linewidth=0.6) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.5))+
  theme_minimal()+
  theme(
    title = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 8,               
      family = "sans"          
    ),
   
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 8,               
      family = "sans"         
    ),
    axis.text.y = element_blank())

g4<-arrangeGrob(p7,p8,ncol=2,top = textGrob(expression(N == 2^16) ,
                                             gp = gpar(fontsize = 10, fontface = "bold")))

g<-grid.arrange(g1,g2,g3,g4,ncol=2)
grid.draw(g)
