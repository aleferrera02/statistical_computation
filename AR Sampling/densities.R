source('fun.R')
#install.packages(c('ggplot2','reshape2','gridExtra','grid',"latex2exp"))
library(latex2exp)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

x<-seq(0,1,length=10000)

y<-sapply(x,p)
y1<-sapply(x,upper_bound)
y2<-sapply(x,upper_bound_divided)

c1<-max(y/y1)
c1<-ceiling(c1*100)/100
c1
c2<-max(y/y2)
c2<-ceiling(c2*100)/100
c2

df<-data.frame(x=x,y=y,y1=y1*c1)

p1<-ggplot(df)+
  geom_line(aes(x=x,y=y1,color='Upper Bound'),linewidth=1.4)+
  geom_line(aes(x=x,y=y,color='Density'),linewidth=0.6)+
  labs(title = NULL,
       x=NULL,
       y=NULL,
       color=NULL) +
  ylim(0,2.8)+
  scale_color_manual(values=c('Density'='#72004E','Upper Bound'='#E0001D'),
                     labels = c(TeX("$f(x)$"), TeX("$c_1 g_1(x)$")))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = c(0.2, 0.6),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
      face = "bold"            
    )
  )

df2<-data.frame(x=x,y=y,y2=y2*c2)

p2<-ggplot(df2)+
  geom_line(aes(x=x,y=y2,color='Upper Bound'),linewidth=1.4)+
  geom_line(aes(x=x,y=y,color='Density'),linewidth=0.6)+
  labs(title = NULL,
       x=NULL,
       y=NULL,
       color=NULL) +
  ylim(0,2.8)+
  scale_color_manual(values=c('Density'='#72004E','Upper Bound'='#E0001D'),
                     labels = c(TeX("$f(x)$"), TeX("$c_2 g_2(x)$")))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    legend.position = c(0.2, 0.6),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
      face = "bold"            
    )
  )

p<-grid.arrange(p1,p2,ncol=2)
