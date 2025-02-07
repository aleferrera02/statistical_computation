source("fun.R")
source("rejection_sampling.R")

N=2^seq(5,16,1)
M1<-2.47
M2<-1.71

time_ub<-rep(0,length(N))
time_ubd<-rep(0,length(N))

for (i in 1:length(N)){
  x1<-rejection_sampling(N[i],p,upper_bound,rand_ub,M1)
  x2<-rejection_sampling(N[i],p,upper_bound_divided,rand_ubd,M2)
  time_ub[i]<-x1$time
  time_ubd[i]<-x2$time
}

df<-data.frame(N=N,time_ub=time_ub,time_ubd=time_ubd)

p<-ggplot(df) +
  geom_line(aes(N,time_ub,color='Unique'),linewidth=1.5) +
  geom_point(aes(N,time_ub,color='Unique'),size=2.8) +
  geom_line(aes(N,time_ubd,color='Separated'),linewidth=1.5) +
  geom_point(aes(N,time_ubd,color='Separated'),size=2.8) +
 # scale_x_continuous(trans = 'log2') + 
  labs(title = NULL,
       x = 'N',
       y = 'seconds',
       color = NULL) +
  scale_color_manual(values=c('Separated'='#28E57B','Unique'='#501087'),
                     breaks = c('Unique', 'Separated'))+
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
    ),
    legend.position = c(0.2, 0.7))

print(p)
