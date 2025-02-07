source("fun.R")
source("rejection_sampling.R")

set.seed(123)

N=2^seq(5,16,1)
M1<-2.47
M2<-1.71

rej_rate_ub<-rep(0,length(N))
rej_rate_ubd<-rep(0,length(N))

for (i in 1:length(N)){
  x1<-rejection_sampling(N[i],p,upper_bound,rand_ub,M1)
  x2<-rejection_sampling(N[i],p,upper_bound_divided,rand_ubd,M2)
  rej_rate_ub[i]<-x1$rej_rate
  rej_rate_ubd[i]<-x2$rej_rate
}

df<-data.frame(N=N,rej_ub=rej_rate_ub,rej_ubd=rej_rate_ubd)

p<-ggplot(df) +
  geom_line(aes(N,rej_ub,color='Unique'),linewidth=1.5) +
  geom_point(aes(N,rej_ub,color='Unique'),size=3) +
  geom_hline(yintercept=1-1/M1,linetype='dashed',color='#6F3B36',linewidth=1)+
  geom_line(aes(N,rej_ubd,color='Separated'),linewidth=1.5) +
  geom_point(aes(N,rej_ubd,color='Separated'),size=3) +
  geom_hline(yintercept=1-1/M2,linetype='dashed',color='#172E2E',linewidth=1)+
  ylim(0.3,0.7)+
  labs(title = NULL,
       x = 'N',
       y = ' ',
       color = NULL) +
  scale_color_manual(values=c('Separated'='#A3D6A2','Unique'='#DBBAAC'),
                     breaks = c('Unique', 'Separated'))+
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y. = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans",
    ),
    legend.position = c(0.8, 0.9),
    legend.text = element_text(size = 7) )

print(p)
