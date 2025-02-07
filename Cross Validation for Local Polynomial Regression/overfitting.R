source('CV_K.R')

bone<-read.csv('bone_mineral.csv')
bone_rest<-bone[,c(3,5)]

p_values<-1:4
K<-10

cv_3<-rep(0,length(p_values))
cv_4<-rep(0,length(p_values))
cv_5<-rep(0,length(p_values))

cv_10<-rep(0,length(p_values))
cv_12<-rep(0,length(p_values))
cv_13<-rep(0,length(p_values))

for (i in 1:length(p_values)){
  cv_3[i]<-err_cv_mod(bone_rest,3,p_values[i],K)
  cv_4[i]<-err_cv_mod(bone_rest,4,p_values[i],K)
  cv_5[i]<-err_cv_mod(bone_rest,5,p_values[i],K)
  
  cv_10[i]<-err_cv_mod(bone_rest,10,p_values[i],K)
  cv_12[i]<-err_cv_mod(bone_rest,12,p_values[i],K)
  cv_13[i]<-err_cv_mod(bone_rest,13,p_values[i],K)
}

df<-data.frame(p=p_values,cv_3,cv_4,cv_5)

p1<-ggplot(df)+
  geom_point(aes(x=p,y=cv_3,color='3'),size=2)+
  geom_line(aes(x=p,y=cv_3,color='3'),linewidth=1)+
  geom_point(aes(x=p,y=cv_4,color='4'),size=2)+
  geom_line(aes(x=p,y=cv_4,color='4'),linewidth=1)+
  geom_point(aes(x=p,y=cv_5,color='5'),size=2)+
  geom_line(aes(x=p,y=cv_5,color='5'),linewidth=1)+
  scale_color_manual(values=c('3'='#60C9D2','4'='#B6174B','5'='#519872'), name = "Bandwidth") +
  labs(title = NULL, x = "Degree", y = "CV error") +
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
    legend.position = 'bottom'
  )

df_1<-data.frame(p=p_values,cv_10,cv_12,cv_13)

p2<-ggplot(df_1)+
  geom_point(aes(x=p,y=cv_10,color='10'),size=2)+
  geom_line(aes(x=p,y=cv_10,color='10'),linewidth=1)+
  geom_point(aes(x=p,y=cv_12,color='12'),size=2)+
  geom_line(aes(x=p,y=cv_12,color='12'),linewidth=1)+
  geom_point(aes(x=p,y=cv_13,color='13'),size=2)+
  geom_line(aes(x=p,y=cv_13,color='13'),linewidth=1)+
  scale_color_manual(values=c('10'='#7FBEAB','12'='#CA1551','13'='#E3B505'), name = "Bandwidth") +
  labs(title = NULL, x = "Degree", y = NULL) +
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
    legend.position = 'bottom'
  )

grid.arrange(p1,p2,ncol=2)

