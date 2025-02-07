source('CV_K.R')

bone<-read.csv('bone_mineral.csv')
bone_rest<-bone[,c(3,5)]

p_values<-1:3
h_values<-3:15
K<-10

cv_mat_2<-matrix(0,nrow=13,ncol=3)

for (i in 1:length(h_values)){
  for (j in 1:length(p_values)){
    cv_mat_2[i,j]<-err_cv_mod(bone_rest,h_values[i],p_values[j],K)
  }
}

min(cv_mat_2)
min_indices <- which(cv_mat_2 == min(cv_mat_2), arr.ind = TRUE)
h_opt<- h_values[min_indices[1]]
h_opt
p_opt<- p_values[min_indices[2]]
p_opt
f_ott<-locpol(rspnbmd~age,data=bone_rest,deg=p_opt,bw=h_opt,kernel=EpaK)

df_ott<-data.frame(age=f_ott$lpFit$age,rspnbmd=f_ott$lpFit$rspnbmd)

max(cv_mat_2)
max_indices <- which(cv_mat_2 == max(cv_mat_2), arr.ind = TRUE)
h_worse<- h_values[max_indices[1]]
h_worse
p_worse<- p_values[max_indices[2]]
p_worse
f_worse<-locpol(rspnbmd~age,data=bone_rest,deg=p_worse,bw=h_worse,kernel=EpaK)

df_worse<-data.frame(age=f_worse$lpFit$age,rspnbmd=f_worse$lpFit$rspnbmd)


p1<-ggplot(bone_rest, aes(x = age, y = rspnbmd)) +
  geom_point(size=1.5,color='#2A4D60')+
  geom_line(data=df_ott, aes(x=age,y=rspnbmd),linewidth=1,color='#60E37B') +
  theme_minimal()+
  labs(title = NULL,
       x = "age",
       y = "bone mineral density") +
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
    legend.position = "none" 
  )+
  annotate("text",label="p=3  h=5", x = 22, y = 0.18, size=3)

p2<-ggplot(bone_rest, aes(x = age, y = rspnbmd)) +
  geom_point(size=1.5,color='#2A4D60')+
  geom_line(data=df_worse, aes(x=age,y=rspnbmd),linewidth=1,color='#60E37B') +
  theme_minimal()+
  labs(title = NULL,
       x = "age",
       y = "bone mineral density") +
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_blank(),
    legend.position = "none" 
  )+
  annotate("text",label="p=1  h=15", x = 22, y = 0.18, size = 3)

grid.arrange(p1,p2,ncol=2)
