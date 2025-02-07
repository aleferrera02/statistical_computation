source('CV_K.R')

bone<-read.csv('bone_mineral.csv')
bone_rest<-bone[,c(3,5)]

p_values<-1:3
h_values<-3:15
K<-10

#naive k cross validation

cv_mat<-matrix(0,nrow=13,ncol=3)

for (i in 1:length(h_values)){
  for (j in 1:length(p_values)){
    cv_mat[i,j]<-err_cv(bone_rest,h_values[i],p_values[j],K)
  }
}

cv_df <- data.frame(h_values, cv_mat)
cv_long <- melt(cv_df, id.vars = "h_values", variable.name = "Degree", value.name = "CV 10 folds")
levels(cv_long$Degree)<-c('1','2','3')

colnames(cv_long)[1]<-'h'

p1<-ggplot(cv_long, aes(x = h, y = `CV 10 folds`, color = Degree)) +
  geom_point(size=2.5) +
  geom_line(linewidth=1) +
  ylim(c(0.00167,0.001855))+
  labs(title = NULL,
       x = "Bandwidth",
       y = "Naive 10-fold-CV") +
  theme_minimal()+
  scale_color_manual(values=c('#4464ad','#9FC490','#902923'))+
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
    legend.position = 'none'
  )


#efficient k cross validation

cv_mat_2<-matrix(0,nrow=13,ncol=3)

for (i in 1:length(h_values)){
  for (j in 1:length(p_values)){
    cv_mat_2[i,j]<-err_cv_mod(bone_rest,h_values[i],p_values[j],K)
  }
}

cv_df_2 <- data.frame(h_values, cv_mat_2)
cv_long_2 <- melt(cv_df_2, id.vars = "h_values", variable.name = "Degree", value.name = "CV 10 folds")
levels(cv_long_2$Degree)<-c('1','2','3')

colnames(cv_long_2)[1]<-'h'

p2<-ggplot(cv_long_2, aes(x = h, y = `CV 10 folds`, color = Degree)) +
  geom_point(size=2.5) +
  geom_line(linewidth=1) +
  ylim(c(0.00167,0.001855))+
  labs(title = NULL,
       x = "Bandwidth",
       y = "Adapted 10-fold-CV") +
  theme_minimal()+
  scale_color_manual(values=c('#4464ad','#9FC490','#902923'))+
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
    legend.position = c(0.13,0.8),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8)
  )


grid.arrange(p1, p2, ncol = 2)

