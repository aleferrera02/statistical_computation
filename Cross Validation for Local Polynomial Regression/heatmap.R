source('CV_K.R')
library(reshape2)

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


colnames(cv_mat_2) <- p_values
rownames(cv_mat_2) <- h_values
mat_long <- melt(cv_mat_2)
colnames(mat_long) <- c("Degree", "Bandwidth", "Error")

p<-ggplot(mat_long, aes(x = Degree, y = Bandwidth, fill=Error)) +
  geom_tile() + 
  scale_fill_gradient(low = "#F5C73D", high = "#492183") +  
  labs(title = NULL, x = "Bandwidth", y = "Degree") +
  scale_x_continuous(breaks = 3:15)+
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
  
print(p)

