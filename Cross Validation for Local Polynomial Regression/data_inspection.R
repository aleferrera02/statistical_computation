#install.packages(c("locpol",'ggplot2','reshape2','gridExtra','grid'))
library(ggplot2)
library(locpol)
library(reshape2)
library(gridExtra)
library(grid)
library(dplyr)

bone<-read.csv('bone_mineral.csv')
colnames(bone)[c(4,6)]<-c('Sex','Ethnicity')

bone$Sex <- ifelse(bone$Sex=='male', "Male", "Female")


p1<-ggplot(bone, aes(x = age, y = rspnbmd,color=Sex)) +
  geom_point(size=1.8)+
  scale_color_manual(values=c('#DE8E82','#37CABA'))+
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
    legend.position = 'bottom',
  )

bone_filtered <- bone %>% filter(!is.na(Ethnicity))

p2<-ggplot(bone_filtered, aes(x = age, y = rspnbmd,color=Ethnicity)) +
  geom_point(size=1.8)+
  scale_color_manual(values=c('#F4E87C','#8E3B46','#9A48D0','#E88873'))+
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
    legend.position = 'bottom',
    axis.title.y = element_blank(),
  )

grid.arrange(p1,p2,ncol=2)

