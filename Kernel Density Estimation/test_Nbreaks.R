library(ggplot2)
source("H_opt.R")

set.seed(2)

N<-seq(5,20,length.out=16)
N<-floor(N)

H_N<-hopt_versus_N(10^6,2,5,N)
H_N

dataset<-data.frame(
  N_breaks=N,
  h_ott=H_N
)

p<-ggplot(dataset, aes(x = N_breaks, y = h_ott)) +
  geom_point(color='#4B3F79',size=2.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE,color='#D34146') +  
  labs(title = NULL, x = 'N', y = "h optimal")+
  theme_minimal() +  
  theme(
    panel.border = element_rect(colour = "black", fill = NA),
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15),  
      size = 11,               
      family = "sans"          
    )
  ) +  
  annotate("text", x = 17.9, y = max(dataset$h_ott) - 0.0005, 
           label = expression(n == 10^6), hjust = 0, family = "sans", size = 3) +
  annotate("text", x = 17.9, y = max(dataset$h_ott) - 0.0017, 
           label = expression(alpha == 2), hjust = 0, family = "sans", size = 3) +
  annotate("text", x = 17.9, y = max(dataset$h_ott) - 0.0030, 
           label = expression(beta == 5), hjust = 0, family = "sans", size = 3)

print(p)

