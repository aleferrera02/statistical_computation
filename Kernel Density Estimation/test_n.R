library(ggplot2)
source("H_opt.R")

set.seed(2)

exp_n<-seq(3.5,7,0.2)
n<-floor(10^exp_n)

H_n<-hopt_versus_n(n,2,5,5)

dataset<-data.frame(
  exp_n=exp_n,
  h_ott=H_n
)

p<-ggplot(dataset, aes(x = exp_n, y = H_n)) +
  geom_point(color='#416768',size=2.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), se = FALSE,color='#A3C36D') +  
  labs(title = NULL, x = 'log(n)', y = "h optimal")+
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
  annotate("text", x = 6.5, y = max(dataset$h_ott) - 0.001, 
           label = "N = 5", hjust = 0, family = "sans", size = 3) +
  annotate("text", x = 6.5, y = max(dataset$h_ott) - 0.0035, 
           label = expression(alpha == 2), hjust = 0, family = "sans", size = 3) +
  annotate("text", x = 6.5, y = max(dataset$h_ott) - 0.0060, 
           label = expression(beta == 5), hjust = 0, family = "sans", size = 3)

print(p)
