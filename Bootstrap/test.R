#install.packages(c("boot",'ggplot2',gridExtra','grid','tidyr'))
library(boot)  
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)

asymptotic_ci <- function(data, alpha, true_sd) {
  n <- length(data)
  sample_mean <- mean(data)
  z_value <- qnorm(1 - alpha/2)
  
  lower <- sample_mean - z_value * true_sd / sqrt(n)
  upper <- sample_mean + z_value * true_sd / sqrt(n)
  
  return(c(lower, upper))
}

set.seed(21)

alpha <- 0.05
B <- 10000       
true_mean <- 1/2  
true_sd <- 1/2
N_values <- c(10, 50, 100)
num_runs <- 1000

sample <- rexp(10, rate=2)

coverage_matrix <- matrix(0, nrow=length(N_values), ncol=4)
colnames(coverage_matrix) <- c("Asymp.", "Basic.", "Stud.", "Percent.")

interval_length_matrix <- coverage_matrix

stat_function <- function(x, i) {
  resampled <- x[i]
  return(c(mean = mean(resampled), std_dev = sd(resampled)))
}

for (i in 1:length(N_values)) {
  N <- N_values[i]
  for (j in 1:num_runs) {
    sample <- rexp(N, rate=2)
    
    boot_result <- boot(sample, stat_function, R = B)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = c("basic","stud","perc"))
    
    as_ci <- asymptotic_ci(sample, alpha, true_sd)
    
    coverage_matrix[i, 1] <- coverage_matrix[i, 1] + as.numeric(as_ci[1] <= true_mean & as_ci[2] >= true_mean)    
    coverage_matrix[i, 2] <- coverage_matrix[i, 2] + as.numeric(boot_ci$basic[4] <= true_mean & boot_ci$basic[5] >= true_mean)
    coverage_matrix[i, 3] <- coverage_matrix[i, 3] + as.numeric(boot_ci$student[4] <= true_mean & boot_ci$student[5] >= true_mean)
    coverage_matrix[i, 4] <- coverage_matrix[i, 4] + as.numeric(boot_ci$percent[4] <= true_mean & boot_ci$percent[5] >= true_mean)
    
    interval_length_matrix[i, 1] <- interval_length_matrix[i, 1] + as_ci[2] - as_ci[1]
    interval_length_matrix[i, 2] <- interval_length_matrix[i, 2] + boot_ci$basic[5] - boot_ci$basic[4]
    interval_length_matrix[i, 3] <- interval_length_matrix[i, 3] + boot_ci$student[5] - boot_ci$student[4]
    interval_length_matrix[i, 4] <- interval_length_matrix[i, 4] + boot_ci$percent[5] - boot_ci$percent[4]
  }
  
  coverage_matrix[i, ] <- coverage_matrix[i, ] / num_runs
  interval_length_matrix[i, ] <- interval_length_matrix[i, ] / num_runs
}

coverage_matrix

data1 <- data.frame(
  Method = colnames(coverage_matrix),
  N10 = coverage_matrix[1, ]
)

plot1 <- ggplot(data1, aes(x = Method, y = N10, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N10, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  labs(title = 'N=10', x = NULL, y = "Coverage Proportion") +
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  ylim(0, 1) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 20), 
      size = 11,               
      family = "sans"          
    ),
    axis.title.y = element_text(
      margin = margin(r = 15), 
      size = 11,               
      family = "sans"          
    ),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none",
  )

data2 <- data.frame(
  Method = colnames(coverage_matrix),
  N50 = coverage_matrix[2, ]
)

plot2 <- ggplot(data2, aes(x = Method, y = N50, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N50, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  labs(title = 'N=50', x = NULL, y = NULL ) +
  ylim(0, 1) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none"
  )

data3 <- data.frame(
  Method = colnames(coverage_matrix),
  N100 = coverage_matrix[3, ]
)

plot3 <- ggplot(data3, aes(x = Method, y = N100, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N100, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  labs(title = 'N=100', x = NULL, y = NULL ) +
  ylim(0, 1) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none"
  )

grid.arrange(plot1, plot2, plot3, ncol = 3)



data1_1 <- data.frame(
  Method = colnames(interval_length_matrix),
  N10 = interval_length_matrix[1, ]
)

plot1 <- ggplot(data1_1, aes(x = Method, y = N10, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N10, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  labs(title = 'N=10', x = NULL, y = "Interval lenght") +
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  ylim(0, max(data1_1$N10)+0.1) +
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
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none"
  )

data2_2 <- data.frame(
  Method = colnames(interval_length_matrix),
  N50 = interval_length_matrix[2, ]
)

plot2 <- ggplot(data2_2, aes(x = Method, y = N50, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N50, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  labs(title = 'N=50', x = NULL, y = NULL ) +
  ylim(0, max(data1_1$N10)+0.1) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none"
  )

data3_3 <- data.frame(
  Method = colnames(interval_length_matrix),
  N100 = interval_length_matrix[3, ]
)

plot3 <- ggplot(data3_3, aes(x = Method, y = N100, fill = Method)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(N100, 2)), 
            vjust = -0.5, 
            size = 3.5, 
            family = "sans") + 
  scale_fill_manual(values = c("#A4243B", "#BD632F", "#D8973C", '#D8C99B')) +
  labs(title = 'N=100', x = NULL, y = NULL ) +
  ylim(0, max(data1_1$N10)+0.1) +
  theme_minimal()+
  theme(
    axis.title.x = element_text(
      margin = margin(t = 15), 
      size = 11,               
      family = "sans"          
    ),
    axis.text.y = element_blank(),
    plot.title = element_text(
      family = "sans",        
      size = 14,   
      hjust = 0.5, 
      vjust = 1, 
    ),
    legend.position = "none"
  )

grid.arrange(plot1, plot2, plot3, ncol = 3)
