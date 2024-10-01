Baseball <- read.csv("/cloud/project/Baseball.csv")
View(Baseball)
install.packages('psych')
library(psych)
BaseballStats <- describe(Baseball)
View(BaseballStats)
BaseballCorrelation <- round(cor(Baseball, method = 'pearson'),3)
View(BaseballCorrelation)
install.packages("reshape2") 
library(reshape2)
install.packages('ggplot2')
library(ggplot2)
install.packages('arules')
library(arules)

head(BaseballCorrelation)

melted_corr_mat <- melt(BaseballCorrelation)

BaseballAssociation <- apriori(Baseball, parameter = list(supp=0.2, conf=0.7))

ggplot(melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3)), color = "black", size = 1.5, angle = 0, hjust = 0.5, vjust = 0.5) + # Adjust text size and angle
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  labs(title = "Correlation Matrix Heatmap",
       subtitle = "Heatmap of correlation coefficients between variables",
       x = "Variables",
       y = "Variables") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 5, hjust = 1),
        axis.text.y = element_text(size = 5))                                  