install.packages('readxl')
dataset=read_excel('Demographic data.xlsx')
attach(dataset)
y = dataset$phys
x1 = dataset$area
x2 = dataset$popul
x3 = dataset$pop1834
x4 = dataset$pop65plus
x5 = dataset$beds
x6 = dataset$crimes
x7 = dataset$higrads
x8 = dataset$bachelors
x9 = dataset$poors
x10 = dataset$unemployed
x11 = dataset$percapitaincome
x12 = dataset$totalincome
x13 = dataset$region
# Creation of Dummy Variables :-
#install.packages('fastDummies')
result = fastDummies::dummy_cols(dataset$region)
x13_1 = result$.data_1
x13_2 = result$.data_2
x13_3 = result$.data_3
x13_4 = result$.data_4
# Creation of Scatterplot matrix :-
pairs(~y+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13, pch = 18, col = "red", main = "Scatter Plot Matrix")
# Creation of pairwise scatterplots:-
par(mfrow=c(2,2)) 
plot(x1,y)
plot(x2,y)
plot(x3,y)
plot(x4,y)
plot(x5,y)
plot(x6,y)
plot(x7,y)
plot(x8,y)
plot(x9,y)
plot(x10,y)
plot(x11,y)
plot(x12,y)
# Fitting of Linear Regression model:-
regressor = lm(formula = y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13_1+x13_2+x13_3, data = dataset)
summary(regressor)

#Testing the assumptions
#install.packages("lmtest")
library("lmtest")
dwtest(regressor)

# Creation of Correlation matrix:-
corr_matrix = cor(dataset[,4:17])
# Variance Inflation Factors :-
#install.packages('car')
library(car)
vif(regressor)

# Detecting Heteroscedasticity Graphically:-
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(regressor)

# Breusch-Pagan test
#install.packages('lmtest')
lmtest::bptest(regressor)
# NCV test (Non-constant Variance Score Test):-
car::ncvTest(regressor)

#Shapiro-Wilk test of normality :-
errors = residuals(regressor)
shapiro.test(errors)
#install.packages('ggplot2')
X = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
library(ggplot2)
#install.packages('reshape2')
library(reshape2)
melted_cormat <- melt(corr_matrix)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(corr_matrix)
upper_tri

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}
# Reorder the correlation matrix
cormat <- reorder_cormat(corr_matrix)
upper_tri <- get_upper_tri(corr_matrix)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
head(melted_cormat)
ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#Testing significance of variables
install.packages("olsrr")
library("olsrr")
k=ols_step_backward_aic(regressor)
k
#Fitting model with the significant variables
regressor = lm(formula = y~x2+x5+x6+x7+x8+x11+x12+x13_1+x13_2+x13_3, data = dataset)
summary(regressor)
k=ols_step_backward_aic(regressor)
k
#Testing for multicollinearity
vif(regressor)
#Compiling the significant variables into single data frame
Comp=data.frame(x2,x5,x6,x7,x8,x11,x12,x13_1,x13_2,x13_3)
#Applying Principal Component Analysis
myPCA <- prcomp(Comp, scale. = T, center = T)
myPCA$rotation # loadings
print(summary(myPCA))
print(myPCA)
PC=myPCA$x # scores
#Fitting model with PCA 
PC=data.frame(PC)
pc1=PC$PC1
pc2=PC$PC2
pc3=PC$PC3
pc4=PC$PC4
pc5=PC$PC5
pc6=PC$PC6
pc7=PC$PC7
pc8=PC$PC8
pc9=PC$PC9
pc10=PC$PC10

pcaregressor_1 = lm(formula = y~pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10)
summary(pcaregressor_1)

#BarGraph of Proportion of Variances explained Vs Principal Components
prop_variances = c(0.3847,0.2318,0.1430,0.1329,0.0414,0.0230,0.01906,0.01521,0.00848,0.00044)
barplot(prop_variances, main= "PCA",type = "b",col = "light pink",xlab = "Principal Components", ylab = "Proportion of Variance explained")
axis(1, seq(0,10,1))

#Testing Multicollinearity
vif(pcaregressor_1)
#Testing the significance of PCA variables
k=ols_step_backward_aic(pcaregressor_1)
k
#Fitting the model with significant PCA variables
pcaregressor_2 = lm(formula = y~pc1+pc2+pc5+pc8+pc9+pc10)
summary(pcaregressor_2)

#Testing the assumptions
#install.packages("lmtest")
library("lmtest")
dwtest(pcaregressor_2)

# Creation of Correlation matrix:-
corr_matrix = cor(dataset[,4:17])
# Variance Inflation Factors :-
#install.packages('car')
#library(car)
vif(pcaregressor_2)

# Detecting Heteroscedasticity Graphically:-
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(pcaregressor_2)

# Breusch-Pagan test
install.packages('lmtest')
#library(lmtest)
lmtest::bptest(pcaregressor_2)
# NCV test (Non-constant Variance Score Test):-
car::ncvTest(pcaregressor_2)

#Shapiro-Wilk test of normality :-
errors = residuals(regressor)
shapiro.test(errors)

#Detecting outliers
ols_plot_cooksd_bar(pcaregressor_2)

#Removing outliers fitting the model
PC=PC[-c(1,2,3,4,5,6,8,9,11,12,16,19,21,22,28,32,34,35,39,48,50,47,52,53,67,168,418),]
y=data.frame(y)
y=y[-c(1,2,3,4,5,6,8,9,11,12,16,19,21,22,28,32,34,35,39,48,50,47,52,53,67,168,418),]

pc1=PC$PC1
pc2=PC$PC2
pc3=PC$PC3
pc4=PC$PC4
pc5=PC$PC5
pc6=PC$PC6
pc7=PC$PC7
pc8=PC$PC8
pc9=PC$PC9
pc10=PC$PC10

pcaregressor_2 = lm(formula = y~pc1+pc2+pc5+pc8+pc9+pc10)
summary(pcaregressor_2)

