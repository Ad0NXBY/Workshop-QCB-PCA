#Install (if necessary) and load required packages
pkgs_needed <- c("tidyverse", "factoextra", "ggpubr")
for (pkg in pkgs_needed) {
  if(!require(pkg, character.only = T)) library(pkg, character.only = T)
}
install.packages("tidyverse")
install.packages("factoextra")
install.packages("ggpubr")

#load librarys
library(tidyverse)
library(factoextra)
library(ggpubr)

#Download and explore iris dataset
data("iris")
dim(iris)
head(iris)
str(iris)

#Creating boxplots
#Creating boxplot to visualizee the distribution of values for each variable
pA <-
  iris %>%
  pivot_longer(!Species) %>% #This changes the data to long format which is compatible with ggplot
  ggplot(aes(x = name, y = value)) + #This defines the values to be plotted
  geom_boxplot() + #specifics the type of plot
  theme_pubr(base_size = 9) + #this specifies the theme used for plotting and the base font size)
  theme(axis.title.x = element_blank()) + #this removes the x ais title
  rotate_x_text(angle = 45) + #this rotates the x-axis text by 45 degress
  ggtitle("Raw data")

#Create boxplot to visualize the distribution of values for each variable after cetering the data
pB <-
  as.data.frame(scale(iris[,1:4], center = T, scale = F)) %>%
  pivot_longer(names(iris[1:4])) %>%
  ggplot(aes(x = name, y = value)) + 
  geom_boxplot() +
  theme_pubr(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  rotate_x_text(angle = 45) +
  ggtitle("Centered data")

#Create boxplot to visualize distribution of values for each variable after (Z-score) scaling and centering data
pC <-
  as.data.frame(scale(iris[,1:4], center = T, scale = T)) %>%
  pivot_longer(names(iris[1:4])) %>%
  ggplot(aes(x = name, y = value)) + 
  geom_boxplot() +
  theme_pubr(base_size = 9) +
  theme(axis.title.x = element_blank()) +
  rotate_x_text(angle = 45) +
  ggtitle("Scaled & Centered data")

#Create multipanel plot to examine how these data processing approches alter the data
ggarrange(pA, pB, pC, ncol = 3, labels = "AUTO")
  
#Creating a scatter plot (using ggplot) to visualize the strong positive correlation (r = 0.963) found between petal length and petal width
iris %>%
  select(!Species) %>%
  scale(center = T, scale = T) %>%
  cor()

iris %>%
  ggplot(aes(x = scale(Petal.Length), y = scale(Petal.Width))) + #ddefines the variables to plot
  geom_point(aes(color = Species)) + #plots individual points for the selected variables
  geom_smooth(method = "lm") + #Adds a line of best fit defined by linear regression
  theme_classic() #defines the theme to be used

#PCA IMPLEMENTED BY PRCOMP
#summary function shows key results
iris.svd <-
  iris %>%
  select(!Species) %>%
  prcomp(scale = F)
summary(iris.svd)

#EXPLORING PC "IMPORATNCE" WITH A SCREE PLOT
PCs_sd <- apply(iris.svd$x,2,sd)
PCs_sd

PCs_sd^2/sum(PCs_sd^2)

cumsum(PCs_sd^2/sum(PCs_sd^2))

#perform calculation on the raw data to demonstrate that PCA has extracted a larger proprotion of the dataset's information with a reduced number of variables compared to using the raw data
raw_sds <- apply(iris[,-5],2,sd) #Apply dunction is used to calculate the standard deviation in the raw data fro each variable (omitting species)
raw_sds_prop <- raw_sds^2/sum(raw_sds^2)
cumsum(raw_sds_prop[order(-raw_sds_prop)])

#Plot the graph
plot(cumsum(PCs_sd^2/sum(PCs_sd^2)), type = "o", lty = 1, ylim = c(0.5,1),
     ylab = "Cumulative Proportion of Variance", xlab = "Dimensions")
points(cumsum(raw_sds_prop[order(-raw_sds_prop)]), col = "red", pch = "*")
lines(cumsum(raw_sds_prop[order(-raw_sds_prop)]), col = "red", lty = 2)
legend(3,0.7, legend = c("PCA", "raw"), col = c("black", "red"), lty = c(1,2))

#Scree plot- plots proportion of variance within each PC, can easily visualize where the PCs start to hold negligible proportions of the total variance and therore minimal information
barplot(PCs_sd^2/sum(PCs_sd^2), ylab = "Proportion of Variance")

#factoextra package provide simple function to generate nice plots disyplaying same information
fviz_eig(iris.svd, addlabels = T) +
  theme_pubr(base_size = 9)

#Feature Loadings and PC scores
str(iris.svd)
iris.svd$rotation
#Use factoextra package to visualize contribution of each feature to different PCs
fviz_pca_var(iris.svd) + 
  xlim(c(-0.5,2)) + ylim(c(-0.5,0.2))#additional line of code to specify axis limits so that labels are not cut 

#the square cosine (Cos2) can be used to determine the relative contribution of each feature to a feature and then can be used to rank importance
fviz_pca_var(iris.svd, select.var = list(cos2 = 2)) + xlim(c(-0.5,2)) + ylim(c(-0.5,0.2))

#feature loadings are also coefficients used to deirive each samples P scores, found in the "x" element of prcomp object
dim(iris.svd$x)
head(iris.svd$x)
#PC score calculation by performing matrix multiplication using the input data for PCA
PC_scores <- scale(iris[,-5], center = T, scale = F) %*% iris.svd$rotation
head(PC_scores)
table(PC_scores == iris.svd$x)

#use factoextra package to plot each observation in two dimensions absed on the scores for the two PCs
fviz_pca_ind(iris.svd, habillage = iris$Species, label = "none", addEllipses = T, invisible = "quali") +
  theme_pubr(base_size = 9)
 
#use biplots to visualize the PC scores and feature loadings simultaneously
fviz_pca_biplot(iris.svd, habillage = iris$Species, label = "var", addEllipses = T, invisible = "quali") +
  theme_pubr(base_size = 9) +
  expand_limits(x = 6) #This command increases the x axis limit to 6 so that the petal length label is not cropped
