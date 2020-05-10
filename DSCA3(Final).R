# Reading the dataset into Rstudio
getwd()
economy <- read.csv("/Users/a.bsurekha/Downloads/economy.csv", header = FALSE)
new_colnames <- c("Country_name","Country_code","Year","Indicator_name","Indicator_code",
                  "Value")
colnames(economy) <- new_colnames

# representing the data in R
View(economy)

#changing year from factor to numeric
head(economy$Year)
economy$Year <- as.character(economy$Year)
str(economy)
economy$Year <- as.numeric(as.character(economy$Year))

#creating dataframe
economy_df <- data.frame(economy)

#displaying total number of null values
sum(is.na(economy_df))
summary(economy_df)

#replacing year mean values with NA 
economy$Year[is.na(economy$Year)] <- round(mean(economy$Year, na.rm = TRUE))
str(economy)


#changing value from factor to numeric
head(economy$Value)
economy$Value <- as.character(economy$Value)
economy$Value <- as.numeric(as.character(economy$Value))
str(economy)
summary(economy_df)

#replacing year mean values with NA 
economy$Value[is.na(economy$Value)] <- round(mean(economy$Value, na.rm = TRUE))
str(economy)
#bar plot
barplot(table(economy_df$Year))

#density plot
d <- density(economy$Year)
plot(d)

#hist plot according to the year
library(ggplot2)
economy_yearwise <- economy$Year
economy_valuewise <- economy$Value
hist(economy_valuewise)
hist(economy_yearwise)
plot(x=economy$Year, y=economy$Value)

# normality test carried out by using the shapiro test  
ModelData = economy$Value
ModelData1 = economy$Year
shapiro.test(ModelData[0:5000])$p.value
shapiro.test(ModelData1[0:5000])$p.value
normality_test <- shapiro.test(ModelData[0:5000])
normality_test
normality_test <- shapiro.test(ModelData1[0:5000])
normality_test

# normality checking using q-q plot for value and year
qqnorm(economy$Value)
qqline(economy$Value, col = 'blue')
qqnorm(economy$Year)
qqline(economy$Year, col = 'red')

# library pwr and dplyr are used to perform the power analysis
#install.packages("pwr")
library(pwr)
library(dplyr)

# to check the effective size 
effective_size <- cohen.ES(test = "r", size = "large")
effective_size

#power analaysis is to check the oprtimal sample sizes
power_analysis <- pwr.r.test(n= NULL, # observations in each group 
                             r = 0.5, sig.level = 0.05, # Type I probability
                             power = 0.95, 
                             alternative = "two.sided")
power_analysis
plot(power_analysis)

# spearman test is stastical method which is used to find the relationship between the two continous variables
test <- cor.test(economy$Year, economy$Value, method = 'spearman', exact = FALSE)
test

new_df <- data.frame(economy$Year, economy$Value)
str(new_df)
#pca analysis for price and date data.
#my dataframe consists of only price and date, which are in numeric format
pca <- prcomp(new_df, center = TRUE, scale. = TRUE)
summary(pca)  #we can see 2 pca are obtained pc1 and pc2.
str(pca) #standard deviation, center and scale can be seen by running the structure
#install.packages("factoextra")
library("factoextra")
eig_values <- get_eigenvalue(pca)
#we can see variance of 67.04% and 32.95% for dim1 and dim2 and eigen values are displayed for both the dimensions.
eig_values 

library("FactoMineR")
pca2 <- PCA(new_df, graph = FALSE)
print(pca2)
pca2_eig_values <- get_eigenvalue(pca2)
pca2_eig_values

fviz_eig(pca, addlabels = TRUE, ylim = c(0, 100)) #variances are plotted with respect to dimensions.

pca_for_variables <- get_pca_var(pca)
pca_for_variables

library("corrplot")
fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var = "contrib",
             gradient.cols = c("red", "Blue", "Green"),
) #


#res <- cor.test(economy$Year, economy$Value, method = 'pearson')
#res

#***********************checking the relation for the indicator name and year************************************
economy$Indicator_name <- as.character(economy$Indicator_name)
another_df <- data.frame(economy$Indicator_name, economy$Year)
str(another_df) #structure of data frame
summary(another_df)
sort(table(another_df$economy.Indicator_name), increasing = TRUE)

#plotting the data points
barplot(table(another_df$economy.Indicator_name))
plot(x=economy_yearwise, y=another_df$economy.Indicator_name)

#normality test
ks.test(another_df$economy.Indicator_name, new_df$economy.Year)

#as the data consists of numerical and factoral data, chisquare test is used.
chisq.test(another_df$economy.Indicator_name, new_df$economy.Year, correct=FALSE)

#install.packages("corrplot")
#corelation graph displaying values.
#library(corrplot)
#M <- cor(new_df)
#corrplot(M, method = "number")
#new dataframe

