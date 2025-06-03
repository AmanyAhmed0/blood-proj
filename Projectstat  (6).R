#Part0
#0-Reading the file
load("BloodPressure.RData")
str(BloodPressure)

#replace 0 and 1 with male and female for gender
BloodPressure$gender<-factor(BloodPressure$gender,labels=c("male","female"))
####################################################################################################
#Part1
#Data Reading 
#Descriptive statistics
summary(BloodPressure)
BloodPressure$dose=as.numeric(BloodPressure$dose)
BloodPressure$bp.reduction=as.numeric(BloodPressure$bp.reduction)
str(BloodPressure)
summary(BloodPressure)

#calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable).
#mean
mean(BloodPressure$dose, na.rm=TRUE)
mean(BloodPressure$bp.reduction, na.rm=TRUE)
#median
median(BloodPressure$dose, na.rm=TRUE)
median(BloodPressure$bp.reduction, na.rm=TRUE)
#minimum
min(BloodPressure$dose, na.rm=TRUE)
min(BloodPressure$bp.reduction, na.rm=TRUE)
#maximum
max(BloodPressure$dose, na.rm=TRUE)
max(BloodPressure$bp.reduction, na.rm=TRUE)
#1st quartile
quantile(BloodPressure$dose, 0.25, na.rm=TRUE)
quantile(BloodPressure$bp.reduction, 0.25, na.rm=TRUE)
#3rd quartile 
quantile(BloodPressure$dose, 0.75, na.rm=TRUE)
quantile(BloodPressure$bp.reduction, 0.75, na.rm=TRUE)


#For the categorical variable existing, calculate a frequency table 
table(BloodPressure$gender)

#Calculate the correlation coefficient between bp-reduction and dose
cor.test(BloodPressure$bp.reduction,BloodPressure$dose) 
cor(BloodPressure$bp.reduction,BloodPressure$dose, use="complete.obs")
#################################################################################################
#Part2
#2-Graphics
barplot(table(BloodPressure$gender), 
        xlab="Gender",ylab="Frequency", col = c("blue","red"))

barplot(tapply(BloodPressure$bp.reduction,
               list(gender=BloodPressure$gender),
               mean,na.rm=T),xlab="Gender",
               ylab="Mean bp.reduction", col =c("black", "pink") )

#histogram for each variable alone
hist(BloodPressure$dose,xlab="Dose",main="Distribution of Dose", col = "aquamarine")
hist(BloodPressure$bp.reduction,xlab="bp.reduction",
     main="Distribution of BP reduction", col = "Yellow")

#histogram for the two cont variables together by setting (add operator = True)
hist(BloodPressure$bp.reduction,xlab="bp.reduction and dose",main="Distribution of BP reduction and dose", col = "Yellow")
hist(BloodPressure$dose,xlab="Dose",main="Distribution of Dose", col = "aquamarine", add = TRUE)

#scatter plot for 2 continous variables
plot(BloodPressure$dose,BloodPressure$bp.reduction,xlab="Dose",ylab="BP reduction",main="Scatterplot", col="orange")
plot(bp.reduction~dose, data=BloodPressure)

#scatter plot with diff color for each gender
plot(BloodPressure$dose[BloodPressure$gender=="male"],
     BloodPressure$bp.reduction[BloodPressure$gender=="male"],
     xlab="Dose",ylab="BP reduction",main="Scatterplot",col = "blue")
points(BloodPressure$dose[BloodPressure$gender=="female"],
       BloodPressure$bp.reduction[BloodPressure$gender=="female"],col="red")
#add the regression lines for each gender
abline(lm(BloodPressure$bp.reduction[BloodPressure$gender=="male"]~
            BloodPressure$dose[BloodPressure$gender=="male"]), col = "blue")
abline(lm(BloodPressure$bp.reduction[BloodPressure$gender=="female"]~
            BloodPressure$dose[BloodPressure$gender=="female"]),col="red")

#Boxplot for bpreduction and doses
boxplot(BloodPressure$bp.reduction, main = "bp.reduction boxplot", col = "limegreen")
boxplot(BloodPressure$bp.reduction~as.factor(BloodPressure$dose), 
        xlab = "Doses", ylab = "BP reduction" ,
        main="Boxplot per dose", col = c("coral", "grey", "purple","whitesmoke"))

#######################################################################################################

#Part3
#3-Outlier detection
o1 = c(boxplot(BloodPressure$dose, plot = TRUE, main = "Dose boxplot", col = "blue4")$out)
o2 = c(boxplot(BloodPressure$bp.reduction, plot = TRUE,main = "bp.reduction boxplot", col = "chartreuse" )$out)
o3 = c(boxplot(BloodPressure$gender, plot = TRUE, main = "Gender boxplot", col = "#009999")$out)

#categorical variables dont have outliers
########################################################################################################

#part4
#4-Testing for normality/ homoscedasticity
BloodPressure$dose<-factor(BloodPressure$dose,labels=c("zero mg","two mg","five mg","ten mg"))
#Check the normality using two methods:

#1st Method (Shapiro-Wilk test)
shapiro.test(BloodPressure$bp.reduction)

#2nd Method (histograms and Q-Q plots)
hist(BloodPressure$bp.reduction,xlab="bp.reduction",main="Distribution of BP reduction", col = "green")
qqnorm(BloodPressure$bp.reduction)
qqline(BloodPressure$bp.reduction)

library(car)

#Check the homoscedasticity using two methods:

#1st Method (Boxplot)
boxplot(bp.reduction ~ dose, data= BloodPressure, xlab = "Doses", ylab = "BP reduction" ,main="Boxplot per dose", col = c("green", "yellow", "orange","red"))
boxplot(bp.reduction ~ gender, data= BloodPressure, xlab = "gender", ylab = "BP reduction" ,main="Boxplot per gender", col = c("green", "red"))

#2nd Method (levene test)
leveneTest(bp.reduction ~ gender,data=BloodPressure)
leveneTest(bp.reduction ~ dose,data=BloodPressure)

#According to the normality:
#the data is likely to be normally distributed as the p-value is substantially higher than 0.05
#it means that there is not enough evidence to reject the null hypothesis.(the data does not provide significant evidence to suggest that it deviates significantly from a normal distribution)
#This also was indicated from the histogram and Quantile-Quantile Plot

#According to the homoscedasticity:
# the result of the Levene's test alone does not directly determine whether the data exhibits heteroscedasticity or homoscedasticity.
#it does not provide information about the pattern of variability within each group or the relationship between the residuals and the predictors.
#Also  the variances of the groups being compared are homogeneous.
#there is not enough evidence to reject the null hypothesis.
#there is no significant difference in variances among the groups being compared.

############################################################################################################
#Part5
#5-confidence interval
Doses = unique(BloodPressure$dose)
for (dose in Doses) {
  cat(sprintf("Dose %s\n", dose))  #cat is to concatenate
  x = BloodPressure$bp.reduction[BloodPressure$dose == dose] #returns all the values of bp reduction for each dose (0,2,5,10)
  n = length(x)
  y = n-1  #degrees of freedom
  meanx = mean(x)
  SE_x = sd(x) / sqrt(n)
  
  tscore_90 = qt(0.95, df = y)  #qt function returns the tscore from t-distribution 
  CI_90 = meanx + c(-1, 1) * tscore_90 * SE_x
  cat(sprintf("  90%% CI: (%f, %f)\n", CI_90[1], CI_90[2]))
  
  tscore_95 = qt(0.975, df = y)
  CI_95 = meanx + c(-1, 1) * tscore_95 * SE_x                #(1+0.95)/2 = 0.975 complementary to multiply 2 in the normal CI formula
  cat(sprintf("  95%% CI: (%f, %f)\n", CI_95[1], CI_95[2]))
  
  tscore_99 = qt(0.995, df = y)
  CI_99 = meanx + c(-1, 1) * tscore_99 * SE_x
  cat(sprintf("  99%% CI: (%f, %f)\n", CI_99[1], CI_99[2]))
}
#for dose = 0
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 0] ~ 1, BloodPressure), level=0.90)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 0] ~ 1, BloodPressure), level=0.95)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 0] ~ 1, BloodPressure), level=0.99)
#for dose = 2
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 2] ~ 1, BloodPressure), level=0.90)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 2] ~ 1, BloodPressure), level=0.95)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 2] ~ 1, BloodPressure), level=0.99)
#for dose = 5
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 5] ~ 1, BloodPressure), level=0.90)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 5] ~ 1, BloodPressure), level=0.95)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 5] ~ 1, BloodPressure), level=0.99)
#for dose = 10
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 10] ~ 1, BloodPressure), level=0.90)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 10] ~ 1, BloodPressure), level=0.95)
confint(lm(BloodPressure$bp.reduction[BloodPressure$dose == 10] ~ 1, BloodPressure), level=0.99)

###############Both ways have the same results#################################
####################################################################################################

#6-Hypothesis Testing
#to subset the data that has dose = 0 only
placebo <- subset(BloodPressure, dose == "zero mg")

#check for normality and homoscedasticity
shapiro.test(placebo$bp.reduction)  #data is normal 
bartlett.test(bp.reduction ~ gender, data = placebo) #data is homo

#apply two sample t-test
t.test(bp.reduction ~ gender, data = placebo, var.equal = T)

#according to the two sample t-test, the p-value is less than 0.05, therefore we have
#enough evidence to reject the null hypothesis in support for the alternative, 
#there is difference in blood pressure reduction between males and females. 


# Test hypothesis that bp.reduction is “higher” in the group receiving Dose = 10
#compared to the control (Dose =0)
#Assuming heteroscedasticity 
group_control <- subset(BloodPressure, dose == 'zero mg')$bp.reduction
group_dose10 <- subset(BloodPressure, dose == 'ten mg')$bp.reduction
result <- t.test(group_dose10, group_control, var.equal = FALSE)
print(result)

#there is a significant and meaningful difference in the mean "bp.reduction" between the two groups.


#Test hypothesis that bp.reduction is different between the different doses
#(ignoring the gender). (By performing comparison between the different groups,
#after assessing the assumptions and performing post-hoc testing)
library(ggplot2)
library(car)
library(multcomp)

# Check normality assumption
ggplot(BloodPressure, aes(x = dose, y = bp.reduction)) +
  geom_boxplot() +
  xlab("Dose") +
  ylab("bp.reduction")

#Check homogeneity of variances assumption
leveneTest(bp.reduction ~ dose, data = BloodPressure)

# Perform one-way ANOVA
anova_result <- aov(bp.reduction ~ dose, data = BloodPressure)
summary(anova_result)

# Perform post-hoc testing (Tukey's HSD)
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

###########################################################################################
#Part 7
#7-linear model
load("BloodPressure.RData")

BloodPressure <- data.frame(
  dose = c(BloodPressure$dose),
  bp.reduction = c(BloodPressure$bp.reduction),
  gender = c(BloodPressure$gender))
BloodPressure

#1-
## STEP 1: Draw a graph of the data to make sure the relationship make sense
plot(BloodPressure$dose, BloodPressure$bp.reduction, pch=16, cex=2)
plot(BloodPressure$gender, BloodPressure$bp.reduction, pch=16, cex=2)
##########################################################################################################
## STEP 2: Do the regression for dose
simple.regression <- lm(bp.reduction ~ dose, data=BloodPressure)
## STEP 3: Look at the R^2, F-value and p-value
summary(simple.regression)  #dose is the slope

# p-value < 0.05 (1.247x10^-12), there is significant difference 
# multiple r-squared = 0.7387  explains to what extent does the dose predict the bpreduction = 74%
# f-value = 107.4

#fit the regression line
plot(BloodPressure$dose, BloodPressure$bp.reduction, pch=16, cex=2, xlab = "Dose", ylab = "BP Reduction")
abline(simple.regression, lwd=5, col="red")
####################################################################################################

#do the regression for gender 
simple.regression2 <- lm(bp.reduction ~ gender, data=BloodPressure)
summary(simple.regression2) #gender is the slope
#p-value > 0.05 (0.358)    #not significant
#multiple R-squared = 0.02227
# f-value = 0.8657

#fit the regression line
plot(BloodPressure$gender, BloodPressure$bp.reduction, pch=16, cex=2, xlab = "Gender", ylab = "BP Reduction")
abline(simple.regression2, lwd=5, col="red")
#According to the results, the dose is better at predicting bp.reduction than the gender, since it has lower
#p-value < 0.05.
################################################################################################
#y = intercept+slope(x)
plot(BloodPressure)
###################################################################################################

## Do MULTIpLE regression
multiple.regression <- lm(bp.reduction ~ dose + gender, data=BloodPressure)
summary(multiple.regression)
library(car)
avPlots(multiple.regression)

#p-value < 0.05 (6.928x10^-12)
#adjusted R-squared = 0.737  explains to what extent does the dose predict the bpreduction 74%
# f-value = 55.7

#according to the results the multiple regression is best in predicting the BP reduction

###########################################################################################

#2- Calculate and interpret a 95% confidence interval of the regression slope
multiple.regression <- lm(bp.reduction ~ dose+gender, data=BloodPressure)
CI <- confint(multiple.regression, level = 0.95)


#we used the confint built in function on the regression model. 
#We are 95% confident that the value of bp.reduction lies between -1.948 and 2.01426.
#the output indicates that for every 1 dose unit, we excpect to see an increase in bp.reduction between 1.43 and 2.13

############################################################################################################
#3- Estimate the average blood pressure reduction for patients that would receive 3mg/day of the medication
#multiple regression 
bp_reduction <- predict(multiple.regression, newdata = data.frame(dose = 3, gender = c(0, 1)))
x = round(bp_reduction, 2)




