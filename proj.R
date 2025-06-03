#Data Reading 
load("BloodPressure.RData")
#Descriptive statistics
summary(BloodPressure)
BloodPressure$dose=as.numeric(BloodPressure$dose)
BloodPressure$bp.reduction=as.numeric(BloodPressure$bp.reduction)
BloodPressure$gender=as.numeric(BloodPressure$gender)
str(BloodPressure)
summary(BloodPressure)

#calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable).
#mean
mean(BloodPressure$dose, na.rm=TRUE)
mean(BloodPressure$bp.reduction, na.rm=TRUE)
mean(BloodPressure$gender, na.rm=TRUE)
#median
median(BloodPressure$dose, na.rm=TRUE)
median(BloodPressure$bp.reduction, na.rm=TRUE)
median(BloodPressure$gender, na.rm=TRUE)
#minimum
min(BloodPressure$dose, na.rm=TRUE)
min(BloodPressure$bp.reduction, na.rm=TRUE)
min(BloodPressure$gender, na.rm=TRUE)
#maximum
max(BloodPressure$dose, na.rm=TRUE)
max(BloodPressure$bp.reduction, na.rm=TRUE)
max(BloodPressure$gender, na.rm=TRUE)

#1st quartile
quantile(BloodPressure$dose, 0.25, na.rm=TRUE)
quantile(BloodPressure$bp.reduction, 0.25, na.rm=TRUE)
quantile(BloodPressure$gender, 0.25, na.rm=TRUE)

#3rd quartile 
quantile(BloodPressure$dose, 0.75, na.rm=TRUE)
quantile(BloodPressure$bp.reduction, 0.75, na.rm=TRUE)
quantile(BloodPressure$gender, 0.75, na.rm=TRUE)


#For the categorical variable existing, calculate a frequency table 
table(BloodPressure$gender)


#Calculate the correlation coefficient between bp-reduction and dose

cor.test(BloodPressure$bp.reduction,BloodPressure$dose) 



