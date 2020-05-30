wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE) 
View(wbcd)
str(wbcd)

wbcd <- wbcd[-1] #The ID feature has been dropped

#DATA PREPARATION
table(wbcd$diagnosis) #to find the count of B and M

wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),        labels = c("Benign", "Malignant")) 

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1) 

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])


#NORMALIZATION OF NUMERIC VALUES
normalize <- function(x) {       return ((x - min(x)) / (max(x) - min(x))) } #Normalizing the numeric data

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean) 

#TRAINING A MODEL ON THE DATA
wbcd_train <- wbcd_n[1:469, ] 
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1] 
wbcd_test_labels <- wbcd[470:569, 1]

install.packages("class") 

library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)


#EVALUATING MODEL PERFORMANCE
library(gmodels) 
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE) 

#Z-SCORE STADARDIZATION
wbcd_z <- as.data.frame(scale(wbcd[-1])) 

summary(wbcd_z$area_mean) 

wbcd_train <- wbcd_z[1:469, ]  
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)  

#TESTING DIFFERENT CASES WITH DIFFERENT VALUES OF K  
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)  

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 33)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)  

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 9)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)  
