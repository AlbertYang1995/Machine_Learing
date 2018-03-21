wbcd <- read.csv("D:\\ндов\\Machine-Learning-with-R-datasets-master\\wisc_bc_data.csv", header = T, stringsAsFactors = F)
str(wbcd)
wbcd <- wbcd[, -1]
table(wbcd$diagnosis)
#-----------------------------------------------------------------
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
#-----------------------------------------------------------------------
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
#---------------------------------------------------------------------
summary(wbcd$radius_mean)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
min(wbcd$radius_mean)
wbcd[1]
str(wbcd)
lapply(wbcd[2:31], normalize)
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
str(wbcd_n)
summary(wbcd_n$radius_mean)
wbcd_n[1, 1]
str(wbcd_n)
#-------------------------------------
wbcd_train <- wbcd_n[1:469,]
#--------------------------------------
wbcd_test <- wbcd_n[470:569,]
#--------------------------------------
wbcd_train_labels <- wbcd[1:469, 1]
#--------------------------------------
wbcd_test_labels <- wbcd[470:569, 1]
#-------------------------------------
install.packages("class")      #KNN packages
library(class)
#-------------------------------------
str(wbcd_train_labels)
table(wbcd_train_labels)
#--------------------------------------
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
#-----------------------------------------------------------------
str(wbcd_test_labels)
#----------------------------
install.packages("gmodels")
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = F)
#-------------------------------------------------------------




