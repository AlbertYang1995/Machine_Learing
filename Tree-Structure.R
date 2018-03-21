credit <- read.csv("D:\\workdir\\credit.csv", stringsAsFactors = T)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)
credit <- credit[-(18:21)]
str(credit)
#------------------------------
set.seed(123)
train_sample <- sample(1000, 900)
#------------------------------
str(train_sample)
#--------------------------------------
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]
#--------------------------------------
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
credit$default <- factor(credit$default, levels = c('1', '2'), labels = c("no", "yes"))
table(credit$default)
#------------------------
install.packages("C50") #TREE-STRUCTURE packages
library(C50)
#----------------------------------------------------------
credit_model <- C5.0(credit_train[-17], credit_train$default)
#---------------------------------------------------------------
credit_model
summary(credit_model)
#---------------------------------------------------
credit_pred <- predict(credit_model, credit_test)
#---------------------------------------------------
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual default', 'predicted default'))
#--------------------------below is boosting------------------------------
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
#----------------------------------------------------------------------------
credit_boost10
credit_model
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = F, prop.c = F, prop.r = F, dnn = c('actual default', 'predicted default'))
credit_pred


mushrooms <- read.csv("D:\\workdir\\mushrooms.csv", stringsAsFactors = T)
str(mushrooms)
mushrooms$veil_type <- NULL
str(mushrooms)
table(mushrooms$type)
mushrooms$type <- factor(mushrooms$type, levels = c('e', 'p'), labels = c("edible", "poisonous"))
table(mushrooms$type)
str(mushrooms)
install.packages("RWeka")
library(RWeka)







