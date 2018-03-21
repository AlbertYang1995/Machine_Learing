letters <- read.csv("D:\\workdir\\letterdata.csv")
str(letters)
letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]
View(letters)
install.packages("kernlab")
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
str(letter_predictions)
str(letters_test$letter)
agreement <- letter_predictions == letters_test$letter    #useful
table(agreement)
prop.table(table(agreement))
letters_test <- letters_test[-1]
str(letters_test)                                            # use the kernel function
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
prop.table(table(agreement_rbf))
