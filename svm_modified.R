#setwd("C://svm regression")

#install.packages("e1071")
library(e1071)

train <- read.csv("cxf-2.1_Complete.csv")
test <- read.csv("cxf-2.2_Complete.csv")

model_prob <- svm(Buggy ~ ., data = test, scale=FALSE, probability=TRUE)


pred <- predict(model, test)
#y <- subset(train, select = Buggy)
#pred <- predict(model, na.omit=na.fail)
#pred <- predict(model, x)

pred_val <- as.numeric(pred > 0.5)
#TP <- sum(as.numeric(isTRUE(pred_val == test$Buggy)))
TP <- sum(as.numeric(pred_val & test$Buggy))
precision <- TP / sum(pred_val)
recall <- TP/sum(as.numeric(test$Buggy))

#####################################################################
########### Another approach
test$Buggy <- as.factor(test$Buggy)
model_response <- svm(Buggy ~ ., data = test, scale=FALSE)
pred_resp <- predict(model_response, test)
#TP <- sum(as.numeric(isTRUE(pred_val == test$Buggy)))
TP_resp <- sum(as.numeric(pred_resp) & as.numeric(test$Buggy))
precision_resp <- TP_resp / sum(as.numeric(pred_resp))
recall_resp <- TP_resp/sum(as.numeric(test$Buggy))



