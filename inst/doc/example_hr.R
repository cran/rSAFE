## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)
library(pander)

## -----------------------------------------------------------------------------
library(rSAFE)
head(HR_data)

## -----------------------------------------------------------------------------
data <- HR_data[, colnames(HR_data) != "sales"]

## -----------------------------------------------------------------------------
set.seed(111)
data <- data[sample(1:nrow(data)),]

data1 <- data[1:4000,]
data2 <- data[4001:8000,]
data3 <- data[8001:12000,]
data4 <- data[12001:14999,]

## -----------------------------------------------------------------------------
n.trees <- 10000
library(gbm)
set.seed(111)
model_xgb1 <- gbm(left ~ ., data = data1, distribution = "bernoulli", n.trees = n.trees)

## -----------------------------------------------------------------------------
library(DALEX)
predict_function <- function(model, x) predict(model, x, type = "response", n.trees = n.trees)
explainer_xgb1 <- explain(model_xgb1, data = data2[,-7], y = data2$left, predict_function = predict_function, verbose = FALSE)

## -----------------------------------------------------------------------------
safe_extractor <- safe_extraction(explainer_xgb1, penalty = 20, verbose = FALSE)

## -----------------------------------------------------------------------------
print(safe_extractor)

## ---- fig.width=7-------------------------------------------------------------
plot(safe_extractor, variable = "last_evaluation")

## ---- fig.width=7-------------------------------------------------------------
plot(safe_extractor, variable = "salary")

## -----------------------------------------------------------------------------
data3_trans <- safely_transform_data(safe_extractor, data3, verbose = FALSE)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(data3_trans))

## ---- fig.width=6-------------------------------------------------------------
selected_variables <- safely_select_variables(safe_extractor, data3_trans, which_y = "left", verbose = FALSE)
data3_trans_sel <- data3_trans[,c("left", selected_variables)]
print(selected_variables)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(data3_trans_sel))

## ---- fig.width=6-------------------------------------------------------------
data4_trans <- safely_transform_data(safe_extractor, data4, verbose = FALSE)
data4_trans_sel <- data4_trans[,c("left", selected_variables)]

## -----------------------------------------------------------------------------
model_lr2 <- glm(left ~ ., data = data3_trans_sel, family = binomial())
set.seed(111)
model_xgb2 <- gbm(left ~ ., data = data3_trans_sel, distribution = "bernoulli", n.trees = n.trees)

## -----------------------------------------------------------------------------
model_lr1 <- glm(left ~ ., data = data1, family = binomial())

## -----------------------------------------------------------------------------
pred_lr1 <- round(predict(model_lr1, data2, type = "response"))
pred_xgb1 <- round(predict(model_xgb1, data2, n.trees = n.trees, type = "response"))
pred_lr2 <- round(predict(model_lr2, data4_trans_sel, type = "response"))
pred_xgb2 <- round(predict(model_xgb2, data4_trans_sel, n.trees = n.trees, type = "response"))

## -----------------------------------------------------------------------------
confusion_matrix <- function(y_true, y_pred) {
  cm <- data.frame(pred_0 = c(sum(y_true==0 & y_pred==0)/sum(y_true==0),
                              sum(y_true==1 & y_pred==0)/sum(y_true==1)),
                   pred_1 = c(sum(y_true==0 & y_pred==1)/sum(y_true==0),
                              sum(y_true==1 & y_pred==1)/sum(y_true==1)))
  cm <- apply(cm, MARGIN = 2, function(x) round(x, 2))
  rownames(cm) <- c("actual_0", "actual_1")
  cm
}

## ---- echo=FALSE--------------------------------------------------------------
A <- matrix(c(0.97, 0.03, 0.09, 0.91),
            nrow=2, byrow = TRUE)
rownames(A) <- pandoc.strong.return(c('actual 0', 'actual 1'))
colnames(A) <- pandoc.strong.return(c('predicted 0', 'predicted 1'))
pander(A)

## ---- echo=FALSE--------------------------------------------------------------
A <- matrix(c(0.93, 0.07, 0.27, 0.73),
            nrow=2, byrow = TRUE)
rownames(A) <- pandoc.strong.return(c('actual 0', 'actual 1'))
colnames(A) <- pandoc.strong.return(c('predicted 0', 'predicted 1'))
pander(A)

## ---- echo=FALSE--------------------------------------------------------------
A <- matrix(c(0.92, 0.08, 0.63, 0.37),
            nrow=2, byrow = TRUE)
rownames(A) <- pandoc.strong.return(c('actual 0', 'actual 1'))
colnames(A) <- pandoc.strong.return(c('predicted 0', 'predicted 1'))
pander(A)

## ---- echo=FALSE--------------------------------------------------------------
A <- matrix(c(0.93, 0.07, 0.27, 0.73),
            nrow=2, byrow = TRUE)
rownames(A) <- pandoc.strong.return(c('actual 0', 'actual 1'))
colnames(A) <- pandoc.strong.return(c('predicted 0', 'predicted 1'))
pander(A)

