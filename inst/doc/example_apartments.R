## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## -----------------------------------------------------------------------------
library(rSAFE)
head(apartments)

## -----------------------------------------------------------------------------
library(randomForest)
set.seed(111)
model_rf1 <- randomForest(m2.price ~ construction.year + surface + floor + no.rooms + district, data = apartments)

## -----------------------------------------------------------------------------
library(DALEX)
explainer_rf1 <- explain(model_rf1, data = apartmentsTest[1:3000,2:6], y = apartmentsTest[1:3000,1], label = "rf1", verbose = FALSE)
explainer_rf1

## -----------------------------------------------------------------------------
safe_extractor <- safe_extraction(explainer_rf1, penalty = 25, verbose = FALSE)

## -----------------------------------------------------------------------------
print(safe_extractor)

## ---- fig.width=7-------------------------------------------------------------
plot(safe_extractor, variable = "construction.year")

## ---- fig.width=7-------------------------------------------------------------
plot(safe_extractor, variable = "district")

## -----------------------------------------------------------------------------
data1 <- safely_transform_data(safe_extractor, apartmentsTest[3001:6000,], verbose = FALSE)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(data1))

## ---- fig.width=6-------------------------------------------------------------
vars <- safely_select_variables(safe_extractor, data1, which_y = "m2.price", verbose = FALSE)
data1 <- data1[,c("m2.price", vars)]
print(vars)

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(head(data1))

## ---- fig.width=6-------------------------------------------------------------
data2 <- safely_transform_data(safe_extractor, apartmentsTest[6001:9000,], verbose = FALSE)[,c("m2.price", vars)]

## -----------------------------------------------------------------------------
model_lm2 <- lm(m2.price ~ ., data = data1)
explainer_lm2 <- explain(model_lm2, data = data2, y = apartmentsTest[6001:9000,1], label = "lm2", verbose = FALSE)
set.seed(111)
model_rf2 <- randomForest(m2.price ~ ., data = data1)
explainer_rf2 <- explain(model_rf2, data2, apartmentsTest[6001:9000,1], label = "rf2", verbose = FALSE)

## -----------------------------------------------------------------------------
model_lm1 <- lm(m2.price ~ ., data = apartments)
explainer_lm1 <- explain(model_lm1, data = apartmentsTest[1:3000,2:6], y = apartmentsTest[1:3000,1], label = "lm1", verbose = FALSE)

## -----------------------------------------------------------------------------
mp_lm1 <- model_performance(explainer_lm1)
mp_rf1 <- model_performance(explainer_rf1)
mp_lm2 <- model_performance(explainer_lm2)
mp_rf2 <- model_performance(explainer_rf2)

## ---- fig.width=7, fig.height=6-----------------------------------------------
plot(mp_lm1, mp_rf1, mp_lm2, mp_rf2, geom = "boxplot")

