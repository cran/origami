## ------------------------------------------------------------------------
data(mtcars)
head(mtcars)

## ------------------------------------------------------------------------
mod <- lm(mpg ~ ., data = mtcars)
summary(mod)

## ------------------------------------------------------------------------
err <- mean(resid(mod)^2)

## ------------------------------------------------------------------------
cvlm <- function(fold) {
    train_data <- training(mtcars)
    valid_data <- validation(mtcars)

    mod <- lm(mpg ~ ., data = train_data)
    preds <- predict(mod, newdata = valid_data)
    list(coef = data.frame(t(coef(mod))), SE = ((preds - valid_data$mpg)^2))
}

## ------------------------------------------------------------------------
library(origami)

## ------------------------------------------------------------------------
resub <- make_folds(mtcars, fold_fun = folds_resubstitution)[[1]]
resub_results <- cvlm(resub)
mean(resub_results$SE)

## ------------------------------------------------------------------------
# cross-validated estimate
folds <- make_folds(mtcars)
results <- cross_validate(cvlm, folds)
mean(results$SE)

## ----rf_cvfun------------------------------------------------------------
cvrf <- function(fold) {
    train_data <- training(mtcars)
    valid_data <- validation(mtcars)

    mod <- randomForest(formula = mpg ~ ., data = train_data)
    preds <- predict(mod, newdata = valid_data)
    list(coef = data.frame(mod$coefs), SE = mod$mse)
}

## ------------------------------------------------------------------------
suppressMessages(library(randomForest))
folds <- make_folds(mtcars)
results <- cross_validate(cvrf, folds)
mean(results$SE)

## ------------------------------------------------------------------------
data(AirPassengers)
print(AirPassengers)

## ------------------------------------------------------------------------
library(forecast)
folds = make_folds(AirPassengers, fold_fun=folds_rolling_origin,
                   first_window = 36, validation_size = 24)
fold = folds[[1]]

# function to calculate cross-validated squared error
cvforecasts <- function(fold) {
  train_data <- training(AirPassengers)
  valid_data <- validation(AirPassengers)
  valid_size <- length(valid_data)

  train_ts <- ts(log10(train_data), frequency = 12)

  # borrowed from AirPassengers help
  arima_fit <- arima(train_ts, c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12))
  raw_arima_pred <- predict(arima_fit, n.ahead = valid_size)
  arima_pred <- 10^raw_arima_pred$pred
  arima_MSE <- mean((arima_pred-valid_data)^2)

  # stl model
  stl_fit <- stlm(train_ts, s.window = 12)
  raw_stl_pred=forecast(stl_fit, h = valid_size)
  stl_pred <- 10^raw_stl_pred$mean
  stl_MSE <- mean((stl_pred-valid_data)^2)

  list(mse = data.frame(fold = fold_index(), arima = arima_MSE, stl = stl_MSE))
}

mses = cross_validate(cvforecasts, folds)$mse
colMeans(mses[, c("arima", "stl")])

## ----sessionInfo, echo=FALSE---------------------------------------------
sessionInfo()

