library(JNaiveBayes)
library(testthat)

train <- system.file("Data", "Training", package = "JNaiveBayes")
test <- system.file("Data", "Testing", package = "JNaiveBayes")

JNaiveBayes(train, test, FALSE, 0.01)
JNaiveBayes(train, test, TRUE, 0.01)

expect_error(JNaiveBayes("Train", test, FALSE, 0.01))
expect_error(JNaiveBayes(train, "test", TRUE, 0.01))
expect_error(JNaiveBayes(train, test, 0, 0.01))
expect_error(JNaiveBayes(train, test, 0, 1))
