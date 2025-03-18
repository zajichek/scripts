\name{JNaiveBayes}
\alias{JNaiveBayes}
\title{Classify testing documents into categories based on a set of training documents}
\description{
  This function calls a collection of Java classes which collectively implement a Naive Bayes text classifier. The user will simultaneously 
    provide a set of training and testing documents via directory paths to build the model then get predictions. A parametric 
    approach can be specified, but nonparametric is implemented by default.
}
\usage{
  JNaiveBayes(trainDir, testDir, parametric = FALSE, delta = 0.01)
}
\arguments{
  \item{trainDir}{Path to a directory containing subdirectories that contain training documents. Assumes each subdirectory
                   contains only and all training documents for that category. See details for specifications of document format.}
  \item{testDir}{Path to a directory containing all testing documents to be classified. See details for specifications
                   of document format.}
  \item{parametric}{Defaults to false. If true, all probabilities are modeled by a Poisson distribution. If kept to false, 
                    probabilities are calculated by various proportions of word counts. See details for more information.}
  \item{delta}{Defaults to 0.01. This tuning parameter allows valid probabilities to be calculated for unobserved words. Must be between 0 and 1 exclusive.}
}
\details{
  See vignette for model specification.
}
\value{                  
  \item{Probabilities}{The probability that the shown test document belongs to that class}
}
\author{Tyler Olson and Alex Zajichek}
\examples{
    train <- system.file("Data", "Training", package = "JNaiveBayes")
    test <- system.file("Data", "Testing", package = "JNaiveBayes")
    JNaiveBayes(train, test, FALSE, 0.01)
}
\keyword{Java, Naive Bayes, text classification, Poisson}
