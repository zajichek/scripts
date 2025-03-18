JNaiveBayes <- function(trainDir, testDir, parametric = FALSE, delta = 0.01) {
    if(delta >= 1 || delta <= 0) {
        stop("delta parameter must exclusively be between 0 and 1")
    }
    getProbs <- function(unNorm) {
        probs <- exp(.jevalArray(unNorm, simplify = TRUE))
        probs/sum(probs)
    }
    nbd <- .jnew("NaiveBayesDriver")
    model <- .jcall(nbd, "LModel;", "buildModel", trainDir, testDir, parametric, delta)
    results <- simplify2array(
                   lapply(
                       .jcall(nbd, "[[D", "probsArray", model), getProbs))
    labels <- .jcall(nbd, "[S", "labelsArray", model)
    testNames <- .jcall(nbd, "[S", "testNamesArray", model)
    labels <- gsub(".*/", "", labels)
    testNames <- gsub(".*/", "", testNames)
    row.names(results) <- labels
    colnames(results) <- testNames
    list("Probabilities" = t(results))
}
