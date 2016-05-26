context("Imputation")
data(bank, package = "flipData")

dat <- data.frame(a = rep((1:10)/10,2),
                  b = rep(1:10,2),
                  c = factor(rep(c(rep("A",5),rep("B",5)),2)),
                  d = ordered(rep(c(rep("A",5),rep("B",5)),2)), e = rep("dog",20), stringsAsFactors = FALSE)
for (i in 1:5)
    dat[i, i] <- NA


test_that("Imputation algorithms",
{
    # Default
    si <- Imputation(dat)[[1]]
    expect_equal(nrow(si), 20)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    si <- Imputation(dat, ~ a + b + c + d)[[1]]
    expect_equal(nrow(si), 20)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    si <- Imputation(dat, a  ~ b + c + d)[[1]]
    expect_equal(nrow(si), 19)
    expect_equal(attr(si, "imputation.method"), "hot decking")

    # mice errors
    expect_error(Imputation(dat, method = "mice"), "Mice imputation failed.")
    expect_error(Imputation(dat,  ~ a + b + c + d, method = "mice"), "Mice imputation failed.")

    #choice of algorithm.
    si <- Imputation(bank, method = "mice")[[1]]
    expect_equal(attr(si, "imputation.method"), "chained equations (predictive mean matching)")

    si <- Imputation(bank, method = "hot deck")[[1]]
    expect_equal(attr(si, "imputation.method"), "hot decking")
})
