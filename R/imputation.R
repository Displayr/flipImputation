#' \code{Imputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param data A \code{\link{data.frame}}.
#' @param formula A \code{\link{formula}}. Where the formula contains a dependent variable,
#' observations with missing values on this variable are deleted after the imputation (von Hippel 2007).
#' @param method "mice" applies multivariate imputation by chained equations
#' (predictive mean matching) with the \code{\link[mice]{mice}} package. "hot deck" applies the \code{\link[hot.deck, hot.deck.rdb]{hot.deck}} method.
#' The default setting is "try mice", which first applies the \code{\link[mice]{mice}} method and,if an error
#' occurs, falls back to \code{\link[hot.deck]{hot.deck}}.
#' @param m Number of imputation samples.
#' @param seed Seed used in random number generation.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' Skyler J. Cranmer and Jeff Gill (2013). We Have to Be Discrete About This: A Non-Parametric Imputation Technique for Missing Categorical Data. British Journal of Political Science, 43, pp 425-449.
#' Stef van Buuren and Karin Groothuis-Oudshoorn (2011), "mice: Multivariate
#' Imputation by Chained Equations in R", Journal of Statistical Software, 45:3, 1-67.
#' @import mice
#' @importFrom flipU OutcomeName CopyAttributes AnyNA
#' @import hot.deck
#' @export
Imputation <- function(data = NULL, formula = NULL, method = "try mice", m = 1, seed = 12321)
{
    .errorInImputation <- function(imputed.data, formula)
    {
        if (any("try-error" %in% class(imputed.data)))
            return(TRUE)
        for (i.data in imputed.data){
            if(AnyNA(i.data, formula))
                return(TRUE)
        }
        FALSE
    }
    if(!any(is.na(data)))
    {
        stop("Imputation has been selected, but the data has no missing values, so nothing has been imputed.")
        return(data)
    }
    if(method != "Hot deck")
    {
        # CE-437: Integer and numeric columns created by calling unclass() on factors contain levels,
        # which confuses mice and causes it to crash (and display cryptic warnings).
        data <- removeLevelsFromNumericData(data)

        imputed.data <- suppressWarnings(try(
            {
                require("mice")
                set.seed(seed)
                mice.setup <- mice(data, m = m, seed = seed, printFlag = FALSE)
                data.sets <- vector("list", m)
                for (i in 1:m)
                {
                    data.sets[[i]] = complete(mice.setup, action = i)
                    attr(data.sets[[i]], "imputation.method") <- "chained equations (predictive mean matching)"
                }
                data.sets
            }
        , silent = TRUE))
        if (method == "mice" && .errorInImputation(imputed.data, formula))
            stop("Mice imputation failed.")
    }
    if(method != "mice" && (method == "hot deck" || .errorInImputation(imputed.data, formula)))
    {
        set.seed(seed)
        imputed.data <- suppressWarnings(hot.deck(data, m = m)$data)
        for (i in 1:m)
            attr(imputed.data[[i]], "imputation.method") <- "hot decking"
    }
    if (.errorInImputation(imputed.data, formula))
        stop("Imputation has failed.")
    if (!is.null(formula))
    {
        outcome.name <- OutcomeName(formula)
        if (!is.null(outcome.name))
        {
            for (i in 1:m)
            {
                valid.dependent <- !is.na(data[, outcome.name])
                imputed.data[[i]] <- imputed.data[[i]][valid.dependent, ] # Excluding observations with missing values.
            }
        }
        imputed.data <- CopyAttributes(data, imputed.data)
    }
    imputed.data
}

removeLevelsFromNumericData <- function(data)
{
    for (nms in names(data))
    {
        clss <- class(data[[nms]])
        if (clss == "integer" ||  clss == "numeric")
            attr(data[[nms]], "levels") <- NULL
    }
    data
}
