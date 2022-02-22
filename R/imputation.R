#' \code{Imputation}
#' @description This is a wrapper for \code{\link{mice}}.
#' @param data A \code{\link{data.frame}}.
#' @param formula A \code{\link{formula}}. Where the formula contains a dependent variable,
#' observations with missing values on this variable are deleted after the imputation (von Hippel 2007).
#' @param method "mice" applies multivariate imputation by chained equations
#' (predictive mean matching) with the \code{\link[mice]{mice}} package. "hot deck" applies the \code{\link[hot.deck:hot.deck]{hot.deck}} method.
#' The default setting is "try mice", which first applies the \code{\link[mice]{mice}} method and,if an error
#' occurs, falls back to \code{\link[hot.deck]{hot.deck}}.
#' @param m Number of imputation samples.
#' @param seed Seed used in random number generation.
#' @references von Hippel, Paul T. 2007. "Regression With Missing Y's: An
#' Improved Strategy for Analyzing Multiply Imputed Data." Sociological Methodology 37:83-117.
#' Skyler J. Cranmer and Jeff Gill (2013). We Have to Be Discrete About This: A Non-Parametric Imputation Technique for Missing Categorical Data. British Journal of Political Science, 43, pp 425-449.
#' Stef van Buuren and Karin Groothuis-Oudshoorn (2011), "mice: Multivariate
#' Imputation by Chained Equations in R", Journal of Statistical Software, 45:3, 1-67.
#' @importFrom mice mice complete
#' @importFrom flipU OutcomeName CopyAttributes AnyNA AllVariablesNames
#' @importFrom hot.deck hot.deck
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
        return(lapply(seq(m), function(x) data))

    single.var <- NCOL(data) == 1L

    outcome.name <- if (is.null(formula)) NULL else OutcomeName(formula)
    if (!is.null(outcome.name))
    {
        temp.data <- data[, AllVariablesNames(formula, data = data)]
        temp.data <- data[!is.na(temp.data[, outcome.name]), , drop = FALSE]
        if(!any(is.na(temp.data)))
        {
            warning("Imputation has been selected, but the data has no missing values in the predictors, so nothing has been imputed.")
            temp.data <- CopyAttributes(temp.data, data)
            return(lapply(seq(m), function(x) temp.data))
        }
    }
    if(method != "Hot deck")
    {
        # CE-437: Integer and numeric columns created by calling unclass() on factors contain levels,
        # which confuses mice and causes it to crash (and display cryptic warnings).
        data <- removeLevelsFromNumericData(data)

        dat.colnames <- NULL
        imputed.data <- suppressWarnings(try(
            {
                # Require is used instead of Depends because using Depends
                # implies that all downstream packages must also use Depends.
                # This can substantially increase the load time
                # as well as risk of conflicting names when the hierachy of
                # dependencies is very deep
                require("mice")
                set.seed(seed)
                dat.colnames <- colnames(data)
                colnames(data) <- paste0("A", 1:ncol(data)) # need to replace names to avoid errors in mice v3.0.0
                mice.setup <- if (single.var)
                                  mice(cbind(data, jrhtr46__ = 1), m = m, seed = seed,
                                        method = c("sample", ""), printFlag = FALSE)
                              else mice(data, m = m, seed = seed, printFlag = FALSE)
                data.sets <- vector("list", m)
                for (i in 1:m)
                {
                    dat.i <- if (single.var)
                                 CopyAttributes(complete(mice.setup,
                                                         action = i)[, 1, drop = FALSE], data)
                             else
                                 CopyAttributes(complete(mice.setup, action = i), data,
                                       attr.to.not.copy = NULL)
                    attr(dat.i, "imputation.method") <- "chained equations (predictive mean matching)"
                    colnames(dat.i) <- dat.colnames
                    data.sets[[i]] <- dat.i
                }
                colnames(data) <- dat.colnames
                data.sets
            }
        , silent = TRUE))
        if (method == "mice" && .errorInImputation(imputed.data, formula))
            stop("Mice imputation failed.")
    }
    if (method != "mice" && (method == "hot deck" || .errorInImputation(imputed.data, formula)))
    {
        set.seed(seed)
        if (!is.null(dat.colnames))
            colnames(data) <- dat.colnames
        imputed.data <- if (single.var)
                            suppressWarnings(hot.deck(cbind(data, xytr745___ = 1L),
                                                      m = m)$data)
                        else
                            suppressWarnings(hot.deck(data, m = m)$data)
        for (i in 1:m)
        {
            attr(imputed.data[[i]], "imputation.method") <- "hot decking"
            if (single.var)
                imputed.data[[i]] <- imputed.data[[i]][, 1, drop = FALSE]
        }
    }
    if (.errorInImputation(imputed.data, formula))
        stop("Imputation has failed.")
    if (!is.null(formula))
    {
        if (!is.null(outcome.name))
        {
            for (i in 1:m)
            {
                valid.dependent <- !is.na(data[, outcome.name])
                imp.data <- imputed.data[[i]][valid.dependent, ] # Excluding observations with missing values.
                imp.data <- CopyAttributes(imp.data, imputed.data[[i]]) # Copying labels
                attr(imp.data, "imputation.method") <- attr(imputed.data[[i]], "imputation.method") # Data file attributes
                imputed.data[[i]] <- imp.data
            }
        }
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
