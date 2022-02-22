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
#' @details Variables with class \code{"POSIXct"} are converted to numeric and
#' scaled prior to imputation. Variables with class \code{"character"} are converted
#' to factor prior to imputation with blank (0-character) entries considered missing.
#' @importFrom mice mice complete
#' @importFrom flipU OutcomeName CopyAttributes AnyNA AllVariablesNames
#' @importFrom hot.deck hot.deck
#' @export
Imputation <- function(data = NULL, formula = NULL, method = "try mice", m = 1, seed = 12321)
{
    method <- tolower(method)
    .errorInImputation <- function(imputed.data, formula)
    {
        if (any("try-error" %in% class(imputed.data)))
            return(TRUE)
        for (i.data in imputed.data){
            if(anyNA(i.data))
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

    pdata <- preProcessData(data)
    hot.deck.used <- FALSE
    if(method != "hot deck")
    {
        imputed.data <- suppressWarnings(try(
            {
                # Require is used instead of Depends because using Depends
                # implies that all downstream packages must also use Depends.
                # This can substantially increase the load time
                # as well as risk of conflicting names when the hierachy of
                # dependencies is very deep
                require("mice")
                set.seed(seed)
                ## dat.colnames <- colnames(data)
                ## colnames(data) <- paste0("A", 1:ncol(data)) # need to replace names to avoid errors in mice v3.0.0
                mice.method <- if (single.var) c("sample", "")  # else NULL
                mice.setup <- mice(pdata, m = m, seed = seed, printFlag = FALSE,
                                   method = mice.method)
                data.sets <- vector("list", m)
                for (i in 1:m)
                    data.sets[[i]] <- complete(mice.setup, action = i)
                data.sets
            }
        , silent = TRUE))
        if (method == "mice" && .errorInImputation(imputed.data, formula))
            stop("Mice imputation failed.")
    }
    if (method != "mice" && (method == "hot deck" || .errorInImputation(imputed.data, formula)))
    {
        set.seed(seed)
        imputed.data <- suppressWarnings(hot.deck(pdata, m = m)$data)
        all.na.rows <- nrow(imputed.data[[1]]) != nrow(data)
        hot.deck.used <- TRUE
    }
    if (.errorInImputation(imputed.data, formula))
        stop("Imputation has failed.")

    imputation.method <- ifelse(hot.deck.used, "hot decking",
                                "chained equations (predictive mean matching)")
    for (i in 1:m)
    {
        imputed.data[[i]] <- postProcessData(pdata, imputed.data[[i]], data)
        attr(imputed.data[[i]], "imputation.method") <- imputation.method
    }
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

#' 1) CE-437: Drops levels from integer and numeric columns created by
#' calling unclass() on factors contain levels, which confuses mice
#' and causes it to crash (and display cryptic warnings).
#' 2) Converts date variables to numeric
#' 3) relabels factors with duplicate levels so they are unique
#' 4) Converts text variables to factors with blank entries converted to NA
#' @noRd
preProcessData <- function(data)
{
    orig.classes <- lapply(data, class)
    ## orig.attrs <- lapply(data, attributes)
    for (nms in names(data))
    {
        clss <- class(data[[nms]])
        if (any(c("integer", "numeric") %in% clss))
            attr(data[[nms]], "levels") <- NULL
        else if ("character" %in% clss)
            data[[nms]] <- factor(data[[nms]], exclude = c("", NA))
        else if ("factor" %in% clss)
            levels(data[[nms]]) <- make.unique(levels(data[[nms]]))
        else if (inherits(data[[nms]], c("QDate", "POSIXlt", "POSIXct")))
            data[[nms]] <- as.numeric(data[[nms]])/1e9
    }
    ## add dummy extra variable to allow mice/hotdeck to run with a single variable
    if (NCOL(data) == 1L)
        data <- cbind(data, DUMMY__VAR__ = 1)
    ## need to replace names to avoid errors in mice v3.0.0
    attr(data, "orig.names") <- colnames(data)
    colnames(data) <- paste0("A", 1:ncol(data))
    return(data)
}

#' Converts text and date variables back to their original types
#' Converts de-duplicated factor levels back to their original state
#' @noRd
postProcessData <- function(preprocessed.data, imputed.data, orig.data)
{
    ## orig.classes <- preprocessed.data[["orig.classes"]]
    ## orig.attrs <- preprocessed.data[["orig.attrs"]]
    colnames(imputed.data) <- colnames(orig.data)
    orig.classes <- lapply(orig.data, class)
    if (NCOL(orig.data) == 1)  # drop dummy variable needed to run mice with 1 var.
        imputed.data <- imputed.data[, 1, drop = FALSE]
    if (nrow(imputed.data) != nrow(orig.data))
        imputed.data <- imputeAllMissingRows(imputed.data, preprocessed.data)
    for (nms in colnames(imputed.data))
    {
        orig.class <- orig.classes[[nms]]
        if ("character" %in% orig.class)
            imputed.data[[nms]] <- as.character(imputed.data[[nms]])
        else if ("factor" %in% orig.class)  # preserve duplicate levels to match Displayr
            imputed.data[[nms]] <- structure(as.integer(imputed.data[[nms]]),
                                             .Label = levels(orig.data[[nms]]),
                                             class = class(orig.data[[nms]]))
        else if (any(orig.class %in% c("QDate", "POSIXct")))
        {
            imputed.data[[nms]] <- as.POSIXct(1e9*imputed.data[[nms]],
                                             origin = "1970-01-01")
            class(imputed.data[[nms]]) <- c(class(imputed.data[[nms]]), "QDate")
        }
    }
    imputed.data <- CopyAttributes(imputed.data, orig.data,
                                   attr.to.not.copy = c("dimnames", "names", "dim", "class", "levels"))
    ## colnames(imputed.data) <- attr(preprocessed.data, "orig.names")
    return(imputed.data)
}

#' The hot deck method drops rows from the data that are entirely missing. This
#' function uses mice to impute values for rows with entirely missing data by
#' randomly sampling the observed values
imputeAllMissingRows <- function(imputed.data, processed.data, seed = 585)
{
    out.dat <- processed.data
    all.na.rows <- which(apply(processed.data, 1, function(x) all(is.na(x))))
    out.dat[-all.na.rows, ] <- imputed.data
    mice.method <- rep("sample", ncol(out.dat))
    try(
    {
        mice.out <- mice(out.dat, m = 1, seed = seed, printFlag = FALSE,
                         method = mice.method)
        out.dat <- complete(mice.out, action = 1)
    }, TRUE)
    return(out.dat)
}
