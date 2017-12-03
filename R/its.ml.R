#' @title massits machine learning functions
#' @name its.ml.model.svm_radial
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  SVM model. This function is a wraper to the \code{\link{svm}}
#'               function of \code{e1071} package.
#'               If no massits features tibble is informed, an enclosed function is returned.
#' @param f             A massits features tibble to compose the formula expression.
#' @param formula      A valid massits formula to be used in classification model
#' @param gamma        Radial kernel parameter (\eqn{e^{(-gamma*|u-v|^2)}}{exp(-gamma*|u-v|^2)})
#' @param cost         Cost of constraint violation
#' @return SVM Model
#' @export
its.ml.model.svm_radial <- function(f = NULL, formula = its.formula.linear(),
                                    gamma = function(f) {1 / its.feat.length(f)}, cost = 1){

    formula <- substitute(formula)
    gamma <- substitute(gamma)
    cost <- substitute(cost)

    result <-
        .its.factory(f, function(f){
            its.feat.valid(f, "its.ml.model.svm_radial - invalid input data")

            model <-
                e1071::svm(formula = eval(formula)(f), data = f,
                           kernel = "radial", gamma = eval(gamma)(f),
                           cost = eval(cost), probability = TRUE)

            attr(model, "levels") <- model$levels
            attr(model, "library") <- "e1071"

            return(model)
        })

    return(result)
}

#' @title massits features functions
#' @name its.ml.create_predict
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  This function returns a predict function to be applied on data to be classified.
#'               Besides the prediction, this function can return a information regarding the
#'               classification quality. The returned information, defined in \code{summation}
#'               parameter can be \code{"probabilities"}, the reliability of each reference
#'               label; \code{"entropy"}, the Shanon entropy given by
#'               \eqn{e=-\sum(p_i\log{p_i})}{e = -sum(p * log p)}; \code{"rentropy"}, the relative
#'               Shanon entropy, given by \eqn{re = \frac{e}{\log{k}}}{re = e / (log k)};
#'               and \code{"none"}, nothing is returned.
#'
#' @param f             A massits features tibble to compose the formula expression.
#' @param ml_model      A valid machine learning model function
#' @param summation     String value indicating a summation regarding the prediction
#' @return A prediction function that receives as input a massits features tibble
#' @export
its.ml.create_predict <- function(f, ml_model = its.ml.model.svm_radial(),
                                  summation = c("probabilities", "entropy", "rentropy", "none")){
    model <- ml_model(f)

    its.ml.predict <- function(f, cores = 1){
        its.feat.valid(f, "its.ml.predict - invalid input data")

        attrs <- attributes(f)[its.attrs]
        attrs$levels <- attr(model, "levels")

        eval(parse(text = sprintf("require(%s, quietly = TRUE, warn.conflicts = FALSE)",
                                  attr(model, "library"))))

        result <- f %>%
            dplyr::select(its.feat.cols)

        do_predict <- function(i = 1){
            partition <-
                if (cores > 1) cut(1:NROW(f), cores, labels = FALSE) else rep(1, NROW(f))
            part_predicted <-
                stats::predict(model,
                               newdata = f[partition == i, -which(names(f) %in% its.feat.cols)],
                               probability = (summation[[1]] != "none"))

            result <- tibble::tibble(predicted = part_predicted)
            result <- list(result,
                           attr(part_predicted, "probabilities") %>%
                               tibble::as_tibble()) %>%
                dplyr::bind_cols()
            return(result)
        }

        predicted.tb <-
            if (cores > 1){
                parallel::mclapply(seq_len(cores), do_predict, mc.cores = cores) %>%
                dplyr::bind_rows()
            } else {
                do_predict()
            }

        result$predicted <- predicted.tb$predicted
        result <-
            result %>%
            dplyr::mutate_all(function(x) factor(x, levels = attr(model, "levels")))

        if (summation[1] != "none"){
            probs.tb <-
                predicted.tb %>%
                dplyr::select(-predicted)

            if (summation[1] %in% c("entropy", "rentropy")){
                probs.tb <-
                    probs.tb %>%
                    dplyr::mutate_all(function(x) ifelse(x > 0, x * log(x), 0))

                probs.tb <-
                    if (summation[1] %in% c("entropy")){
                        probs.tb %>%
                            dplyr::mutate(entropy = -rowSums(.))
                    } else {
                        probs.tb <-
                            probs.tb %>%
                            dplyr::mutate(rentropy = -rowSums(.) / log(NCOL(probs.tb)))
                    }
                result <-
                    dplyr::bind_cols(list(result, probs.tb[summation[1]]))

            } else {
                result <-
                    dplyr::bind_cols(list(result, probs.tb))
            }
        }

        result <-
            result %>%
            .its.pred.stamp(attrs)

        return(result)
    }

    return(its.ml.predict)
}

#' @title massits machine learning functions
#' @name its.ml.cross_validation
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Proceeds a cross validation for a given machine learning method.
#'               The number of partitions is given by \code{cross} parameter.
#'               The result is a tibble with a "reference" and "predicted" columns
#' @param f             A massits features tibble to compose the formula expression.
#' @param ml_model      A valid machine learning model function
#' @param cross         Cross validation partitions to be test for each parameter combinations
#' @param cores         Number of threads to process cross-validation (Default \code{1}).
#' @return A confusion matrix returned by \code{\link{confusionMatrix}} of \code{caret} package
#' @export
its.ml.cross_validation <- function(f, ml_model = its.ml.model.svm_radial(), cross = 5, cores = 1){

    its.fold <- its.feat.create_folds(f, cross = cross)

    folds.tb <-
        parallel::mclapply(seq_len(cross), function(i){
            f_train <- its.fold(-i)
            f_test <- its.fold(i)
            its.ml.predict <- its.ml.create_predict(f_train, ml_model = ml_model, summation = "none")
            predict.tb <- its.ml.predict(f_test)

            result <- predict.tb %>%
                dplyr::select(reference, predicted)

            return(result)
        }, mc.cores = cores) %>%
        dplyr::bind_rows()

    result <- caret::confusionMatrix(folds.tb$predicted, folds.tb$reference)

    return(result)
}

#' @title massits machine learning functions
#' @name its.tune
#' @author Rolf Simoes, \email{rolf.simoes@@inpe.br}
#' @description  Proceeds a tunning of parameters for a given machine learning method
#' @param f             A massits features tibble to compose the formula expression.
#' @param ml_model      A valid machine learning method function
#' @param range         A named list to be expanded that is passed as arguments to method function
#' @param cross         Cross validation partitions to be test for each parameter combinations
#' @return Tibble of parameters and accuracy
#' @export
its.tune <- function(f, ml_model = its.ml.model.svm_radial(), range, cross = 5){
    envir <- environment(ml_model)
    params <- expand.grid(range)

    if (!all(names(params) %in% ls(envir)))
        stop("its.tune - not all range arguments are valid")

    params$accuracy <-
        seq_len(NROW(params)) %>%
        purrr::map(function(i){
            for(p in names(params)){
                envir[[p]] <<- params[[p]][i]
            }
            result <-
                its.ml.cross_validation(f, ml_model = ml_model, cross = cross)
            result <-
                result$overall[["Accuracy"]]

            return(result)
        }) %>%
        unlist()

    return(params)
}
