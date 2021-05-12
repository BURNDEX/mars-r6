MARSEnsemble <- R6::R6Class(
    "MARSEnsemble",
    public = list(
        initialize = function(list_of_models) {
            private$.models <- list_of_models
        },
        predict = function(new_data) {
            truth <- new_data[["burn_index"]]
            predictions <- foreach::foreach(
                index = seq_len(length(private$.models)),
                .combine   = dplyr::bind_cols
            ) %dopar% {
                pred <- predict(private$.models[[index]], new_data = new_data)

                dplyr::rename(
                    pred,
                    !!names(private$.models)[index] := .pred
                )
            }

            predictions %>%
                dplyr::mutate(
                    .pred = rowMeans(.)
                ) %>%
                dplyr::select(.pred)
        },
        augment = function(new_data) {
            preds <- self$predict(new_data = new_data)

            dplyr::bind_cols(new_data, preds)
        }
    ),
    private = list(
        .models = NULL
    )
)
