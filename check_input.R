# For the Kaplan-Meier code, the data need to be formatted in a data.frame The
# columns need to hold (1) the measured concentration [numeric], (2) a detection
# flag [numeric/factor/logical/integer], and (3) a site identifier
# [factor/character/integer/numeric]
check_input <- function(censored_data){
  stopifnot(is.data.frame(censored_data),
            class(censored_data[,1]) == 'numeric',
            class(censored_data[,2]) %in% c('numeric','factor','logical','integer'),
            class(censored_data[,3]) %in% c('factor', 'character','integer','numeric'))
}
