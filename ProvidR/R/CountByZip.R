#'Count by zip
#'
#'This is a helper function that allows you to create a count of providers by zipcode
#'#' @param data
countbyzip <- function(data){data %>%
    group_by(zipcode) %>% 
    count() %>%
    arrange(n) 
}