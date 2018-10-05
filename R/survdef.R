# private vector of intake variable names
itvars = c("irt76", "irt78", "irt80", "irt82", "irt84", "irt86", "irt88", 
"irt90", "irt92", "irt94", "irt96", "irt98", "irt00", "irt02")

#' produce a query that generates fields life_months and death_observed
#' based on NHS irt76, age76, and deathmonth
#' @import survival bigrquery dplyr magrittr tibble
#' @param bqconn a bigrquery BigQueryConnection instance, or a tibble
#' @param tablename character(1) a table in the connection dataset
#' @param extras defaults to NULL; or a vector of character() variables to be
#' retrieved from the source table
#' @note The algorithm for computing life months is
#' '''
#' life_months = ifelse(is.na(deathmonth), (age76*12)+(maxintake-912),
#'      deathmonth+(age76*12)-912)  # 912 is 76 years, a temporal anchor for study beginning in 1976
#' '''
#' @return a tibble if input is a tibble
#' @examples
#' survdef(tibble::as.tibble(bq4nhs::df500), extras=c("smkdr80", "bmi80"))
#' @export
survdef = function(bqconn, tablename, extras = NULL) {
  if (is.tibble(bqconn)) curtab = bqconn
  else curtab = bqconn %>% tbl(tablename)
  varvec = c("deathmonth", "age76", extras)
  curtab = curtab %>% select(c("id", itvars, varvec)) %>% group_by(id) %>% 
      mutate(maxintake=pmax( irt76, irt78, irt80, irt82, irt84, irt86, irt88,
         irt90, irt92, irt94, irt96, irt98, irt00, irt02)) 
  curtab %>% 
       mutate(life_months = ifelse(is.na(deathmonth), (age76*12)+(maxintake-912), 
             deathmonth+(age76*12)-912), death_observed = !is.na(deathmonth))
}
