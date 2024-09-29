
#' @import dplyr
#' @import janitor
#' @import readr
#'
#'



get_conflict = function(df1, df2, sort_by_column = 'label', compare_column = 'screened_abstracts'){

  processed_dta = process_files(df1, df2, sort_by_column, compare_column)

  dta_one_sort = processed_dta$dta_one_sort
  dta_two_sort = processed_dta$dta_two_sort


  idx_conflict = which(dta_one_sort[compare_column] != dta_two_sort[compare_column])

  final_conflict = dta_one_sort[idx_conflict,]

}



#' @example if(F)conflict = get_conflict(dta_one, dta_two)

