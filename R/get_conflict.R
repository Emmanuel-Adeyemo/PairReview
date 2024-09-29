#' get_conflict extract conficting picks from both independent reviewers
#' @param df1 dataframe file from independent reviewer one
#' @param df2 dataframe file from independent reviewer two
#' @param sort_by_column unique column name present in both reviewer files to use for sorting. Default is 'label' from revtools.
#' @param compare_column column name present in both reviewer files for comparison.Default is 'screened_abstracts' from revtools.
#'
#'
#' @author Emmanuel Adeyemo, Kelechi Igwe
#'
#' @return dataframe with conflictin picks from both reviewers


#' @export
get_conflict = function(df1, df2, sort_by_column = 'label', compare_column = 'screened_abstracts'){

  processed_dta = process_files(df1, df2, sort_by_column, compare_column)

  dta_one_sort = processed_dta$dta_one_sort
  dta_two_sort = processed_dta$dta_two_sort


  idx_conflict = which(dta_one_sort[compare_column] != dta_two_sort[compare_column])

  final_conflict = dta_one_sort[idx_conflict,]

  return(final_conflict)

}

#' @example if(F)conflict = get_conflict(dta_one, dta_two)


