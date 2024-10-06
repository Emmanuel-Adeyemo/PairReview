

process_files = function(df1, df2, sort_by_column = 'label', compare_column = 'screened_abstracts'){

  `%nin%` = Negate(`%in%`)

  compare_column = tolower(compare_column)

  sort_by_column = tolower(sort_by_column)

  if(compare_column %nin% names(df1) | compare_column %nin% names(df2))stop(glue::glue('{compare_column} not present in one or both files'))

  if(sort_by_column %nin% names(df1) | sort_by_column %nin% names(df2))stop(glue::glue('{sort_by_column} not present in one or both files'))

  dta_one_sort = df1 %>% dplyr::arrange_at(dplyr::all_of(sort_by_column))
  dta_two_sort = df2 %>% dplyr::arrange_at(dplyr::all_of(sort_by_column))



  out = list(dta_one_sort = dta_one_sort, dta_two_sort = dta_two_sort)

  return(out)
}

#' both_selected extracts similar picks from both independent reviewers
#' @param df1 dataframe file from independent reviewer one
#' @param df2 dataframe file from independent reviewer two
#' @param sort_by_column unique column name present in both reviewer files to use for sorting. Default is 'label' from revtools.
#' @param compare_column column name present in both reviewer files for comparison.Default is 'screened_abstracts' from revtools.
#'
#'
#' @author Emmanuel Adeyemo, Kelechi Igwe
#'
#' @return dataframe with similar picks from both reviewers

#' @export
both_selected = function(df1, df2, sort_by_column = 'label', compare_column = 'screened_abstracts'){

  processed_dta = process_files(df1, df2, sort_by_column, compare_column)

  dta_one_sort = processed_dta$dta_one_sort
  dta_two_sort = processed_dta$dta_two_sort

  idx_selected = which(dta_one_sort[compare_column] == 'selected' & dta_two_sort[compare_column] == 'selected')
  final_selected = dta_one_sort[idx_selected,]

  return(final_selected)

}


#' @example if(F)selected = both_selected(dta_one, dta_two)
