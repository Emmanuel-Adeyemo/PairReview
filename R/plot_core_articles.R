
#' plot_core_articles line plot of core articles
#' @param df_journal dataframe with similar or conflicted picks from both reviewers. Output from both_selected or get_conflict function.
#'
#' @author Emmanuel Adeyemo, Kelechi Igwe
#'
#' @return ggplot image
#'
#' @export

plot_core_articles = function(df_journal){

  journal_count = df_journal %>% dplyr::filter(!is.na(journal)) %>% dplyr::group_by(journal) %>% dplyr::tally() %>%
    dplyr::arrange(desc(n))

  journal_count$journal = factor(journal_count$journal, levels = (journal_count$journal))

  keep = floor(IQR(journal_count$journal))
  rem = length(journal_count$journal) - keep

  ggplot2::ggplot(journal_count, ggplot2::aes(x=journal, y=n, group = 1)) +
    ggplot2::geom_line() + ggplot2::ylab('Article count') + ggplot2::xlab('Journal name') +
    ggplot2::ggtitle('Core Articles') +
    #ggplot2::scale_x_discrete() +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(25),
                              breaks = levels(journal_count$journal)[c(rep(T, keep), rep(F, rem))],
                              expand = ggplot2::expansion(add = 1.2))+
    ggplot2::theme_classic() +
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggplot2::annotate("text",x = 1, y = 0.2, label = "", size = 5)

}





#' plot_core_by_year line plot of core articles by year
#' @param df_journal dataframe with similar or conflicted picks from both reviewers. Output from both_selected or get_conflict function.
#'
#' @author Emmanuel Adeyemo, Kelechi Igwe
#'
#' @return ggplot image
#'
#' @export


plot_core_by_year = function(df_journal){

  year_count = df_journal %>% dplyr::filter(!is.na(year)) %>% dplyr::group_by(year) %>% dplyr::tally() %>%
    dplyr::arrange(year)

  year_count$year = factor(year_count$year, levels = (year_count$year))


  ggplot2::ggplot(year_count, ggplot2::aes(x=year, y=n, group = 1)) +
    ggplot2::geom_line() + ggplot2::ylab('Article count') + ggplot2::xlab('Publication Year') +
    ggplot2::ggtitle('Articles by Year') + ggplot2::theme_classic()  +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
}


