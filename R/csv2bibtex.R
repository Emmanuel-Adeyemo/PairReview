
# Function to clean strings
clean_string = function(x) {
  gsub("[{}]", "", x)
}



# Function to convert a row to BibTeX format

row_to_bibtex = function(row) {

  author_key = if (!is.na(row$author)) {

    tail(strsplit(row$author, ",")[[1]][1], 1)

  } else {

    "Unknown"
  }

  # Create BibTeX entry
  bibtex_entry = sprintf("@article{%s_%d,\n", author_key, as.integer(row$year))

  fields = c("author", "title", "journal", "doi", "url", "abstract")

  for (field in fields) {

    if (!is.na(row[[field]])) {

      bibtex_entry = paste0(bibtex_entry, sprintf("  %s = {%s},\n", field, clean_string(row[[field]])))
    }
  }

  # Handle volume separately with error handling
  if (!is.na(row$volume)) {

    volume = suppressWarnings(as.integer(unlist(strsplit(as.character(row$volume), "-"))[1]))

    if (!is.na(volume)) {

      bibtex_entry = paste0(bibtex_entry, sprintf("  volume = {%d},\n", volume))
    }
  }

  bibtex_entry = sub(",\n$", "\n", paste0(bibtex_entry, sprintf("  year = {%d},\n", as.integer(row$year))))
  paste0(bibtex_entry, "}")

}



#' csv2bibtex Function to convert a csv dataframe to BibTeX format
#' @param csv_df CSV dataframe to convert to bibTex
#' @param save_name user input name to save file
#'
#' @author Emmanuel Adeyemo, Kelechi Igwe
#'
#' @return saves a bibTex file in working directory

#' @export

csv2bibtex = function(csv_df, save_name){

  final_name = paste0(save_name,'.bib')

  bibtex_entries = csv_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(bibtex_entry = row_to_bibtex(dplyr::cur_data())) %>%
    dplyr::pull(bibtex_entry)

  # Save to a .bib file
  writeLines(bibtex_entries, final_name, useBytes = TRUE)
  
  # put some output on the console to tell user where file is saved. Read couple lines of the saved file.
  cat(crayon::blue(glue::glue('BibTex file: {final_name} created and saved in {getwd()}. \n\n')))

  cat(crayon::blue("First two bibTeX entries with abstracts:\n"))
  cat(paste(bibtex_entries_with_abstract[1:2], collapse = "\n\n"))

}





