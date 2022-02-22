#' @title Data getting
#' @description This function get information from GrupLac.
#' @import usethis dplyr stringr tidyr rlang devtools lubridate SnowballC rvest scholar stringi tidytext tidyverse treemapify widyr writexl
#' @param groups A dataframe with information about groups and links from GrupLAC
#' @param researchers A dataframe with ID from google scholar form researchers
#' @details Extracts data from Minciencias web pages about research groups and researchers.
#' @export
#' @importFrom tibble column_to_rownames
#' @importFrom purrr map safely
#' @importFrom readr parse_date locale read_csv
#' @importFrom dplyr select mutate filter
#' @importFrom stats end var
#' @importFrom utils data
#' @importFrom lubridate ym today
#' @importFrom igraph get.edgelist simplify graph_from_data_frame E
#' @examples
#' \dontrun{
#' Load data in a dataframe of r
#' groups <- read.csv(".../groups_information.csv", header=T, sep=",")
#'
#' margaret_data <- getting_data(groups)
#'
#' #or just
#' margaret::getting_data(groups)
#' }

getting_data <- function(groups, researchers) {
#
#   usethis::use_package('rlang')
#   usethis::use_package('tidyverse')
#   usethis::use_package('SnowballC')
#   usethis::use_package('here')
#   usethis::use_package('gt')
#   usethis::use_package('kableExtra')
#   usethis::use_package('lubridate')
#   usethis::use_package('openxlsx')
#   usethis::use_package('rvest')
#   usethis::use_package('scholar')
#   usethis::use_package('stringi')
#   usethis::use_package('tidytext')
#   usethis::use_package('treemapify')
#   usethis::use_package('widyr')
#   usethis::use_package('xml2')
#   usethis::use_package('writexl')
#   usethis::use_package('igraph')
  grupo <- NULL

  if(missing(researchers)){
    researchers = 0
  }

  # source("R/data_getting.R")
  # source("R/data_cleaning.R")
  # source("R/data_tidying.R")
  # source("R/merge_quality_articles.R")
  # source("R/data_analysis_descriptive.R")
  # source("R/functions.R")
  # eval(parse("R/functions.R", encoding = "UTF-8"))

  df <- groups |>
    mutate(grupo = str_to_upper(grupo),
           grupo = stri_trans_general(str = grupo,
                                      id = "Latin-ASCII"))

  grupo_df <- data_getting(df)
  produccion_grupos <- data_cleaning(df, grupo_df, researchers)
  produccion_grupos <- data_tidying(produccion_grupos, df)
  produccion_grupos <- merge_quality_articles(produccion_grupos)
  produccion_grupos <- data_analysis_descriptive(produccion_grupos, df)

  #export xlsx file
  produccion_grupos = c(produccion_grupos[1],produccion_grupos[3],produccion_grupos[[2]])
  write_xlsx(produccion_grupos, "margaret.xlsx", col_names = TRUE, format_headers = TRUE, use_zip64 = TRUE)

  return(produccion_grupos)
}
