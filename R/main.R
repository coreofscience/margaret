#' @title Data getting
#' @description This function get information from GrupLac
#' @import SnowballC here gt kableExtra lubridate openxlsx rvest scholar stringi tidytext tidyverse treemapify widyr xml2 writexl igraph
#' @param groups A dataframe that has group's informations
#' @param researchers A dataframe with ID from google scholar form researchers
#' @details Extracts data from Minciencias web pages about research groups and researchers
#' @export
#' @importFrom stats end var
#' @importFrom utils data
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

  library(tidyverse)
  library(SnowballC)
  library(here)
  library(gt)
  library(kableExtra)
  library(lubridate)
  library(openxlsx)
  library(rvest)
  library(scholar)
  library(stringi)
  library(tidytext)
  library(treemapify)
  library(widyr)
  library(xml2)
  library(writexl)
  library(igraph)

  if(missing(researchers)){
    researchers = 0
  }

  #source("R/data_getting.R")
  #source("R/data_cleaning.R")
  #source("R/data_tidying.R")
  #source("R/merge_quality_articles.R")
  #source("R/data_analysis_descriptive.R")
  #source("R/functions.R")
  #eval(parse("R/functions.R", encoding = "UTF-8"))

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
