library(tidyverse)
library(rvest)
#library(here)
library(xml2)

data_getting <- function(grupos) {

  data_grupos_all <-
    apply(grupos, 1, function(x){
      URL <- read_html(x['url'])})

  grupo_main = data_getting_main(grupos, data_grupos_all)
  grupo_researcher = data_getting_researcher(grupos, data_grupos_all)
  grupo_product = data_getting_product(grupos, data_grupos_all)

  return(list(grupo_product=grupo_product,
              grupo_main=grupo_main,
              grupo_researcher=grupo_researcher))
}
