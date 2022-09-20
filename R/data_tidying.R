library(tidyverse)
library(treemapify)
#library(kableExtra)
library(stringi)
library(tidytext)
library(SnowballC)
library(widyr)
library(igraph)

# Finding similarities per group

data_tidying <- function(produccion_grupos, grupos) {

  grupo <- id <- titulo <- words <- similarity <- V2 <-
    NULL

  df <- tibble(id = numeric())

  grupo_df_articulos <- # create an id
    produccion_grupos[[2]][["articulos"]] %>%
    mutate(id = 1:length(produccion_grupos[[2]][["articulos"]][["grupo"]]))

  grupos <-
    grupo_df_articulos %>%
    select(grupo) %>%
    unique

  for (i in grupos$grupo) {

    df_1 <-
      grupo_df_articulos %>%
      filter(grupo == i) %>%
      select(id,
             titulo)

    df_2 <-
      df_1 %>%
      unnest_tokens(output = "words",
                    input = titulo,
                    token = "words") %>%
      count(id, words) %>%
      pairwise_similarity(item = id,
                          feature = words,
                          value = n)

    df_3 <-
      df_2 %>%
      filter(similarity >= 0.70) %>%
      rename(Source = "item1",
             Target = "item2",
             weight = "similarity") %>%
      graph_from_data_frame(directed = FALSE) %>%
      simplify()

    df_4 <-
      cbind(get.edgelist(df_3),
            E(df_3)$weight/2) %>%
      as.data.frame() %>%
      select(V2) %>%
      rename(id = "V2") %>%
      mutate(id = as.numeric(id))

    df_5 <-
      df_1 %>%
      anti_join(df_4) %>%
      select(-titulo)

    df <-
      df %>%
      bind_rows(df_5)
  }

  # Select unique values from grupo_df_articulos

  grupo_df_articulos_unicos <-
    grupo_df_articulos %>%
    inner_join(df)


  produccion_grupos[[2]][["articulos"]] <- grupo_df_articulos_unicos

  return(produccion_grupos)

}
