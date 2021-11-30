data_analysis_descriptive <- function(produccion_actualizada, grupos) {

  produccion_actualizada[[1]] <-
    make_general_grupos(produccion_actualizada) |>
    left_join(grupos, by = "grupo")

  # Added amount of produced papers per researcher to researcher df

  # produccion_actualizada[[3]] <-
  #   count_articles_researcher(produccion_actualizada)

  produccion_actualizada[[3]] <-
    researcher_product(produccion_actualizada)

  produccion_actualizada

}
