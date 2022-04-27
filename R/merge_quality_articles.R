merge_quality_articles <- function(produccion_grupos){

  Issn <- ISSN <- SJR_Q <- ano <- VIGENCIA <- revista <-
  revista_h <- categoria <- internationals_journal_2021 <-
  categoria.x <- categoria.y <- revista.x <-
  categoria_revista <- grupo <- NULL

  data(scimago_2020, envir = environment())
  data(scimago_data, envir = environment())
  data(international_journals_2016_2020, envir = environment())
  data(internationals_journal_2021, envir = environment())
  data(international_journals_2022, envir = environment())

  articulos_unicos <-
    produccion_grupos[[2]][["articulos"]]

  #devtools::install_github("ikashnitsky/sjrdata", force = TRUE)
  #library(sjrdata)

  scimago_2020 <- scimago_2020 |>
    separate_rows(Issn, sep = ", ")|>
    rename("ISSN" = Issn,
           "SJR_Q"= 7) |>
    select(ISSN,SJR_Q) |>
    mutate(ano = "2020,2021,2022") |>
    separate_rows(ano, sep = ",")

  scimago_data <-  scimago_data |>
    rbind(scimago_2020) |>
    mutate(i1 = substr(ISSN, 1,4),
           i2 = substr(ISSN, 5,8)) |>
    unite(ISSN, c("i1","i2"), sep = "-", remove = TRUE)

  journal_2016_2017 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=279750741") |>
    mutate(ano = "2016,2017") |>
    separate_rows(ano, sep = ",") |>
    filter(!str_detect(ISSN, "NA"))


  journal_2018_2019 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=1992863285") |>
    mutate(ano = "2018,2019") |>
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |>
    separate_rows(ISSN, sep = ",") |>
    separate_rows(ano, sep = ",") |>
    filter(!str_detect(ISSN, "NA")) |>
    select(2,3,1,5,4)

  journal_2020 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=219349269") |>
    unite(ISSN, c("issn_impreso","issn_electronico"), sep = ",", remove = TRUE) |>
    mutate(ano = "2020") |>
    select(-VIGENCIA) |>
    separate_rows(ISSN, sep = ",") |>
    filter(!str_detect(ISSN, "NA")) |>
    select(1,2,4,5,3)

  journal_2021 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=758989915") |>
    unite(ISSN, c("issn_impreso","issn_electronico", "issn_l"), sep = ",", remove = TRUE) |>
    mutate(ano = "2021") |>
    select(-VIGENCIA) |>
    separate_rows(ISSN, sep = ",") |>
    filter(!str_detect(ISSN, "NA")) |>
    select(1,2,4,5,3)

  journal_2022 <- read_csv("https://docs.google.com/spreadsheets/d/1ALPh_lgq6OtxgbKXRUEFEmoWcY37gfsnyTszFXbHvWw/export?format=csv&gid=63772797") |>
    unite(ISSN, c("issn_impreso","issn_electronico", "issn_l"), sep = ",", remove = TRUE) |>
    mutate(ano = "2022") |>
    select(-VIGENCIA) |>
    separate_rows(ISSN, sep = ",") |>
    filter(!str_detect(ISSN, "NA")) |>
    select(1,2,4,5,3)

  national_journals_2016_2022 <- rbind(journal_2016_2017, journal_2018_2019,
                                       journal_2020, journal_2021, journal_2022)

  international_journals_2016_2020 <- international_journals_2016_2020 |>
    separate_rows(ISSN, sep = "; ") |>
    rename("revista" = revista_h) |>
    mutate(ano = as.character(ano))

  international_journal_2021 <- internationals_journal_2021 |>
    separate_rows(ISSN, sep = "; ") |>
    mutate(ano = as.character(ano))

  international_journals_2022 <- international_journals_2022 |>
    select(revista, categoria, ano, ISSN)|>
    mutate(ano = as.character(ano),
           ISSN = str_trim(ISSN)) |>
    as_tibble()

  international_journal <- rbind(international_journals_2016_2020,
                                 international_journal_2021,
                                 international_journals_2022)

  scimago_data_merge <- scimago_data |>
    full_join(international_journal, by=c("ISSN","ano")) |>
    mutate(SJR_Q = ifelse(is.na(SJR_Q),categoria,SJR_Q))

  articulos <- articulos_unicos |>
    mutate(ISSN = str_trim(ISSN)) |>
    left_join(national_journals_2016_2022, by =c("ISSN", "ano")) |>
    select(1:7,16,8:14) |>
    rename("categoria" = categoria.x,
           "categoria_revista" = categoria.y,
           "revista" = revista.x) |>
    unique()

  articulos_df <- articulos |>
    filter(is.na(categoria_revista)) |>
    left_join(international_journal, by =c("ISSN", "ano"))|>
    select(1:7,17,9:15) |>
    rename("categoria" = categoria.x,
           "revista" = revista.x,
           "categoria_revista" = categoria.y)

  articulos_national <- articulos |> filter(!is.na(categoria_revista))
  articulos_unicos <- rbind(articulos_national, articulos_df)  |>
    left_join(scimago_data, by = c("ano", "ISSN")) |>
    select(1:8,16,8:15) |>
    group_by(grupo) |> arrange(desc(grupo))|>
    mutate(categoria_revista = ifelse(is.na(categoria_revista),"Sin categoria",categoria_revista),
           SJR_Q = ifelse(is.na(SJR_Q),"Sin categoria",SJR_Q),
           SJR_Q = case_when(SJR_Q =="A1" ~ "Q1",
                             SJR_Q =="A2" ~ "Q2",
                             SJR_Q =="B" ~ "Q3",
                             SJR_Q =="C" ~ "Q4",
                             TRUE ~ SJR_Q))

  rm(scimago_2020,international_journals_2016_2020,
     internationals_journal_2021, international_journals_2022)
  produccion_grupos[[2]][["articulos"]] <- articulos_unicos

  return(produccion_grupos)
}
