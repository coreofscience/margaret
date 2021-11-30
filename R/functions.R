getting_scholar_h_index <- function(data_scholar) {

  scholar_index <-
    data_scholar |>
    mutate(h_index = map(id_scholar, get_profile)) |>
    unnest_wider(h_index) |>
    select(integrantes, id_scholar, h_index)

  return(scholar_index)
}

data_cleaning_researcher <- function(grupo_df, researchers) {

  grupo_researcher_cleaned <-
    grupo_df[["grupo_researcher"]] |>
    mutate(inicio_vinculacion = str_remove(inicio_fin_vinculacion,
                                           "-.*"),
           inicio_vinculacion = ym(inicio_vinculacion),
           fin_vinculacion = str_remove(inicio_fin_vinculacion,
                                        ".*-")) |>
    select(-inicio_fin_vinculacion) |>
    filter(str_detect(fin_vinculacion, "Actual")) |> # Only active researchers
    mutate(posgrade = map(.x = url,
                          .f = safely(get_posgrade_clasficitation_cvlac))) |>
    mutate(posgrade = map(posgrade, "result")) |>
    mutate(posgrade = map(posgrade, ~ replace(.x, is.null(.x), "CvLAC oculto")))

  grupo_researcher_cleaned_1 =
    grupo_researcher_cleaned |> filter(posgrade == "CvLAC oculto") |>
    select(-posgrade) |>
    mutate(posgrade = "CvLAC oculto",
           clasification = "CvLAC oculto")

  grupo_researcher_cleaned_2 <-
    grupo_researcher_cleaned |> filter(posgrade != "CvLAC oculto") |>
    unnest(posgrade) |>
    rbind(grupo_researcher_cleaned_1) |>
    mutate(integrantes = str_to_upper(integrantes),
           integrantes = stri_trans_general(str = integrantes,
                                            id = "Latin-ASCII"),
           integrantes = str_squish(integrantes))

    if(researchers != 0){
      researchers <- researchers |>
        unique() |>
        mutate(researcher = str_to_upper(researcher),
               researcher = stri_trans_general(str = researcher,
                                               id = "Latin-ASCII")) |>
        mutate(h_index = map(id_scholar, safely(get_profile))) |>
        unnest_wider(h_index) |>
        unnest_wider(result) |>
        select(researcher, id_scholar, h_index) |>
        mutate(h_index = if_else(is.na(h_index), 0, h_index))

      grupo_researcher_cleaned_2 <-
        grupo_researcher_cleaned_2 |> left_join(researchers, by = c("integrantes" = "researcher")) |>
        mutate(h_index = ifelse(is.na(h_index), 0, h_index)) |>
        group_by(integrantes,
                 vinculacion,
                 url,
                 posgrade,
                 clasification,
                 h_index,
                 id_scholar)
    }
  else{
    grupo_researcher_cleaned_2 <-
      grupo_researcher_cleaned_2 |>
      group_by(integrantes,
               vinculacion,
               url,
               posgrade,
               clasification)
  }
  grupo_researcher_cleaned_2 <-
    grupo_researcher_cleaned_2 |>
    mutate(grupo = paste0(grupo,
                          collapse = "; "),
           horas_dedicacion = paste0(horas_dedicacion,
                                     collapse = "; "),
           inicio_vinculacion = paste0(inicio_vinculacion,
                                       collapse = "; ")) |>
    unique()

  return(grupo_researcher_cleaned_2)

}


data_cleaning_main <- function(grupos, grupo_df) {

  grupo_main_cleaned <-
    grupo_df[["grupo_main"]] |>
    mutate(fecha_creacion = ym(fecha_creacion),
           departamento = str_remove(departamento_ciudad,
                                     "-.*"),
           departamento = str_trim(departamento),
           ciudad = str_remove(departamento_ciudad,
                               ".*-"),
           ciudad = str_trim(ciudad),
           clasificacion = substr(clasificacion,
                                  start = 1,
                                  stop = 2),
           clasificacion = str_remove(clasificacion,
                                      "\r"),
           area_conocimiento_1 = str_remove(area_conocimiento,
                                            "--.*"),
           area_conocimiento_0 = str_extract(area_conocimiento,
                                             "--.*"),
           area_conocimiento_0 = str_remove(area_conocimiento,
                                            "--"),
           area_conocimiento_2 = str_remove(area_conocimiento_0,
                                            "--.*"),
           area_conocimiento_3 = str_remove(area_conocimiento,
                                            ".*--")) |>
    select(grupo,
           fecha_creacion,
           departamento,
           ciudad,
           lider,
           web,
           email,
           clasificacion,
           area_conocimiento_1,
           area_conocimiento_2,
           area_conocimiento_3) |>
    mutate(grupo = stri_trans_general(str = grupo, # Removing tildes
                                      id = "Latin-ASCII")) |>
    left_join(grupos)

  return(grupo_main_cleaned)
}

data_cleaning_product <- function(grupo_df) {

  trabajos_dirigidos = trabajos_dirigidos_ucla(grupo_df[["grupo_product"]])
  eventos_cientificos = eventos_cientificos_ucla(grupo_df[["grupo_product"]])
  articulos = articulos_ucla(grupo_df[["grupo_product"]])
  proyectos = proyectos_ucla(grupo_df[["grupo_product"]])
  capitulos = capitulos_ucla(grupo_df[["grupo_product"]])
  jurado = jurado_ucla(grupo_df[["grupo_product"]])
  cursos = cursos_ucla(grupo_df[["grupo_product"]])
  otros_articulos = otros_articulos_ucla(grupo_df[["grupo_product"]])
  consultorias = consultorias_ucla(grupo_df[["grupo_product"]])
  libros = libros_ucla(grupo_df[["grupo_product"]])
  participacion_comites <- participacion_comites_ucla(grupo_df[["grupo_product"]])
  demas_trabajos <- demas_trabajos_ucla(grupo_df[["grupo_product"]])
  informes_investigacion <- informes_investigacion_ucla(grupo_df[["grupo_product"]])
  innovaciones_gestion <- innovaciones_gestion_ucla(grupo_df[["grupo_product"]])
  generacion_multimedia <- generacion_multimedia_ucla(grupo_df[["grupo_product"]])
  otra_publicacion_divulgativa <- otra_publicacion_divulgativa_ucla(grupo_df[["grupo_product"]])
  documentos_trabajo <- documentos_trabajo_ucla(grupo_df[["grupo_product"]])
  ediciones <- ediciones_ucla(grupo_df[["grupo_product"]])
  estrategias_pedagogicas <- estrategias_pedagogicas_ucla(grupo_df[["grupo_product"]])
  redes_conocimiento <-  redes_conocimiento_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_virtual <- generacion_contenido_virtual_ucla(grupo_df[["grupo_product"]])
  espacios_participacion <- espacios_participacion_ucla(grupo_df[["grupo_product"]])
  softwares <- softwares_ucla(grupo_df[["grupo_product"]])
  innovaciones_procesos <- innovaciones_procesos_ucla(grupo_df[["grupo_product"]])
  otros_libros <- otros_libros_ucla(grupo_df[["grupo_product"]])
  estrategias_comunicacion <- estrategias_comunicacion_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_impreso <- generacion_contenido_impreso_ucla(grupo_df[["grupo_product"]])
  informes_tecnicos <- informes_tecnicos_ucla(grupo_df[["grupo_product"]])
  participacion_ciudadana_cti <- participacion_ciudadana_cti_ucla(grupo_df[["grupo_product"]])
  regulaciones_normas <- regulaciones_normas_ucla(grupo_df[["grupo_product"]])
  actividades_evaluador <- actividades_evaluador_ucla(grupo_df[["grupo_product"]])
  actividades_formacion <- actividades_formacion_ucla(grupo_df[["grupo_product"]])
  apropiacion_social_conocimiento <- apropiacion_social_conocimiento_ucla(grupo_df[["grupo_product"]])
  produccion_tecnica_tecnologica <- produccion_tecnica_tecnologica_ucla(grupo_df[["grupo_product"]])
  generacion_contenido_audio <- generacion_contenido_audio_ucla(grupo_df[["grupo_product"]])
  conceptos_tecnicos <- conceptos_tecnicos_ucla(grupo_df[["grupo_product"]])
  reglamentos_tecnicos<- reglamentos_tecnicos_ucla(grupo_df[["grupo_product"]])
  otros_productos_tencologicos <- otros_productos_tencologicos_ucla(grupo_df[["grupo_product"]])
  traducciones <- traducciones_ucla(grupo_df[["grupo_product"]])
  signos_distintivos <- signos_distintivos_ucla(grupo_df[["grupo_product"]])
  nuevos_registros_cientificos <- nuevos_registros_cientificos_ucla(grupo_df[["grupo_product"]])
  notas_cientificas <- notas_cientificas_ucla(grupo_df[["grupo_product"]])
  Producciones_de_contenido_digital <- Producciones_de_contenido_digital_ucla(grupo_df[["grupo_product"]])
  libros_divulgacion <- libros_divulgacion_compilacion_ucla(grupo_df[["grupo_product"]])
  libros_formacion <- libros_formacion_ucla(grupo_df[["grupo_product"]])
  Producciones_digital_audiovisual <-producciones_digital_audiovisual_ucla(grupo_df[["grupo_product"]])
  manuales_guias_especializadas <- manuales_guias_especializadas_ucla(grupo_df[["grupo_product"]])
  divulgacion_publica_contenidos_transmedia <- divulgacion_publica_contenidos_transmedia_ucla(grupo_df[["grupo_product"]])

  return(list(trabajos_dirigidos = trabajos_dirigidos,
              eventos_cientificos = eventos_cientificos,
              articulos = articulos,
              proyectos = proyectos,
              capitulos = capitulos,
              jurado = jurado,
              cursos = cursos,
              otros_articulos = otros_articulos,
              consultorias = consultorias,
              libros = libros,
              participacion_comites = participacion_comites,
              demas_trabajos = demas_trabajos,
              informes_investigacion = informes_investigacion,
              innovaciones_gestion = innovaciones_gestion,
              generacion_multimedia = generacion_multimedia,
              otra_publicacion_divulgativa = otra_publicacion_divulgativa,
              documentos_trabajo = documentos_trabajo,
              ediciones = ediciones,
              estrategias_pedagogicas = estrategias_pedagogicas,
              redes_conocimiento =  redes_conocimiento,
              generacion_contenido_virtual = generacion_contenido_virtual,
              espacios_participacion = espacios_participacion,
              softwares = softwares,
              innovaciones_procesos = innovaciones_procesos,
              otros_libros = otros_libros,
              estrategias_comunicacion = estrategias_comunicacion,
              generacion_contenido_impreso = generacion_contenido_impreso,
              informes_tecnicos = informes_tecnicos,
              participacion_ciudadana_cti = participacion_ciudadana_cti,
              regulaciones_normas = regulaciones_normas,
              actividades_evaluador = actividades_evaluador,
              actividades_formacion = actividades_formacion,
              apropiacion_social_conocimiento = apropiacion_social_conocimiento,
              produccion_tecnica_tecnologica = produccion_tecnica_tecnologica,
              generacion_contenido_audio = generacion_contenido_audio,
              conceptos_tecnicos = conceptos_tecnicos,
              reglamentos_tecnicos=reglamentos_tecnicos,
              otros_productos_tencologicos = otros_productos_tencologicos,
              traducciones = traducciones,
              signos_distintivos = signos_distintivos,
              nuevos_registros_cientificos = nuevos_registros_cientificos,
              notas_cientificas = notas_cientificas,
              Producciones_de_contenido_digital = Producciones_de_contenido_digital,
              libros_divulgacion = libros_divulgacion,
              libros_formacion = libros_formacion,
              Producciones_digital_audiovisual= Producciones_digital_audiovisual,
              manuales_guias_especializadas = manuales_guias_especializadas,
              divulgacion_publica_contenidos_transmedia = divulgacion_publica_contenidos_transmedia))
}



data_getting_product <- function(grupos, data_grupos_all){

  grupo_df <-
    tibble(grupo = character(),
           producto = character(),
           categoria = character())

  for (i in 1:length(grupos$url)) {

    grupo <-
      data_grupos_all[[i]] |>
      html_table()

    for (j in 14:85) {

      df_1 =
        grupo %>%
        tibble() %>%
        slice(j) %>%
        unlist %>%
        tibble() %>%
        rename(producto = ".") %>%
        mutate(grupo = grupos$grupo[i])

      if (length(df_1$producto) > 1) {

        df_2 =
          df_1 %>%
          filter(producto != "") %>%
          mutate(categoria = df_1$producto[1]) %>%
          filter(str_detect(producto, "^[0-9]\\.*")) %>%
          select(grupo, producto, categoria)

      } else {

        df_2 =
          df_1 %>%
          mutate(categoria = df_1$producto[1],
                 producto = "NO TIENE") %>%
          select(grupo, producto, categoria) %>%
          unique()

      }

      grupo_df <-
        bind_rows(df_2,
                  grupo_df)

    }
  }

  rm(df_1, df_2, grupo, i, j)
  return(grupo_df)
}

data_getting_main <- function(grupos, data_grupos_all){

  df_group_main = tibble()

  for (i in 1:length(data_grupos_all)) {

    df_1 =
      data_grupos_all[[i]] |>
      html_table()

    df_2 =
      df_1[[1]] |>
      column_to_rownames("X1") |>
      t() |>
      as.data.frame() |>
      rename(fecha_creacion = 2,
             departamento_ciudad = 3,
             lider = 4,
             web = 6,
             email = 7,
             clasificacion = 8,
             area_conocimiento = 9) |>
      select(fecha_creacion,
             departamento_ciudad,
             lider,
             web,
             email,
             clasificacion,
             area_conocimiento) |>
      mutate(grupo = grupos$grupo[i],
             gruplac = grupos$url[i])

    df_group_main <- bind_rows(df_group_main, df_2)
  }
  return(df_group_main)
}

data_getting_researcher <- function(grupos, data_grupos_all){

  df_researcher = tibble()

  for (i in 1:length(data_grupos_all)) {

    df_i <- html_nodes(data_grupos_all[[i]],"a") %>%
      html_attr("href") %>%
      tibble() %>%
      mutate(grupo = grupos$grupo[i]) |>
      rename(url = 1) |>
      slice(-1,-2)

    df_1 =
      data_grupos_all[[i]] |>
      html_table()

    df_2 =
      df_1[[5]] |>
      slice(-1,-2) |>
      rename(nombre = X1,
             vinculacion = X2,
             horas_dedicacion = X3,
             inicio_fin_vinculacion = X4) |>
      mutate(integrantes = str_remove(nombre, ".*-"),
             integrantes = str_trim(integrantes)) |>
      select(integrantes,
             vinculacion,
             horas_dedicacion,
             inicio_fin_vinculacion,
             -nombre) |>
      cbind(df_i)

    df_researcher <- bind_rows(df_researcher,df_2)
  }
  return(df_researcher)
}



trabajos_dirigidos_ucla <- function(grupo_df) {

  trabajosdirigidos =
    grupo_df %>%
    filter(categoria == "Trabajos dirigidos/turorías") %>%
    separate(producto,
             c("info_1","info_2","info_3","info_4","info_5",
               "info_6","info_7","info_8","info_9","info_10",
               "info_11","info_12","info_13","info_14"),
             sep = "\r\n" ) %>%
    select(-info_4,-info_5,-info_6,-info_8,-info_12,-info_14) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    select(-info_1) %>%
    mutate(desde = str_trim(info_2),
           desde = str_remove(desde, "Desde"),
           desde = str_remove(desde, " hasta"),
           desde = str_trim(desde),
           hasta = str_trim(info_3),
           hasta = str_remove(hasta, ",.*"),
           tipo_orientacion =str_remove(info_3, ".*: "),
           estudiante = str_trim(info_7),
           estudiante = str_remove(estudiante, ",$"),
           programa_academico = str_remove(info_9, ".*:"),
           programa_academico = str_trim(programa_academico),
           paginas = str_remove(info_10, ".*páginas: "),
           paginas= str_remove(paginas, ",.*"),
           valoracion= str_extract(info_10, ",.*"),
           valoracion= str_remove(valoracion, "^,"),
           valoracion= str_remove(valoracion, ".*:"),
           valoracion= str_remove(valoracion, ","),
           valoracion= str_trim(valoracion),
           institucion = str_trim(info_11),
           institucion = str_remove(institucion, ".*Institución: "),
           tutor_coautor = str_trim(info_13),
           tutor_coautor = str_remove(tutor_coautor, ".*: ")) %>%
    select(-info_2,
           -info_3,
           -info_7,
           -info_9,
           -info_10,
           -info_11,
           -info_13)

  return(trabajosdirigidos)

}

eventos_cientificos_ucla <- function(grupo_df) {

  grupo_df_EventosCientificos <-
    grupo_df %>%
    filter(categoria == "Eventos Científicos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"),
             sep = "\r\n" )%>%
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_4),
           tipo_evento = str_remove(info_1, ":.*"),
           tipo_evento = str_remove(tipo_evento, ".*-"),
           tipo_evento = str_trim(tipo_evento),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    select(-info_1) %>%
    mutate(ciudad_evento= str_remove(info_2, ",.*"),
           info_2 = str_remove(info_2, ".*desde*"),
           fecha_inicio = str_remove(info_2, "-$"),
           fecha_fin = str_remove(info_3, ".*hasta")) %>%
    select(-info_2,-info_3) %>%
    mutate(info_4 = str_remove(info_4, "Ámbito:"),
           ambito = str_remove(info_4, ",.*"),
           info_4= str_extract(info_4, "Tipos de participación:.*"),
           info_4= str_remove(info_4, ".*Tipos de participación:"),
           tipo_participacion=str_remove(info_4,"Nombre de la institución.*"),
           info_4= str_extract(info_4, "Nombre de la institución.*"),
           nombre_Institucion= str_remove(info_4, ".*Nombre de la institución:")) %>%
    select(-info_4) %>%
    mutate(tipo_vinculacion = str_remove(info_5,"Nombre.*"),
           tipo_vinculacion = str_remove(tipo_vinculacion,"Ámbito.*"),
           tipo_vinculacion = str_trim(tipo_vinculacion)) %>%
    select(-info_5)

  return(grupo_df_EventosCientificos)
}

articulos_ucla <- function(grupo_df) {

  grupo_df_articulos_lost_1 <-
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>%
    separate(producto,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    filter(!is.na(info_5)) %>%
    slice(1) %>%
    select(-info_4) %>%
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_3),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    select(-info_1) %>%
    mutate(pais_revista = str_remove(info_2, ",.*"),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = info_2,
           info_2 = str_extract(info_4, "ISSN.*"),
           info_2 = str_trim(info_4),
           ISSN = str_remove(info_4, ",.*"),
           ISSN = str_remove(ISSN, "ISSN:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_4, ",.*"),
           info_2 = str_remove(info_4, "^,"),
           info_2 = str_trim(info_4),
           ano = str_remove(info_4, "\\s.*"),
           info_2 = str_extract(info_4, "\\s.*"),
           info_2 = str_trim(info_4),
           vol = str_remove(info_4, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_2 = str_extract(info_4, "\\s.*"),
           info_2 = str_trim(info_4),
           fasc = str_remove(info_4, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_2 = str_extract(info_4, "págs.*"),
           info_2 = str_trim(info_4),
           pags = str_remove(info_4, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_4, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>%
    select(-info_4,
           -info_2) %>%
    mutate(autores = str_remove(info_5, "Autores: "),
           autores = str_remove(autores, ",$"),
           autores = str_trim(autores)) %>%
    select(-info_5,
           -info_3)

  grupo_df_articulos_lost_2 <-
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>%
    separate(producto,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    filter(!is.na(info_5)) %>%
    slice(2) %>%
    select(-info_4) %>%
    mutate(info_2 = str_trim(info_2),
           info_5 = str_trim(info_5),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    select(-info_1) %>%
    mutate(pais_revista = str_remove(info_2, ",.*"),
           pais_revista = str_remove(pais_revista, ".*,"),
           revista = str_remove(info_3, "ISSN.*"),
           info_3 = str_extract(info_3, "ISSN.*"),
           info_3 = str_trim(info_3),
           ISSN = str_remove(info_3, ",.*"),
           info_3 = str_extract(info_3, ",.*"),
           info_3 = str_remove(info_3, "^,"),
           info_3 = str_trim(info_3),
           ano = str_remove(info_3, "\\s.*"),
           info_3 = str_extract(info_3, "\\s.*"),
           info_3 = str_trim(info_3),
           vol = str_remove(info_3, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_3 = str_extract(info_3, "\\s.*"),
           info_3 = str_trim(info_3),
           fasc = str_remove(info_3, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_3 = str_extract(info_3, "págs.*"),
           info_3 = str_trim(info_3),
           pags = str_remove(info_3, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_3, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>%
    select(-info_2,
           -info_3) %>%
    mutate(autores = str_remove(info_5, "Autores: "),
           autores = str_remove(autores, ",$"),
           autores = str_trim(autores)) %>%
    select(-info_5) %>%
    mutate(DOI = str_remove(DOI, "http://dx.doi.org/"))

  grupo_df_articulos <-
    grupo_df %>%
    filter(categoria == "Artículos publicados") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    filter(is.na(info_5)) %>%
    select(-info_3,
           -info_5) %>%
    mutate(info_2 = str_trim(info_2),
           info_4 = str_trim(info_4),
           tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    select(-info_1) %>%
    mutate(pais_revista = str_remove(info_2, ",.*"),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = str_remove(info_2, "ISSN.*"),
           info_2 = str_extract(info_2, "ISSN.*"),
           info_2 = str_trim(info_2),
           ISSN = str_remove(info_2, ",.*"),
           ISSN = str_remove(ISSN, "ISSN:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_2, ",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           ano = str_remove(info_2, "\\s.*"),
           info_2 = str_extract(info_2, "\\s.*"),
           info_2 = str_trim(info_2),
           vol = str_remove(info_2, "\\s.*"),
           vol = str_remove(vol, "vol:"),
           info_2 = str_extract(info_2, "\\s.*"),
           info_2 = str_trim(info_2),
           fasc = str_remove(info_2, "págs.*"),
           fasc = str_remove(fasc, "fasc: "),
           info_2 = str_extract(info_2, "págs.*"),
           info_2 = str_trim(info_2),
           pags = str_remove(info_2, ", DOI.*"),
           pags = str_remove(pags, "págs: "),
           DOI = str_extract(info_2, "DOI.*"),
           DOI = str_remove(DOI, "DOI:")) %>%
    select(-info_2) %>%
    mutate(autores = str_remove(info_4, "Autores: "),
           autores = str_remove(autores, ",$")) %>%
    select(-info_4)

  return(grupo_df_articulos)
}

proyectos_ucla <- function(grupo_df) {

  grupo_df_proyectos <-
    grupo_df %>%
    filter(categoria == "Proyectos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_proyecto = str_remove(info_1, ":.*"),
           Tipo_proyecto = str_remove(Tipo_proyecto, ".*-"),
           Tipo_proyecto = str_trim(Tipo_proyecto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1, -info_2) %>%
    mutate(info_3= str_trim(info_3),
           Fecha_inicio= str_remove(info_3, "-$"),
           Fecha_inicio= str_trim(Fecha_inicio)) %>%
    select(-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Fecha_Fin=str_extract(info_4, ".*")) %>%
    select(-info_4,-info_5)
}

capitulos_ucla <- function(grupo_df) {

  grupo_df_capitulos_libros_publicados =
    grupo_df %>%
    filter(categoria == "Capítulos de libro publicados") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4"),
             sep = "\r\n" ) %>%
    select(-info_3) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo_capitulo = str_extract(info_1, ":.*"),
           titulo_capitulo = str_remove(titulo_capitulo, "^:"),
           titulo_capitulo = str_trim(titulo_capitulo),
           pais = str_remove(info_2, ", \\d.*"),
           pais = str_trim(pais),
           ano = str_remove(info_2, ", ISBN.*"),
           ano = str_extract(ano, ", .*"),
           ano = str_remove(ano, "^,"),
           ano = str_remove(ano, ",.*"),
           ano = str_trim(ano),
           titulo_libro = str_extract(info_2, "\\d, .*"),
           titulo_libro = str_remove(titulo_libro, ", ISBN.*"),
           titulo_libro = str_remove(titulo_libro, ".*, "),
           ISBN = str_remove(info_2, ".*ISBN: "),
           ISBN = str_remove(ISBN, ", Vol.*"),
           vol = str_remove(info_2, ".*Vol."),
           vol = str_remove(vol, ", pág.*"),
           pags = str_remove(info_2, ".*pág.:"),
           pags = str_remove(pags, ",.*"),
           editorial = str_remove(info_2, ".*Ed. "),
           autores = str_remove(info_4, ".*Autores: ")) %>%
    select(-info_1, -info_2, -info_4)
}

jurado_ucla <- function(grupo_df) {

  grupo_df_Jurado <-
    grupo_df %>%
    filter(categoria == "Jurado/Comisiones evaluadoras de trabajo de grado") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7"),
             sep = "\r\n" ) %>%
    select(-info_6) %>%
    mutate(Nivel_Academico = str_remove(info_1, ":.*"),
           Nivel_Academico = str_remove(Nivel_Academico, ".*-"),
           Nivel_Academico = str_trim(Nivel_Academico),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>%
    mutate(Pais=str_trim(pais),
           ano=str_trim(anno),
           Idioma=str_remove(info_3,",.*"),
           Idioma=str_extract(Idioma,":.*"),
           Idioma=str_remove(Idioma,".*:"),
           Idioma=str_trim(Idioma),
           Medio_divulgacion=str_extract(info_3,"n:.*"),
           Medio_divulgacion=str_remove(info_3,".*:"),
           Medio_divulgacion=str_trim(Medio_divulgacion),
           Sitio_Web=str_remove(info_4,",.*"),
           Sitio_Web=str_remove(Sitio_Web,".*:"),
           Sitio_Web=str_trim(Sitio_Web),
           Nombre_del_Orientado=str_extract(info_4,",.*"),
           Nombre_del_Orientado=str_extract(Nombre_del_Orientado,":.*"),
           Nombre_del_Orientado=str_remove(Nombre_del_Orientado,":"),
           Nombre_del_Orientado=str_trim(Nombre_del_Orientado),
           Programa_Academico=str_remove(info_5,",.*"),
           Programa_Academico=str_remove(Programa_Academico,".*:"),
           Programa_Academico=str_trim(Programa_Academico),
           Institucion=str_extract(info_5,",.*"),
           Institucion=str_extract(Institucion,":.*"),
           Institucion=str_remove(Institucion,":"),
           Institucion=str_remove(Institucion,".$"),
           Institucion=str_trim(Institucion),
           Autores=str_remove(info_7,".*:"),
           Autores=str_trim(Autores)) %>%
    select(-info_1,-info_3,-info_4,-info_5,-info_7,-pais,-anno)
}

cursos_ucla <- function(grupo_df) {

  grupo_df_CursosCortaDuracion <-
    grupo_df %>%
    filter(categoria == "Curso de Corta Duración Dictados") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_Curso = str_remove(info_1, ":.*"),
           Tipo_Curso = str_remove(Tipo_Curso, ".*-" ),
           Nombre_curso = str_remove(info_1, ".*:"),
           Nombre_curso = str_trim(Nombre_curso)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Ano = str_extract(info_2, ",.*"),
           Ano = str_remove(Ano, ","),
           Ano = str_trim(Ano),
           Ano = str_remove(Ano, ",")) %>%
    select(-info_2) %>%
    mutate(info_3=str_trim(info_3),
           Idioma=str_extract(info_3, ".*,"),
           Idioma=str_remove(Idioma, ","),
           Idioma=str_remove(Idioma, ".*: "),
           Medio_divulgacion= str_remove(info_3, ".*: ")) %>%
    select(-info_3) %>%
    mutate(info_4= str_trim(info_4),
           sitio_web= str_remove(info_4, ",.*"),
           sitio_web=str_remove(sitio_web, ".*:"),
           Participacion=str_remove(info_4, ".*, "),
           Participacion=str_remove(Participacion, ","),
           Participacion=str_remove(Participacion, "Participación como"),
           Participacion=str_trim(Participacion)
    ) %>%
    select(-info_4) %>%
    mutate(info_5= str_trim(info_5),
           Duracion_semanas= str_remove(info_5, ",.*"),
           Duracion_semanas=str_remove(Duracion_semanas, ".*:"),
           Duracion_semanas= str_trim(Duracion_semanas),
           Finalidad= str_remove(info_5,".*Finalidad:"),
           Finalidad= str_trim(Finalidad)) %>%
    select(-info_5) %>%
    mutate(info_6= str_trim(info_6),
           lugar= str_remove(info_6, ",.*"),
           lugar= str_remove(lugar, ".*:"),
           lugar= str_trim(lugar),
           Institucion_Financiadora= str_extract(info_6, "Institución financiadora:.*"),
           Institucion_Financiadora= str_remove(Institucion_Financiadora, "Institución financiadora:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>%
    select(-info_6,-info_7) %>%
    mutate(info_8 = str_trim(info_8),
           Autores = str_remove(info_8,".*:")) %>%
    select(-info_8)
}

otros_articulos_ucla <- function(grupo_df) {

  grupo_df_otros_articulos <-
    grupo_df %>%
    filter(categoria == "Otros artículos publicados") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    unite(info_2,"info_2",c("info_2","info_3"),sep = "",remove = TRUE) %>%
    unite(info_4,"info_4",c("info_4","info_5"),sep = "",remove = TRUE) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           info_2 = str_extract(info_2,",.*"),
           info_2 = str_remove(info_2, "^,"),
           info_2 = str_trim(info_2),
           revista = str_remove(info_2,"ISSN.*"),
           revista = str_trim(revista),
           info_2 = str_extract(info_2, "ISSN.*"),
           info_2 = str_trim(info_2),
           ISSN = str_remove(info_2,",.*"),
           ISSN = str_extract(ISSN,":.*"),
           ISSN = str_remove(ISSN, "^:"),
           ISSN = str_trim(ISSN),
           info_2 = str_extract(info_2,",.*"),
           ano = str_remove(info_2,"vol.*"),
           ano = str_remove(ano,"^,"),
           ano = str_trim(ano),
           info_2 = str_extract(info_2,"vol.*"),
           vol = str_remove(info_2,"fasc.*"),
           vol = str_remove(vol,".*:"),
           vol = str_remove(vol,"^:"),
           vol = str_trim(vol),
           fasc = str_extract(info_2,".*p"),
           fasc = str_remove(fasc,"p$"),
           fasc = str_extract(fasc,"c.*"),
           fasc = str_extract(fasc,":.*"),
           fasc = str_remove(fasc,"^:"),
           fasc = str_trim(fasc),
           pags = str_extract(info_2,"gs.*"),
           pags = str_extract(pags,":.*"),
           pags = str_remove(pags,"^:"),
           pags = str_trim(pags),
           autores = str_remove(info_4, "Autores: "),
           autores = str_trim(autores)) %>%
    select(-info_1,-info_2,-info_4)
}

consultorias_ucla <- function(grupo_df) {

  grupo_df_consultorias_cientico_tecnologicas <-
    grupo_df%>%
    filter(categoria == "Consultorías científico-tecnológicas") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_Consultoria = str_remove(info_1, ":.*"),
           Tipo_Consultoria = str_trim(Tipo_Consultoria),
           Tipo_Consultoria = str_remove(Tipo_Consultoria, ".*\\d."),
           Tipo_Consultoria = str_remove(Tipo_Consultoria, "."),
           Tipo_Consultoria = str_trim(Tipo_Consultoria),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1, -info_2) %>%
    separate(info_3,
             c("i_1","i_2","i_3","i_4"),
             sep = ",") %>%
    mutate(i_1 = str_trim(i_1),
           i_1= str_remove(i_1, ".*:"),
           Ano_inicio= str_extract(i_1, ".*"),
           i_2= str_trim(i_2),
           i_2= str_remove(i_2, ".*:"),
           Mes_inicio= str_extract(i_2, ".*"),
           i_3 = str_trim(i_3),
           i_3= str_remove(i_3, ".*:"),
           Ano_fin= str_extract(i_3, ".*"),
           i_4= str_trim(i_4),
           i_4= str_remove(i_4, ".*:"),
           Mes_fin= str_extract(i_4, ".*")) %>%
    select(-i_1,-i_2,-i_3,-i_4) %>%
    mutate(info_5=str_trim(info_5),
           Idioma= str_remove(info_5, ",.*"),
           Idioma= str_remove(Idioma, ".*:"),
           Idioma= str_trim(Idioma),
           Ciudad= str_extract(info_5, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_remove(Ciudad, ",.*"),
           Ciudad= str_remove(Ciudad, ".*:"),
           Ciudad= str_trim(Ciudad),
           Disponibilidad= str_extract(info_5, "Disponibilidad.*"),
           Disponibilidad= str_remove(Disponibilidad, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Duracion= str_extract(info_5, "Duración.*"),
           Duracion= str_remove(Duracion, ".*:"),
           Duracion= str_remove(Duracion, ",$"),
           Duracion= str_trim(Duracion)
    ) %>%
    select(-info_4,-info_5) %>%
    mutate(info_6=str_trim(info_6),
           Num_contrato= str_remove(info_6, ",.*"),
           Num_contrato= str_remove(Num_contrato, ".*:"),
           Num_contrato= str_trim(Num_contrato),
           Institucion_Prestadora_servicio= str_extract(info_6, ",.*"),
           Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, "^,"),
           Institucion_Prestadora_servicio= str_remove(Institucion_Prestadora_servicio, ".*:"),
           Institucion_Prestadora_servicio= str_trim(Institucion_Prestadora_servicio)
    ) %>%
    select(-info_6,-info_7)
}

libros_ucla <- function(grupo_df) {

  grupo_df_librosPublicados <-
    grupo_df %>%
    filter(categoria == "Libros publicados") %>%
    separate(producto ,
             c("info_1", "info_2","info_3",
               "info_4", "info_5", "info_6", "info_7"),
             sep = "\r\n" ) %>%
    select(-info_6) |>
    mutate(info_1 = str_trim(info_1),
           Tipo_producto = str_remove(info_1, ":.*"),
           Tipo_producto = str_remove(Tipo_producto, ".*-" ),
           Tipo_producto = str_trim(Tipo_producto),
           Titulo = str_remove(info_1, ".*investigación :"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Ano = str_remove(info_3, ","),
           Ano = str_trim(Ano, side = "both"),
           ISBN= str_extract(info_4, "ISBN.*"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ".*:"),
           ISBN = str_trim(ISBN),
           ISBN = str_remove(ISBN, ","),
           ISBN = str_remove(ISBN, "SIN"),
           Editorial= str_extract(info_5,"Ed.*"),
           Editorial= str_remove(Editorial,"Ed."),
           Editorial=str_trim(Editorial)) %>%
    select(-info_2,-info_3) %>%
    mutate(info_7=str_trim(info_7),
           Autores=str_extract(info_7, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_4, -info_5, -info_7)

}

participacion_comites_ucla <- function(grupo_df) {

  #Data cleaning "Participación en comités de evaluación"

  grupo_df_participacion_comites <-
    grupo_df %>%
    filter(categoria == "Participación en comités de evaluación") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"),
             sep = "\r\n" ) %>%
    select(-info_5) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>%
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           sitio_web = str_remove(info_3,".*web"),
           sitio_web = str_remove(sitio_web,"^:"),
           sitio_web = str_trim(sitio_web),
           medio_divulgacion = str_remove(info_4,",.*"),
           medio_divulgacion = str_remove(medio_divulgacion,".*:"),
           medio_divulgacion = str_trim(medio_divulgacion),
           institucion = str_remove(info_4,".*,"),
           institucion = str_remove(institucion,".*:"),
           institucion = str_trim(institucion),
           autores = str_remove(info_6,".*:"),
           autores = str_trim(autores)) %>%
    select(-info_1,-pais,-anno,-info_3,-info_4,-info_6)


}

demas_trabajos_ucla <- function(grupo_df) {

  grupo_df_demas_trabajos <-
    grupo_df%>%
    filter(categoria == "Demás trabajos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Pais= str_trim(Pais),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ",$")) %>%
    select(-info_2) %>%
    mutate(info_3=str_trim(info_3),
           Idioma= str_remove(info_3, ",.*"),
           Idioma= str_remove(Idioma, ".*:"),
           Idioma= str_trim(Idioma),
           Medio_divulgacion= str_extract(info_3, ",.*"),
           Medio_divulgacion= str_remove(Medio_divulgacion, "^,"),
           Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
           Medio_divulgacion= str_trim(Medio_divulgacion)) %>%
    select(-info_3, -info_4) %>%
    mutate(info_5=str_trim(info_5),
           Autores= str_remove(info_5, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_5)

}

informes_investigacion_ucla <- function(grupo_df) {

  grupo_df_informe_de_investigacion <-
    grupo_df %>%
    filter(categoria == "Informes de investigación") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:" ),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Ano = str_remove(info_2, ",.*"),
           Ano = str_trim(Ano),
           Proyecto_de_investigacion = str_extract(info_2, ",.*" ),
           Proyecto_de_investigacion = str_extract(Proyecto_de_investigacion, ":.*"),
           Proyecto_de_investigacion = str_remove(Proyecto_de_investigacion, "^:"),
           Proyecto_de_investigacion = str_trim(Proyecto_de_investigacion)) %>%
    select(-info_2,-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_4)

}

innovaciones_gestion_ucla <- function(grupo_df) {

  grupo_df_innov_gestion_empresarial <-
    grupo_df %>%
    filter(categoria == "Innovaciones generadas en la Gestión Empresarial") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    select(-info_4) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>%
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           disponibilidad = str_remove(info_3,",.*"),
           disponibilidad = str_remove(disponibilidad,".*:"),
           disponibilidad = str_trim(disponibilidad),
           info_3 = str_extract(info_3, ",.*"),
           info_3 = str_extract(info_3, ":.*"),
           info_3 = str_remove(info_3, "^:"),
           institucion_financiadora = str_trim(info_3),
           autores = str_remove(info_5,".*:"),
           autores = str_trim(autores)) %>%
    select(-info_1,-pais,-anno,-info_3,-info_5)


}

generacion_multimedia_ucla <- function(grupo_df) {

  grupo_df_generacion_contenido_multimedia <-
    grupo_df%>%
    filter(categoria == "Generación de Contenido Multimedia") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_generacion = str_remove(info_1, ":.*"),
           Tipo_generacion = str_trim(Tipo_generacion),
           Tipo_generacion = str_remove(Tipo_generacion, ".*-"),
           Tipo_generacion = str_trim(Tipo_generacion),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Ano= str_remove(info_2, ",.*"),
           Ano= str_trim(Ano),
           Pais= str_extract(info_2, ",.*"),
           Pais= str_remove(Pais, "^,"),
           Pais= str_remove(Pais, ",$")) %>%
    select(-info_2) %>%
    mutate(info_3=str_trim(info_3),
           Idioma= str_remove(info_3, ".*:"),
           Idioma= str_trim(Idioma)) %>%
    select(-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Medio_divulgacion= str_remove(info_4, ",.*"),
           Medio_divulgacion= str_remove(Medio_divulgacion, ".*:"),
           Medio_divulgacion= str_trim(Medio_divulgacion),
           Sitio_web= str_extract(info_4, ",.*"),
           Sitio_web= str_remove(Sitio_web, "^,"),
           Sitio_web= str_remove(Sitio_web, ".*:"),
           Sitio_web= str_trim(Sitio_web)) %>%
    select(-info_4) %>%
    mutate(info_5=str_trim(info_5),
           Emisora= str_remove(info_5, ",.*"),
           Emisora= str_remove(Emisora, ".*:"),
           Emisora= str_trim(Emisora),
           Instituciones_participantes= str_extract(info_5, ",.*"),
           Instituciones_participantes= str_remove(Instituciones_participantes, "^,"),
           Instituciones_participantes= str_remove(Instituciones_participantes, ".*:"),
           Instituciones_participantes= str_trim(Instituciones_participantes)) %>%
    select(-info_5,-info_6) %>%
    mutate(info_7=str_trim(info_7),
           Autores= str_remove(info_7, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_7)

}

otra_publicacion_divulgativa_ucla <- function(grupo_df) {

  grupo_df_otra_publicacion_divulgativa <-
    grupo_df %>%
    filter(categoria == "Otra publicación divulgativa") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_Publicacion_divulgativa = str_remove(info_1, ":.*"),
           Tipo_Publicacion_divulgativa = str_remove(Tipo_Publicacion_divulgativa, ".*-" ),
           Tipo_Publicacion_divulgativa = str_trim( Tipo_Publicacion_divulgativa),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:" ),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           pais= str_remove(info_2, ",.*"),
           pais= str_trim(pais),
           ano=str_extract(info_2, ",.*"),
           ano = str_remove(ano, "^,"),
           info_2=str_extract(ano, ",.*"),
           ano= str_remove(ano, ",.*"),
           libro= str_remove(info_2, "^,"),
           libro= str_remove(libro, "vol.*"),
           libro= str_remove(libro, ",.$"),
           libro= str_extract(libro, ".*,"),
           libro= str_remove(libro, ",$"),
           libro=str_trim(libro),
           ISBN= str_remove(info_2, "^,"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ",.$"),
           ISBN= str_remove(ISBN, ".*,"),
           ISBN= str_trim(ISBN),
           volumen=str_extract(info_2, "vol.*"),
           volumen=str_remove(volumen, ",.*"),
           volumen=str_remove(volumen, "vol."),
           volumen=str_trim(volumen),
           Paginas=str_extract(info_2, "págs.*"),
           Paginas=str_remove(Paginas, ",.*"),
           Paginas=str_remove(Paginas, ".*:"),
           Paginas=str_trim(Paginas),
           Informacion=str_extract(info_2, "págs.*"),
           Informacion=str_extract(Informacion, ",.*"),
           Informacion=str_remove(Informacion, "^,"),
           info_2=str_extract(Informacion, ".*"),
           Informacion=str_extract(Informacion, ".*,"),
           Informacion=str_remove(Informacion, ",$"),
           Informacion=str_trim(Informacion),
           Informacion=str_remove(Informacion, "^-"),
           Informacion=str_trim(Informacion),
           Editorial=str_remove(info_2, ".*,"),
           Editorial=str_remove(Editorial, "Ed."),
           Editorial=str_trim(Editorial)
    ) %>%
    select(-info_2,-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_4)
}

documentos_trabajo_ucla <- function(grupo_df) {

  grupo_df_documentos_trabajo <-
    grupo_df %>%
    filter(categoria == "Documentos de trabajo") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5",
               "info_6", "info_7", "info_8", "info_9", "info_10", "info_11"),
             sep = "\r\n" ) %>%
    unite(info_4,"info_4",c("info_4","info_5", "info_6", "info_7", "info_8",
                            "info_9", "info_10", "info_11"),sep = " ",remove = TRUE) %>%
    separate(info_4, c("info_4","info_5"), sep = "URL") %>%
    separate(info_5, c("info_5", "info_6", "info_7"), sep = "DOI") %>%
    unite(info_6, "info_6", c("info_6", "info_7"),sep = "",remove = TRUE) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           ano = str_remove(info_2, ",$"),
           ano = str_trim(ano),
           numero_paginas = str_remove(info_3, ".*:"),
           numero_paginas = str_remove(numero_paginas, ",$"),
           numero_paginas = str_trim(numero_paginas),
           instituciones_participantes = str_remove(info_4, ".*:"),
           instituciones_participantes = str_remove(instituciones_participantes,","),
           instituciones_participantes = str_trim(instituciones_participantes),
           instituciones_participantes = str_remove(instituciones_participantes,",$"),
           URL = str_remove(info_5, "^:"),
           URL = str_remove(URL, ","),
           DOI = str_remove(info_6, "Autores.*"),
           DOI = str_remove(DOI, ":"),
           DOI = str_remove(DOI, ":"),
           DOI = str_trim(DOI),
           autores = str_remove(info_6, ".*Autores: "),
           autores = str_remove(autores, " NA .*")) %>%
    select(-info_1,-info_2,-info_3,-info_4,-info_5,-info_6)
}

ediciones_ucla <- function(grupo_df) {

  grupo_df_ediciones <-
    grupo_df%>%
    filter(categoria == "Ediciones") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Medio = str_remove(info_1, ":.*"),
           Medio = str_remove(Medio, ".*-"),
           Medio = str_trim(Medio),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Pais= str_trim(Pais),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ",$")) %>%
    select(-info_2) %>%
    mutate(info_3=str_trim(info_3),
           Editorial=str_remove(info_3, ",.*"),
           Editorial=str_remove(Editorial, ".*:"),
           Editorial=str_trim(Editorial),
           Idioma= str_remove(info_3, ".*:"),
           Idioma= str_remove(Idioma, ",$"),
           Idioma= str_trim(Idioma)) %>%
    select(-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Paginas= str_remove(info_4, ".*:"),
           Paginas= str_trim(Paginas)) %>%
    select(-info_4,-info_5) %>%
    mutate(info_6=str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_6)

}

estrategias_pedagogicas_ucla <- function(grupo_df) {

  grupo_df_estrategias_pedagogicas <-
    grupo_df %>%
    filter(categoria == "Estrategias Pedagógicas para el fomento a la CTI") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7",
               "info_8","info_9","info_10","info_11","info_12","info_13","info_14"
               ,"info_15","info_16","info_17","info_18","info_19","info_20","info_21",
               "info_22","info_23","info_24","info_25","info_26"),
             sep = "\r\n" ) %>%
    unite( info_3,c(4:27),  sep = ",", remove = TRUE) %>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_remove(info_1, "desde.*"),
           Titulo = str_remove(Titulo, ".*- "),
           Titulo= str_remove(Titulo, ": $"),
           Titulo = str_trim(Titulo),
           Fecha_inicio= str_extract(info_1, "desde.*"),
           Fecha_inicio= str_remove(Fecha_inicio, "desde"),
           Fecha_inicio= str_remove(Fecha_inicio, "hasta"),
           Fecha_inicio= str_trim(Fecha_inicio)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Fecha_Fin= str_extract(info_2, ".*")) %>%
    select(-info_2) %>%
    mutate(info_3= str_trim(info_3),
           Descripcion= str_remove(info_3, "Descripción:"),
           Descripcion= str_remove(Descripcion, "NA.*"),
           Descripcion= str_remove(Descripcion, ",$"),
           Descripcion= str_trim(Descripcion)) %>%
    select(-info_3)
}

redes_conocimiento_ucla <- function(grupo_df) {

  grupo_df_redes_conocimiento <-
    grupo_df %>%
    filter(categoria == "Redes de Conocimiento Especializado") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3"),
             sep = "\r\n" ) %>%
    mutate(titulo = str_remove(info_1, ":.*"),
           titulo = str_remove(titulo, ".*\\d.-"),
           titulo = str_trim(titulo),
           tipo_red = str_remove(info_1, ".*:"),
           tipo_red = str_trim(tipo_red),
           pais_ciudad = str_remove(info_2, ", desde.*"),
           pais_ciudad = str_remove(pais_ciudad, ".*en "),
           desde = str_remove(info_2, ".*desde "),
           desde = str_trim(desde),
           desde = str_remove(desde, "-$"),
           desde = str_trim(desde),
           hasta = str_remove(info_3, " Nro.*"),
           hasta = str_remove(hasta, ".*hasta "),
           numero_participantes = str_remove(info_3, ".*:"),
           numero_participantes = str_trim(numero_participantes)) %>%
    select(-info_1, -info_2, -info_3)


}

generacion_contenido_virtual_ucla <- function(grupo_df) {

  grupo_df_generacion_contenido_virtual <-
    grupo_df%>%
    filter(categoria == "Generación de Contenido Virtual") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Medio = str_remove(info_1, ":.*"),
           Medio = str_remove(Medio, ".*-"),
           Medio = str_trim(Medio),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2=str_trim(info_2),
           Fecha= str_remove(info_2, ",.*"),
           Fecha= str_trim(Fecha),
           Entidades_vinculadas= str_extract(info_2, ",.*"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, "^,"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, ".*:"),
           Entidades_vinculadas= str_remove(Entidades_vinculadas, ",$")) %>%
    select(-info_2) %>%
    mutate(info_3=str_trim(info_3),
           Sitio_web= str_remove(info_3, "Sitio web:"),
           Sitio_web= str_trim(Sitio_web))%>%
    select(-info_3,-info_4) %>%
    mutate(info_5=str_trim(info_5),
           Autores= str_remove(info_5, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_5)

}

espacios_participacion_ucla <- function(grupo_df) {

  grupo_df_espacio_participacion_ciudadano<-
    grupo_df %>%
    filter(categoria == "Espacios de Participación Ciudadana") %>%
    separate(producto ,
             c("info_1", "info_2","info_3"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ".*en"),
           Titulo = str_remove(Titulo, " en$"),
           Titulo = str_remove(Titulo, ".*- "),
           Titulo = str_trim(Titulo),
           Ciudad = str_remove(info_1,".*en"),
           Ciudad = str_trim(Ciudad)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Fecha_inicio = str_remove(info_2,"hasta.*"),
           Fecha_inicio = str_remove(Fecha_inicio, "desde"),
           Fecha_inicio = str_remove(Fecha_inicio, "- $"),
           Fecha_inicio =str_trim(Fecha_inicio),
           Fecha_Fin = str_extract(info_2, "hasta.*"),
           Fecha_Fin = str_remove(Fecha_Fin, "hasta"),
           Fecha_Fin = str_trim(Fecha_Fin)) %>%
    select(-info_2) %>%
    mutate(info_3 = str_trim(info_3),
           N_participantes= str_extract(info_3, ".*,"),
           N_participantes= str_remove(N_participantes, ".*:"),
           N_participantes= str_remove(N_participantes, ",$"),
           N_participantes= str_trim(N_participantes),
           Pag_web= str_extract(info_3, ",.*"),
           Pag_web= str_remove(Pag_web, "^,"),
           Pag_web= str_remove(Pag_web, ".*:"),
           Pag_web= str_trim(Pag_web)) %>%
    select(-info_3)

}

softwares_ucla <- function(grupo_df) {

  grupo_df_softwares <-
    grupo_df %>%
    filter(categoria == "Softwares") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6", "info_7"),
             sep = "\r\n" ) %>%
    select(-info_6) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo)) %>%
    separate(info_2,into=c("pais","anno"), sep = ",",remove = TRUE) %>%
    mutate(Pais = str_trim(pais),
           ano = str_trim(anno),
           disponibilidad = str_remove(info_3, ",.*"),
           disponibilidad = str_remove(disponibilidad, ".*:"),
           disponibilidad = str_remove(disponibilidad, "^:"),
           disponibilidad = str_trim(disponibilidad),
           sitio_web = str_remove(info_3, ".*web:"),
           sitio_web = str_trim(sitio_web),
           nombre_comercial = str_remove(info_4, ",.*"),
           nombre_comercial = str_remove(nombre_comercial, "Nombre comercial: "),
           nombre_comercial = str_trim(nombre_comercial),
           nombre_proyecto = str_remove(info_4, ".*Nombre del proyecto"),
           nombre_proyecto = str_remove(nombre_proyecto, "^:"),
           nombre_proyecto = str_trim(nombre_proyecto),
           institucion_financiadora = str_remove(info_5, ".*:"),
           institucion_financiadora = str_trim(institucion_financiadora),
           autores = str_remove(info_7, ".*Autores: ")) %>%
    select(-info_1,-pais,-anno,-info_3,-info_4,-info_5,-info_7)
}

innovaciones_procesos_ucla <- function(grupo_df) {

  grupo_df_inn_procesos_procedimientos <-
    grupo_df %>%
    filter(categoria == "Innovaciones en Procesos y Procedimientos") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    select(-info_4) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           disponibilidad = str_remove(info_3, ",.*"),
           disponibilidad = str_remove(disponibilidad, ".*: "),
           institucion_financadora = str_remove(info_3, ".*dora: "),
           autores = str_remove(info_5, ".*Autores: ")) %>%
    select(-info_1, -info_2, -info_3, -info_5)

}

otros_libros_ucla <- function(grupo_df) {
  grupo_df_otros_libros_publicados<-
    grupo_df %>%
    filter(categoria == "Otros Libros publicados") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Tipo_libro = str_remove(info_1, ":.*"),
           Tipo_libro = str_remove(Tipo_libro, ".*- "),
           Tipo_libro = str_trim(Tipo_libro),
           Titulo= str_extract(info_1, ":.*"),
           Titulo= str_remove(Titulo, "^:"),
           Titulo= str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2 = str_trim(info_2),
           Pais = str_remove(info_2,",.*"),
           Pais = str_trim(Pais),
           Ano = str_extract(info_2, ",.*"),
           Ano = str_remove(Ano, ","),
           Ano = str_trim(Ano),
           Ano = str_remove(Ano, ",.*"),
           ISBN= str_extract(info_2, "ISBN.*"),
           ISBN= str_remove(ISBN, "vol.*"),
           ISBN= str_remove(ISBN, ".*:"),
           ISBN = str_trim(ISBN),
           Volumen = str_extract(info_2,"vol:.*"),
           Volumen = str_remove(Volumen, "págs:.*"),
           Volumen= str_remove(Volumen, "vol:"),
           Volumen=str_trim(Volumen),
           Paginas= str_extract(info_2,"págs:.*,"),
           Paginas= str_remove(Paginas, ",.*"),
           Paginas= str_remove(Paginas, "págs:"),
           Paginas=str_trim(Paginas),
           Editorial= str_extract(info_2,"Ed.*"),
           Editorial= str_remove(Editorial,"Ed."),
           Editorial=str_trim(Editorial)) %>%
    select(-info_2,-info_3) %>%
    mutate(info_4=str_trim(info_4),
           Autores=str_extract(info_4, ".*"),
           Autores=str_remove(Autores, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_4)
}

estrategias_comunicacion_ucla <- function(grupo_df) {

  grupo_df_estreategias_comunicacion <-
    grupo_df %>%
    filter(categoria == "Estrategias de Comunicación del Conocimiento") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6",
               "info_7", "info_8", "info_9", "info_10", "info_11", "info_12",
               "info_13", "info_14", "info_15","info_16", "info_17", "info_18"),
             sep = "\r\n" ) %>%
    unite("info_3", c("info_3", "info_4", "info_5", "info_6",
                      "info_7", "info_8", "info_9", "info_10", "info_11", "info_12",
                      "info_13", "info_14", "info_15","info_16", "info_17", "info_18"),
          sep = " ",remove = TRUE) %>%
    mutate(tipo_producto = str_remove(info_1, ": desde.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           desde = str_remove(info_1, ".*desde "),
           desde = str_remove(desde, " hasta.*"),
           hata = str_trim(info_2),
           info_3 = str_remove(info_3, "NA.*"),
           descripcion = str_remove(info_3, ".*Descripción: "),
           descripcion = str_trim(descripcion)) %>%
    select(-info_1, -info_2, -info_3)

}

generacion_contenido_impreso_ucla <- function(grupo_df) {

  grupo_df_generacion_cont_impreso <-
    grupo_df %>%
    filter(categoria == "Generación de Contenido Impreso") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"),
             sep = "\r\n" ) %>%
    select(-info_5) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           fecha = str_remove(info_2, ", Ambito.*"),
           fecha = str_trim(fecha),
           ambito = str_remove(info_2, ".*: "),
           ambito = str_trim(ambito),
           ambito = str_remove(ambito, ","),
           medio_circulacion = str_remove(info_3, " Lugar.*"),
           medio_circulacion = str_remove(medio_circulacion, ".*: "),
           medio_circulacion = str_trim(medio_circulacion),
           lugar_publicacion = str_remove(info_3, ".*:"),
           lugar_publicacion = str_remove(lugar_publicacion, ","),
           lugar_publicacion = str_trim(lugar_publicacion),
           sitio_web = str_remove(info_4, ".*web: "),
           autores = str_remove(info_6, ".*Autores: "),
           autores = str_trim(autores)) %>%
    select(-info_1, -info_2, -info_3, -info_4, -info_6)

}

informes_tecnicos_ucla <- function(grupo_df) {

  grupo_df_informes_tecnicos<-
    grupo_df %>%
    filter(categoria == "Informes técnicos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1, -info_2) %>%
    separate(info_3,
             c("ano","mes","idioma","ciudad"),
             sep = ",") %>%
    mutate(ano = str_trim(ano),
           ano= str_remove(ano, ".*:"),
           Ano= str_extract(ano, ".*"),
           mes= str_trim(mes),
           mes= str_remove(mes, ".*:"),
           Mes= str_extract(mes, ".*"),
           idioma = str_trim(idioma),
           idioma= str_remove(idioma, ".*:"),
           Idioma= str_extract(idioma, ".*"),
           ciudad = str_trim(ciudad),
           ciudad= str_remove(ciudad, ".*:"),
           Ciudad = str_extract(ciudad, ".*"),
    ) %>%
    select(-info_4,-ano,-mes,-idioma,-ciudad) %>%
    separate(info_5,
             c("i_1","i_2","i_3","i_4"),
             sep = ",") %>%
    mutate(Disponibilidad= str_extract(i_1, ".*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Num_pag= str_extract(i_2, ".*"),
           Num_pag= str_remove(Num_pag, ".*:"),
           Num_pag= str_trim(Num_pag),
           Num_contrato= str_extract(i_3, ".*"),
           Num_contrato= str_remove(Num_contrato, ".*:"),
           Num_contrato= str_trim(Num_contrato),
           Institucion_Presta_servicio= str_extract(i_4, ".*"),
           Institucion_Presta_servicio= str_remove(Institucion_Presta_servicio, ".*:"),
           Institucion_Presta_servicio= str_trim(Institucion_Presta_servicio)) %>%
    select(-i_1,-i_2,-i_3,-i_4)
}

participacion_ciudadana_cti_ucla <- function(grupo_df) {

  grupo_df_participacion_cti <-
    grupo_df %>%
    filter(categoria == "Participación Ciudadana en Proyectos de CTI") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"),
             sep = "\r\n" ) %>%
    unite("info_3", c("info_3", "info_4", "info_5","info_6", "info_7", "info_8", "info_9"),
          sep = "",remove = TRUE) %>%
    mutate(titulo = str_remove(info_1, ":.*"),
           titulo = str_remove(titulo, ".*\\d.-"),
           titulo = str_trim(titulo),
           desde = str_remove(info_1, ".*: desde"),
           desde = str_remove(desde, "hasta.*"),
           desde = str_trim(desde),
           hasta = str_trim(info_2),
           descripcion = str_remove(info_3, ".*:"),
           descripcion = str_remove(descripcion, "NANANA.*")) %>%
    select(-info_1, -info_2, -info_3)
}

regulaciones_normas_ucla <- function(grupo_df) {

  grupo_df_regulaciones_normas <-
    grupo_df %>%
    filter(categoria == "Regulaciones y Normas") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6"),
             sep = "\r\n" ) %>%
    select(-info_5) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           ambito = str_remove(info_3, ", Fecha.*"),
           ambito = str_remove(ambito, ".*: "),
           fecha = str_remove(info_3, ".*publicación: "),
           fecha = str_trim(fecha),
           objeto = str_remove(info_4, ".*Objeto: "),
           objeto = str_trim(objeto),
           autores = str_remove(info_6, ".*Autores: ")) %>%
    select(-info_1, -info_2, -info_3, -info_4, -info_6)

}

actividades_evaluador_ucla <- function(grupo_df) {

  grupo_df_actividades_evaluador<-
    grupo_df %>%
    filter(categoria == "ACTIVIDADES COMO EVALUADOR")

}

actividades_formacion_ucla <- function(grupo_df) {

  grupo_df_actividades_formacion <-
    grupo_df %>%
    filter(categoria == "ACTIVIDADES DE FORMACIÓN")

}

apropiacion_social_conocimiento_ucla <- function(grupo_df) {

  grupo_df_apropiacion_social <-
    grupo_df %>%
    filter(categoria == "APROPIACIÓN SOCIAL Y CIRCULACIÓN DEL CONOCIMIENTO")

}

produccion_tecnica_tecnologica_ucla <- function(grupo_df) {

  grupo_df_produccion_tecnica_tecnologica<-
    grupo_df %>%
    filter(categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA")

}

generacion_contenido_audio_ucla <- function(grupo_df) {

  grupo_df_generacion_audio <-
    grupo_df %>%
    filter(categoria == "Generaciónes de contenido de audio") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    select(-info_2, -info_4) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d. "),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_extract(info_1, ":.*"),
           titulo = str_remove(titulo, "^:"),
           titulo = str_trim(titulo),
           ano = str_remove(info_3, ", Mes.*"),
           ano = str_remove(ano, ".*: "),
           mes = str_remove(info_3, ", Ciudad.*"),
           mes = str_remove(mes, ".*: "),
           ciudad = str_remove(info_3, ".*Ciudad: "),
           formato_archivo_digital = str_remove(info_5, ", Descripcion.*"),
           formato_archivo_digital = str_remove(formato_archivo_digital, ".*: "),
           descripcion_audio = str_remove(info_5, ".*audio: ")) %>%
    select(-info_1, -info_3, -info_5)

}

conceptos_tecnicos_ucla <- function(grupo_df) {

  grupo_df_conceptos_tecnicos <-
    grupo_df %>%
    filter(categoria == "Conceptos técnicos") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5"),
             sep = "\r\n" ) %>%
    select(-info_2, -info_4) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d."),
           tipo_producto = str_trim(tipo_producto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo),
           ano_solicitud = str_remove(info_3, ", Mes.*"),
           ano_solicitud = str_remove(ano_solicitud, ".*: "),
           ano_solicitud = str_trim(ano_solicitud),
           mes_solicitud = str_remove(info_3, ", Fecha.*"),
           mes_solicitud = str_remove(mes_solicitud, ".*: "),
           fecha_envio = str_remove(info_3, ".*envío: "),
           institucion_solicitante = str_remove(info_5, ", Ciudad.*"),
           institucion_solicitante = str_remove(institucion_solicitante, ".*solicitante: "),
           ciudad = str_remove(info_5, ", Número.*"),
           ciudad = str_remove(ciudad, ".*Ciudad: "),
           numero_cosecutivo_concepto = str_remove(info_5, ".*concepto: ")) %>%
    select(-info_1, -info_3, -info_5)

}

reglamentos_tecnicos_ucla <- function(grupo_df) {

  grupo_df_reglamentos_tecnicos <-
    grupo_df %>%
    filter(categoria == "Reglamentos técnicos") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5","info_6"),
             sep = "\r\n" ) %>%
    mutate(Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2= str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ","),
           Ano= str_trim(Ano)) %>%
    select(-info_2) %>%
    mutate(info_3 = str_trim(info_3),
           Disponibilidad= str_remove(info_3, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Sitio_Web= str_extract(info_3, ",.*"),
           Sitio_Web= str_remove(info_3, "^,"),
           Sitio_Web= str_remove(Sitio_Web, ".*:"),
           Sitio_Web= str_trim(Sitio_Web)) %>%
    select(-info_3) %>%
    mutate(info_4= str_trim(info_4),
           Institucion_Financiadora= str_remove(info_4, ".*:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>%
    select(-info_4,-info_5) %>%
    mutate(info_6= str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_6)
}

otros_productos_tencologicos_ucla <- function(grupo_df) {

  grupo_df_otros_productos_tecnologicos<-
    grupo_df %>%
    filter(categoria == "Otros productos tecnológicos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1) %>%
    mutate(info_2= str_trim(info_2),
           Pais= str_remove(info_2, ",.*"),
           Ano= str_extract(info_2, ",.*"),
           Ano= str_remove(Ano, "^,"),
           Ano= str_remove(Ano, ","),
           Ano= str_trim(Ano)) %>%
    select(-info_2) %>%
    mutate(info_3 = str_trim(info_3),
           Disponibilidad= str_remove(info_3, ",.*"),
           Disponibilidad= str_remove(Disponibilidad, ".*:"),
           Disponibilidad= str_trim(Disponibilidad),
           Nombre_comercial= str_extract(info_3, ",.*"),
           Nombre_comercial= str_remove(info_3, "^,"),
           Nombre_comercial= str_remove(Nombre_comercial, ".*:"),
           Nombre_comercial= str_trim(Nombre_comercial)) %>%
    select(-info_3) %>%
    mutate(info_4= str_trim(info_4),
           Institucion_Financiadora= str_remove(info_4, ".*:"),
           Institucion_Financiadora= str_trim(Institucion_Financiadora)) %>%
    select(-info_4,-info_5) %>%
    mutate(info_6= str_trim(info_6),
           Autores= str_remove(info_6, ".*:"),
           Autores= str_trim(Autores)) %>%
    select(-info_6)
}

traducciones_ucla <- function(grupo_df) {

  grupo_df_traducciones <-
    grupo_df %>%
    filter(categoria == "Traducciones") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3", "info_4", "info_5", "info_6","info_7", "info_8", "info_9"),
             sep = "\r\n" ) %>%
    select(-info_8) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_remove(info_1, ".*: "),
           ano = str_remove(info_2, ",.*"),
           ano = str_trim(ano),
           revista = str_remove(info_2, ".*: "),
           revista = str_remove(revista, " ISSN.*"),
           revista = str_trim(revista),
           ISSN = str_remove(info_3, ", Libro.*"),
           ISSN = str_trim(ISSN),
           libro = str_remove(info_3, ".*: "),
           libro = str_remove(libro, " ISBN.*"),
           ISBN = str_remove(info_4, ", Medio.*"),
           ISBN = str_trim(ISBN),
           medio_divulgacion = str_remove(info_4, ".*ción: "),
           idioma_documento_original = str_remove(info_5, ",.*"),
           idioma_documento_original = str_remove(idioma_documento_original, ".*: "),
           idioma_traduccion = str_remove(info_5, ".*ción: "),
           edicion = str_remove(info_6, ", Serie.*"),
           edicion = str_remove(edicion, ".*: "),
           serie = str_remove(info_6, ".*Serie: "),
           serie = str_remove(serie, ","),
           serie = str_trim(serie),
           autor_documento_original = str_remove(info_7, ".*: "),
           autores = str_remove(info_9, ".*: ")) %>%
    select(-info_1, -info_2, -info_3, -info_4,
           -info_5, -info_6, -info_7, -info_9)
}

signos_distintivos_ucla <- function(grupo_df){

  grupo_df_signos_distintivos <-
    grupo_df %>%
    filter(categoria == "Signos distintivos") %>%
    separate(producto ,
             c("info_1", "info_2", "info_3"),
             sep = "\r\n" ) %>%
    mutate(tipo_producto = str_remove(info_1, ":.*"),
           tipo_producto = str_remove(tipo_producto, ".*\\d.-"),
           tipo_producto = str_trim(tipo_producto),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo),
           pais = str_remove(info_2, ",.*"),
           pais = str_trim(pais),
           ano = str_trim(info_2),
           ano = str_remove(ano, ",$"),
           ano = str_remove(ano, ".*, "),
           numero_registro = str_remove(info_3, ", Nombre.*"),
           numero_registro = str_remove(numero_registro, ".*: "),
           nombre_titular = str_remove(info_3, ".*titular: ")) %>%
    select(-info_1, -info_2, -info_3)
}

nuevos_registros_cientificos_ucla <- function(grupo_df) {

  grupo_df_nuevos_registros_cientificos<-
    grupo_df %>%
    filter(categoria == "Nuevos registros científicos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8","info_9"),
             sep = "\r\n" )%>%
    mutate(info_1 = str_trim(info_1),
           Titulo = str_extract(info_1, ":.*"),
           Titulo = str_remove(Titulo, "^:"),
           Titulo = str_trim(Titulo)) %>%
    select(-info_1, -info_2) %>%
    mutate(info_3= str_trim(info_3),
           Ano= str_remove(info_3, ",.*"),
           Ano= str_remove(Ano, ".*:"),
           Ano= str_trim(Ano),
           Mes= str_extract(info_3, ",.*"),
           Mes= str_remove(Mes, "^,"),
           Mes= str_remove(Mes, ",.*"),
           Mes= str_remove(Mes, ".*:"),
           Mes= str_trim(Mes),
           Ciudad= str_extract(info_3, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_extract(Ciudad, ",.*"),
           Ciudad= str_remove(Ciudad, "^,"),
           Ciudad= str_remove(Ciudad, ".*:"),
           Ciudad= str_trim(Ciudad)) %>%
    select(-info_3, -info_4) %>%
    mutate(info_5= str_trim(info_5),
           Base_de_Datos= str_remove(info_5, ",.*"),
           Base_de_Datos= str_remove(Base_de_Datos, ".*:"),
           Base_de_Datos= str_trim(Base_de_Datos),
           Sitio_web= str_extract(info_5, ",.*"),
           Sitio_web= str_remove(Sitio_web, "^,"),
           Sitio_web= str_remove(Sitio_web, ",.*"),
           Sitio_web= str_remove(Sitio_web, ".*:"),
           Sitio_web= str_trim(Sitio_web),
           Institucion= str_extract(info_5, ",.*"),
           Institucion= str_remove(Institucion, "^,"),
           Institucion= str_extract(Institucion, ",.*"),
           Institucion= str_remove(Institucion, "^,"),
           Institucion= str_remove(Institucion, ".*:"),
           Institucion= str_trim(Institucion)) %>%
    select(-info_5, -info_6) %>%
    mutate(info_7= str_trim(info_7),
           Instituccion_certificadora= str_remove(info_7, ",.*"),
           Instituccion_certificadora= str_remove(Instituccion_certificadora, ".*:"),
           Instituccion_certificadora= str_trim(Instituccion_certificadora),
           Descripcion_registro= str_extract(info_7, ",.*"),
           Descripcion_registro= str_remove(Descripcion_registro, "^,"),
           Descripcion_registro= str_extract(Descripcion_registro, ":.*"),
           Descripcion_registro= str_remove(Descripcion_registro, "^:"),
           Descripcion_registro= str_trim(Descripcion_registro)) %>%
    select(-info_7, -info_8) %>%
    mutate(Descripcion=str_extract(info_9, ".*"),
           Descripcion= str_trim(Descripcion)) %>%
    select(-info_9)
}

libros_divulgacion_compilacion_ucla <- function(grupo_df) {

  grupo_df_libros_divulgacion_compilacion<-
    grupo_df %>%
    filter(categoria == "Libros de divulgación y/o Compilación de divulgación") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4",
               "info_5","info_6","info_7"),
             sep = "\r\n" ) |>
    select(-info_6) |>
    mutate(tipo_producto1 = str_remove(info_1, ".*.- "),
           tipo_producto = str_extract(tipo_producto1, ".*divulgación :"),
           tipo_producto = str_remove(tipo_producto, ":"),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_remove(tipo_producto1, ".*divulgación :"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ","),
           pais = str_trim(pais),
           ano = str_remove(info_3, ","),
           ano = str_trim(ano),
           ISBN = str_remove(info_4, ".*:"),
           ISBN = str_remove(ISBN, ","),
           ISBN = str_trim(ISBN),
           editorial = str_remove(info_5, ".*Ed. "),
           editorial = str_trim(editorial),
           autores = str_remove(info_7, ".*: "),
           autores = str_trim(autores)) |>
    select(-tipo_producto1, -info_1, -info_2, -info_3, -info_4,
           -info_5, -info_7)

}

libros_formacion_ucla <- function(grupo_df) {

  grupo_df_libros_formacion<-
    grupo_df %>%
    filter(categoria == "Libros de formación") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4",
               "info_5","info_6","info_7"),
             sep = "\r\n" ) |>
    select(-info_6) |>
    mutate(tipo_producto = str_extract(info_1, ".*formación : "),
           tipo_producto = str_remove(tipo_producto, ":"),
           tipo_producto = str_remove(tipo_producto, ".*.- "),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_remove(info_1, ".*formación :"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ","),
           pais = str_trim(pais),
           ano = str_remove(info_3, ","),
           ano = str_trim(ano),
           ISBN = str_remove(info_4, ".*:"),
           ISBN = str_remove(ISBN, ","),
           ISBN = str_trim(ISBN),
           editorial = str_remove(info_5, ".*Ed. "),
           editorial = str_trim(editorial),
           autores = str_remove(info_7, ".*: "),
           autores = str_trim(autores)) |>
    select(-info_1, -info_2, -info_3, -info_4,
           -info_5, -info_7)
}

producciones_digital_audiovisual_ucla <- function(grupo_df){

  grupo_df_producciones_digital_audiovisual <-
    grupo_df %>%
    filter(categoria == "Producciones de contenido digital - Audiovisual") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8"),
             sep = "\r\n" ) |>
    mutate(tipo_producto = str_remove(info_1, "\\d. "),
           tipo_producto = str_extract(tipo_producto, ".*Recurso gráfico\\)"),
           tipo_producto = str_trim(tipo_producto),
           titulo_producto = str_remove(info_1, ".*\\) "),
           titulo_producto = str_trim(titulo_producto),
           ano = str_remove(info_3, ".*Año: "),
           ano = str_remove(ano, ",.*"),
           ano = str_trim(ano),
           mes = str_remove(info_3, ".*Mes: "),
           mes = str_remove(mes, ",.*"),
           mes = str_trim(mes),
           verificacion = str_remove(info_3, ".*: "),
           verificacion = str_trim(verificacion),
           publico_objetivo = str_remove(info_5, ".*objetivo: "),
           publico_objetivo = str_remove(publico_objetivo, ",.*"),
           publico_objetivo = str_trim(publico_objetivo),
           cuidad = str_remove(info_5, ".*Ciudad: "),
           cuidad = str_remove(cuidad, ",.*"),
           cuidad = str_trim(cuidad),
           genero_literario = str_remove(info_5, ".*literario: "),
           genero_literario = str_remove(genero_literario, ",.*"),
           genero_literario = str_trim(genero_literario),
           tipo = str_remove(info_5, ".*Tipo: "),
           tipo = str_remove(tipo, ","),
           nombre_proyecto = str_remove(info_6, ".*proyecto: "),
           nombre_proyecto = str_remove(nombre_proyecto, ",.*"),
           nombre_proyecto = str_trim(nombre_proyecto),
           tipo_circulacion = str_remove(info_6, ".*circulación: "),
           tipo_circulacion = str_trim(tipo_circulacion),
           duracion = str_remove(info_7, ".*: ,"),
           enfoque_diferencial = str_remove(info_8, ".*:")) |>
    select(-info_1, -info_2, -info_3, -info_4,-info_5,-info_6,-info_7,-info_8)

}

Producciones_de_contenido_digital_ucla <- function(grupo_df){

  grupo_df_producciones_de_contenido_digital <-
    grupo_df %>%
    filter(categoria == "Producciones de contenido digital - Recursos gráficos") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8"),
             sep = "\r\n" ) |>
    mutate(tipo_producto = str_remove(info_1, "\\d. "),
           tipo_producto = str_extract(tipo_producto, ".*Recurso gráfico\\)"),
           tipo_producto = str_trim(tipo_producto),
           titulo_producto = str_remove(info_1, ".*\\) "),
           titulo_producto = str_trim(titulo_producto),
           ano = str_remove(info_3, ".*Año: "),
           ano = str_remove(ano, ",.*"),
           ano = str_trim(ano),
           mes = str_remove(info_3, ".*Mes: "),
           mes = str_remove(mes, ",.*"),
           mes = str_trim(mes),
           verificacion = str_remove(info_3, ".*: "),
           verificacion = str_trim(verificacion),
           publico_objetivo = str_remove(info_5, ".*objetivo: "),
           publico_objetivo = str_remove(publico_objetivo, ",.*"),
           publico_objetivo = str_trim(publico_objetivo),
           cuidad = str_remove(info_5, ".*Ciudad: "),
           cuidad = str_remove(cuidad, ",.*"),
           cuidad = str_trim(cuidad),
           genero_literario = str_remove(info_5, ".*literario: "),
           genero_literario = str_remove(genero_literario, ",.*"),
           genero_literario = str_trim(genero_literario),
           tipo = str_remove(info_5, ".*Tipo: "),
           tipo = str_remove(tipo, ","),
           nombre_proyecto = str_remove(info_6, ".*proyecto: "),
           nombre_proyecto = str_remove(nombre_proyecto, ",.*"),
           nombre_proyecto = str_trim(nombre_proyecto),
           tipo_circulacion = str_remove(info_6, ".*circulación: "),
           tipo_circulacion = str_trim(tipo_circulacion),
           duracion = str_remove(info_7, ".*: ,"),
           enfoque_diferencial = str_remove(info_8, ".*:")) |>
    select(-info_1, -info_2, -info_3, -info_4,-info_5,-info_6,-info_7,-info_8)

}

notas_cientificas_ucla <- function(grupo_df){

  grupo_df_notas_cientificas <-
    grupo_df %>%
    filter(categoria == "Notas científicas") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4","info_5","info_6","info_7","info_8", "info_9"
               , "info_10", "info_11", "info_12"),
             sep = "\r\n" ) |>
    mutate(nota_cientifica = str_remove(info_1, ".*científica: "),
           nota_cientifica = str_trim(nota_cientifica),
           revista = str_remove(info_3, ".*Revista: "),
           revista = str_remove(revista, ",.*"),
           revista = str_trim(revista),
           medio = str_trim(info_5),
           pagina_inicial = str_remove(info_8, ".*inicial: "),
           pagina_inicial = str_remove(pagina_inicial, ",.*"),
           pagina_final= str_remove(info_8, ".*final: "),
           ano = str_remove(info_10, ".*Año: "),
           ano = str_remove(ano, ",.*"),
           mes = str_remove(info_10, ".*Mes: "),
           sitio_web = str_extract(info_10, "https.*"),
           sitio_web = str_remove(sitio_web, ", DOI.*"),
           mes = str_remove(mes, ",.*"),
           volumen = str_remove(info_12, ".*men: "),
           volumen = str_remove(volumen, ",.*")) |>
    select(-info_1, -info_2, -info_3, -info_4,-info_5,-info_6,-info_7,-info_8,-info_9
           ,-info_10,-info_11,-info_12)
}

manuales_guias_especializadas_ucla <- function(grupo_df) {

  grupo_df_manuales_guias_especializadas <-
    grupo_df %>%
    filter(categoria == "Manuales y Guías Especializadas") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4",
               "info_5","info_6","info_7"),
             sep = "\r\n" ) |>
    mutate(tipo_producto = str_extract(info_1, ".*Especializadas : "),
           tipo_producto = str_remove(tipo_producto, ":"),
           tipo_producto = str_remove(tipo_producto, ".*.- "),
           tipo_producto = str_trim(tipo_producto),
           titulo = str_remove(info_1, ".*Especializadas :"),
           titulo = str_trim(titulo),
           pais = str_remove(info_2, ","),
           pais = str_trim(pais),
           ano = str_remove(info_3, ","),
           ano = str_trim(ano),
           ISBN = str_remove(info_4, ".*:"),
           ISBN = str_remove(ISBN, ","),
           ISBN = str_trim(ISBN),
           editorial = str_remove(info_5, ".*Ed. "),
           editorial = str_trim(editorial),
           autores = str_remove(info_7, ".*: "),
           autores = str_trim(autores)) |>
    select(-info_1, -info_2, -info_3, -info_4,
           -info_5, -info_6, -info_7)
}

divulgacion_publica_contenidos_transmedia_ucla <- function(grupo_df) {

  grupo_df_divulgacion_publica_contenidos_transmedia <-
    grupo_df %>%
    filter(categoria == "Divulgación Pública de la Ciencia producción de estrategias y contenidos transmedia") %>%
    separate(producto ,
             c("info_1", "info_2","info_3","info_4",
               "info_5","info_6","info_7"),
             sep = "\r\n" ) |>
    mutate(tipo_producto = str_extract(info_1, ".*transmedia"),
           tipo_producto = str_remove(tipo_producto, ".*\\d. "),
           titulo = str_remove(info_1, ".*transmedia"),
           titulo = str_trim(titulo),
           ano = str_remove(info_3, ".*Año: "),
           ano = str_remove(ano, ",.*"),
           ano = str_trim(ano),
           mes = str_remove(info_3, ".*Mes: "),
           mes = str_remove(mes, ",.*"),
           mes = str_trim(mes),
           verificacion = str_remove(info_3, ".*: "),
           verificacion = str_trim(verificacion),
           publico_objetivo = str_remove(info_5, ".*objetivo: "),
           publico_objetivo = str_remove(publico_objetivo, ",.*"),
           publico_objetivo = str_trim(publico_objetivo),
           cuidad = str_remove(info_5, ".*Ciudad: "),
           cuidad = str_remove(cuidad, ",.*"),
           cuidad = str_trim(cuidad),
           genero_literario = str_remove(info_5, ".*literario: "),
           genero_literario = str_remove(genero_literario, ",.*"),
           genero_literario = str_trim(genero_literario),
           nombre_proyecto = str_remove(info_6, ".*proyecto: "),
           nombre_proyecto = str_remove(nombre_proyecto, ",.*"),
           nombre_proyecto = str_trim(nombre_proyecto),
           tipo_circulacion = str_remove(info_6, ".*circulación: "),
           tipo_circulacion = str_trim(tipo_circulacion),
           enfoque_diferencial = str_remove(info_7, ".*:")) |>
    select(-info_1, -info_2,-info_3,-info_4,
           -info_5,-info_6,-info_7)
}

# export_csv <- function(shiny_data) {
#
#   write_csv(shiny_data[[1]],
#             here("output",
#                  "grupos_general.csv"))
#   write_csv(shiny_data[[2]][["trabajos_dirigidos"]],
#             here("output",
#                  "trabajos_dirigidos.csv"))
#   write_csv(shiny_data[[2]][["eventos_cientificos"]],
#             here("output",
#                  "eventos_cientificos.csv"))
#   write_csv(shiny_data[[2]][["articulos"]],
#             here("output",
#                  "articulos.csv"))
#   write_csv(shiny_data[[2]][["capitulos"]],
#             here("output",
#                  "capitulos.csv"))
#   write_csv(shiny_data[[2]][["jurado"]],
#             here("output",
#                  "jurado.csv"))
#   write_csv(shiny_data[[2]][["cursos"]],
#             here("output",
#                  "cursos.csv"))
#   write_csv(shiny_data[[2]][["otros_articulos"]],
#             here("output",
#                  "otros_articulos.csv"))
#   write_csv(shiny_data[[2]][["consultorias"]],
#             here("output",
#                  "consultorias.csv"))
#   write_csv(shiny_data[[2]][["libros"]],
#             here("output",
#                  "libros.csv"))
#   write_csv(shiny_data[[2]][["participacion_comites"]],
#             here("output",
#                  "participacion_comites.csv"))
#   write_csv(shiny_data[[2]][["demas_trabajos"]],
#             here("output",
#                  "demas_trabajos.csv"))
#   write_csv(shiny_data[[2]][["informes_investigacion"]],
#             here("output",
#                  "informes_investigacion.csv"))
#   write_csv(shiny_data[[2]][["innovaciones_gestion"]],
#             here("output",
#                  "innovaciones_gestion.csv"))
#   write_csv(shiny_data[[2]][["generacion_multimedia"]],
#             here("output",
#                  "generacion_multimedia.csv"))
#   write_csv(shiny_data[[2]][["otra_publicacion_divulgativa"]],
#             here("output",
#                  "otra_publicacion_divulgativa.csv"))
#   write_csv(shiny_data[[2]][["documentos_trabajo"]],
#             here("output",
#                  "documentos_trabajo.csv"))
#   write_csv(shiny_data[[2]][["ediciones"]],
#             here("output",
#                  "ediciones.csv"))
#   write_csv(shiny_data[[2]][["estrategias_pedagogicas"]],
#             here("output",
#                  "estrategias_pedagogicas.csv"))
#   write_csv(shiny_data[[2]][["redes_conocimiento"]],
#             here("output",
#                  "redes_conocimiento.csv"))
#   write_csv(shiny_data[[2]][["generacion_contenido_virtual"]],
#             here("output",
#                  "generacion_contenido_virtual.csv"))
#   write_csv(shiny_data[[2]][["espacios_participacion"]],
#             here("output",
#                  "espacios_participacion.csv"))
#   write_csv(shiny_data[[2]][["softwares"]],
#             here("output",
#                  "softwares.csv"))
#   write_csv(shiny_data[[2]][["innovaciones_procesos"]],
#             here("output",
#                  "innovaciones_procesos.csv"))
#   write_csv(shiny_data[[2]][["otros_libros"]],
#             here("output",
#                  "otros_libros.csv"))
#   write_csv(shiny_data[[2]][["estrategias_comunicacion"]],
#             here("output",
#                  "estrategias_comunicacion.csv"))
#   write_csv(shiny_data[[2]][["generacion_contenido_impreso"]],
#             here("output",
#                  "generacion_contenido_impreso.csv"))
#   write_csv(shiny_data[[2]][["informes_tecnicos"]],
#             here("output",
#                  "informes_tecnicos.csv"))
#   write_csv(shiny_data[[2]][["participacion_ciudadana_cti"]],
#             here("output",
#                  "participacion_ciudadana_cti.csv"))
#   write_csv(shiny_data[[2]][["regulaciones_normas"]],
#             here("output",
#                  "regulaciones_normas.csv"))
#   write_csv(shiny_data[[2]][["actividades_evaluador"]],
#             here("output",
#                  "actividades_evaluador.csv"))
#   write_csv(shiny_data[[2]][["actividades_formacion"]],
#             here("output",
#                  "actividades_formacion.csv"))
#   write_csv(shiny_data[[2]][["apropiacion_social_conocimiento"]],
#             here("output",
#                  "apropiacion_social_conocimiento.csv"))
#   write_csv(shiny_data[[2]][["produccion_tecnica_tecnologica"]],
#             here("output",
#                  "produccion_tecnica_tecnologica.csv"))
#   write_csv(shiny_data[[2]][["generacion_contenido_audio"]],
#             here("output",
#                  "generacion_contenido_audio.csv"))
#   write_csv(shiny_data[[2]][["conceptos_tecnicos"]],
#             here("output",
#                  "conceptos_tecnicos.csv"))
#   write_csv(shiny_data[[2]][["reglamentos_tecnicos"]],
#             here("output",
#                  "reglamentos_tecnicos.csv"))
#   write_csv(shiny_data[[2]][["otros_productos_tencologicos"]],
#             here("output",
#                  "otros_productos_tencologicos.csv"))
#   write_csv(shiny_data[[2]][["traducciones"]],
#             here("output",
#                  "traducciones.csv"))
#   write_csv(shiny_data[[2]][["signos_distintivos"]],
#             here("output",
#                  "signos_distintivos.csv"))
#   write_csv(shiny_data[[2]][["nuevos_registros_cientificos"]],
#             here("output",
#                  "nuevos_registros_cientificos.csv"))
#   write_csv(shiny_data[[2]][["notas_cientificas"]],
#             here("output",
#                  "notas_cientificas.csv"))
#   write_csv(shiny_data[[2]][["Producciones_de_contenido_digital"]],
#             here("output",
#                  "Producciones_de_contenido_digital.csv"))
#   write_csv(shiny_data[[2]][["libros_divulgacion"]],
#             here("output",
#                  "libros_divulgacion.csv"))
#   write_csv(shiny_data[[2]][["libros_formacion"]],
#             here("output",
#                  "libros_formacion.csv"))
#   write_csv(shiny_data[[2]][["Producciones_digital_audiovisual"]],
#             here("output",
#                  "Producciones_digital_audiovisual.csv"))
#   write_csv(shiny_data[[2]][["manuales_guias_especializadas"]],
#             here("output",
#                  "manuales_guias_especializadas.csv"))
#   write_csv(shiny_data[[2]][["divulgacion_publica_contenidos_transmedia"]],
#             here("output",
#                  "divulgacion_publica_contenidos_transmedia.csv"))
#   write_csv(shiny_data[[2]][["Eliminados_por_grupo"]],
#             here("output",
#                  "Eliminados_por_grupo.csv"))
#   write_csv(shiny_data[[2]][["Similares_entre_grupo"]],
#             here("output",
#                  "Similares_entre_grupo.csv"))
#   write_csv(shiny_data[[3]],
#             here("output",
#                  "investigadores.csv"))
#
#
# }

get_posgrade_clasficitation_cvlac <- function(cvlac_url) {

  cvlac_df = read_html(cvlac_url) |>
    html_table()

  cvlac_posgrade = cvlac_df[[1]] |>
    filter(str_detect(string = X1,
                      pattern = "Formación Académica")) |>
    select(X5, X7, X9) |>
    slice(1) |>
    separate_rows(X5, sep = "\r\n") |> # X5
    slice(1,4) |>
    mutate(X5 = str_trim(X5)) |>
    nest(data = X5) |>
    rename("X5" = data) |>
    separate_rows(X7, sep = "\r\n") |> # X7
    slice(1,4) |>
    mutate(X7 = str_trim(X7)) |>
    nest(data = X7) |>
    rename("X7" = data) |>
    separate_rows(X9, sep = "\r\n") |> # X9
    slice(1,4) |>
    mutate(X9 = str_trim(X9)) |>
    nest(data = X9) |>
    rename("X9" = data) |>
    unnest(cols = c(X5, X7, X9)) |>
    add_rownames() |>
    gather(var, value, -rowname) |>
    spread(rowname, value) |>
    select(-var) |>
    rename("posgrade" = 1,
           "duration" = 2) |>
    separate(duration, sep = " - ", into = c("start", "end")) |>
    filter(end != "de") |> # Removing not ending postgrades
    filter(posgrade %in% c("Doctorado",
                           "Maestría/Magister",
                           "Especialización",
                           "Pregrado/Universitario")) |>
    separate(end, into = c("Month", "year"), sep = " ") |>
    mutate(Month = if_else(Month == "de", "Enero", Month)) |>
    mutate(Month = str_remove(Month, "de"),
           end = str_c(Month, year, sep = " "),
           end = parse_date(end, "%B %Y", locale = locale("es"))) |>
    filter(end <= today()) |>
    mutate(ranking = if_else(posgrade == "Doctorado", 3,
                             if_else(posgrade == "Maestría/Magister", 2,
                                     if_else(posgrade == "Especialización", 1,
                                             0)
                                     )
                             )
           ) |>
    slice_max(ranking) |>
    select(posgrade) |>
    slice(1)

  if (is_empty(cvlac_posgrade$posgrade)) {

    cvlac_posgrade <-
      tibble(posgrade = "Técnico")

  }

  cvlac_category <- cvlac_df[[1]] |>
    filter(X1 == "Categoría") |>
    select(X2)

  if (is_empty(cvlac_category$X2)) {

    cvlac_category <-  tibble(clasification = "Sin clasificar")

  } else {

    cvlac_category <-
      cvlac_category |>
      mutate(clasification = str_extract(string = X2, pattern = ".*\\)")) |>
      select(clasification)
  }

  cvlac_posgrade_category <-
    cvlac_posgrade |>
    bind_cols(cvlac_category)


}

make_general_grupos <- function(produccion_actualizada){

  # # Count production of each group of active researchers
  # ## Identify active researchers with the group
  # researcher_active <-
  #   produccion_actualizada[[3]] |>
  #   select(grupo, integrantes) |>
  #   unique() |>
  #   mutate(integrantes = str_to_upper(integrantes),
  #          integrantes = stri_trans_general(str = integrantes,
  #                                           id = "Latin-ASCII"))
  #
  # # Identify the production of each researcher and count
  # group_production_general <-
  #   produccion_actualizada[[2]][["articulos"]] |>
  #   separate_rows(autores, sep = ", ") |>
  #   group_by(grupo) |>
  #   count(autores, sort = TRUE) |>
  #   rename("integrantes" = autores)
  #
  # # Merge active researches with production
  # group_production <-
  #   researcher_active |>
  #   left_join(group_production_general,
  #             by = c("grupo", "integrantes")) |>
  #   mutate(count_papers = ifelse(is.na(n), 0, n)) |>
  #   select(-integrantes, -n) |>
  #   group_by(grupo) |>
  #   summarize(sum_papers = sum(count_papers))
  #
  # # Merge group production with produccion actualizada
  # general_grupos <-
  #   produccion_actualizada[[1]]  |>
  #   left_join(group_production, by = "grupo") #<--- Datos elevados

  ## Revisar cantidad de articulos (esta treayendo mas del timepo 2016/20)

  general_grupos <- produccion_actualizada[[2]][["articulos"]] |>
    select(grupo, ano) |>
    filter(ano>=2016, ano<=2020) |>
    count(grupo, sort = T, name = "sum_papers") |>
    right_join(produccion_actualizada[[1]], by = "grupo") |>
    select(1,3:13,2)

  return(general_grupos)
}

count_articles_researcher <- function(produccion_actualizada) {

  # Count production of each group of active researchers
  ## Identify active researchers with the group
  researcher_active <-
    produccion_actualizada[[3]] |>
    ungroup() |>
    select(grupo, integrantes) |>
    unique() |>
    mutate(integrantes = str_to_upper(integrantes),
           integrantes = stri_trans_general(str = integrantes,
                                            id = "Latin-ASCII"))

  # Identify the production of each researcher and count
  group_production_general <- # Active and inactive researchers
    produccion_actualizada[[2]][["articulos"]] |>
    filter(ano>=2016, ano<=2020) |>
    separate_rows(autores, sep = ", ") |>
    group_by(grupo) |>
    count(autores, sort = TRUE) |>
    rename("integrantes" = autores)

  # Merge active researches with production
  researcher_production <-
    researcher_active |>
    separate_rows(grupo, sep = "; ") |>
    left_join(group_production_general,
              by = c("grupo", "integrantes")) |>
    mutate(count_papers = replace_na(data = n,
                                     replace = 0,)) |>
    select(integrantes, grupo, count_papers) |>
    group_by(integrantes) |>
    mutate(grupo = paste0(grupo,
                          collapse = "; "),
           count_papers = paste0(count_papers,
                                 collapse = "; "))


  # Merge group production with produccion actualizada
  researcher_general <-
    produccion_actualizada[[3]]  |>
    mutate(integrantes = str_to_upper(integrantes),
           integrantes = stri_trans_general(integrantes,
                                            id = "Latin-ASCII"),
           integrantes = str_squish(integrantes)) |>
    left_join(researcher_production,
              by = c("grupo","integrantes") ) |>
    unique()

  return(researcher_general)

}

researcher_product <- function(produccion_actualizada){

  articulos_author <-
    produccion_actualizada[[2]][["articulos"]] |>
    separate_rows(autores, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "autores" = "integrantes")) |>
    filter(!is.na(titulo)) |>
    group_by(grupo, autores) |>
    select(grupo, autores, titulo) |>
    unique() |>
    select(grupo, autores) |>
    count(autores) |>
    group_by(autores) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(articulos = n) |>
    separate_rows(c(grupo,articulos), sep = "; ")

  capitulos_author <-
    produccion_actualizada[[2]][["capitulos"]] |>
    separate_rows(autores, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "autores" = "integrantes")) |>
    filter(!is.na(titulo_capitulo)) |>
    group_by(grupo, autores) |>
    select(grupo, autores, titulo_capitulo) |>
    unique() |>
    select(grupo, autores) |>
    count(autores) |>
    group_by(autores) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(capitulos = n)|>
    separate_rows(c(grupo,capitulos), sep = "; ")

  libros_author <-
    produccion_actualizada[[2]][["libros"]] |>
    separate_rows(Autores, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "Autores" = "integrantes")) |>
    filter(!is.na(Titulo)) |>
    group_by(grupo, Autores) |>
    select(grupo, Autores, Titulo) |>
    unique() |>
    select(grupo, Autores) |>
    count(Autores) |>
    group_by(Autores) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(libros = n)|>
    separate_rows(c(grupo,libros), sep = "; ")

  software_author <-
    produccion_actualizada[[2]][["softwares"]] |>
    separate_rows(autores, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "autores" = "integrantes")) |>
    filter(!is.na(titulo)) |>
    group_by(grupo, autores) |>
    select(grupo, autores, titulo) |>
    unique() |>
    select(grupo, autores) |>
    count(autores) |>
    group_by(autores) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(softwares = n) |>
    separate_rows(c(grupo, softwares), sep = "; ")

  innovaciones_author <-
    produccion_actualizada[[2]][["innovaciones_gestion"]] |>
    separate_rows(autores, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "autores" = "integrantes")) |>
    filter(!is.na(titulo)) |>
    group_by(grupo, autores) |>
    select(grupo, autores, titulo) |>
    unique() |>
    select(grupo, autores) |>
    count(autores) |>
    group_by(autores) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(innovaciones = n) |>
    separate_rows(c(grupo, innovaciones), sep = "; ")

  trabajos_dirigidos_author <-
    produccion_actualizada[[2]][["trabajos_dirigidos"]] |>
    separate_rows(tutor_coautor, sep = ", ") |>
    right_join(produccion_actualizada[[3]] |>
                 separate_rows(grupo, sep = "; "),
               by = c("grupo" = "grupo",
                      "tutor_coautor" = "integrantes")) |>
    filter(!is.na(titulo)) |>
    group_by(grupo, tutor_coautor) |>
    select(grupo, tutor_coautor, titulo) |>
    unique() |>
    select(grupo, tutor_coautor) |>
    count(tutor_coautor) |>
    group_by(tutor_coautor) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           n = paste0(n, collapse = "; ")) |>
    unique() |>
    rename(trabajos_dirigidos = n) |>
    separate_rows(c(grupo, trabajos_dirigidos), sep = "; ")

  researcher_general <-
    produccion_actualizada[[3]] |>
    separate_rows(grupo, sep = "; ") |>
    left_join(articulos_author,
              by = c("integrantes" = "autores",
                     "grupo" = "grupo")) |>
    left_join(capitulos_author,
              by = c("integrantes" = "autores",
                     "grupo" = "grupo")) |>
    left_join(libros_author |>
                separate_rows(grupo, sep = "; "),
              by = c("integrantes" = "Autores",
                     "grupo" = "grupo")) |>
    left_join(software_author,
              by = c("integrantes" = "autores",
                     "grupo" = "grupo")) |>
    left_join(innovaciones_author,
              by = c("integrantes" = "autores",
                     "grupo" = "grupo")) |>
    left_join(trabajos_dirigidos_author,
              by = c("integrantes" = "tutor_coautor",
                     "grupo" = "grupo")) |>
    mutate(articulos = replace_na(articulos, 0),
           capitulos = replace_na(capitulos, 0),
           libros = replace_na(libros, 0),
           softwares = replace_na(softwares, 0),
           trabajos_dirigidos = replace_na(trabajos_dirigidos,0),
           innovaciones = replace_na(innovaciones, 0)) |>
    group_by(integrantes) |>
    mutate(grupo = paste0(grupo, collapse = "; "),
           articulos = paste0(articulos, collapse = "; "),
           capitulos = paste0(capitulos, collapse = "; "),
           libros = paste0(libros, collapse = "; "),
           softwares = paste0(softwares, collapse = "; "),
           trabajos_dirigidos = paste0(trabajos_dirigidos, collapse = "; "),
           innovaciones = paste0(innovaciones, collapse = "; "),
           horas_dedicacion = paste0(horas_dedicacion, collapse = "; ")) |>
    unique() |>
    filter(!duplicated(integrantes))

  return(researcher_general)
}
