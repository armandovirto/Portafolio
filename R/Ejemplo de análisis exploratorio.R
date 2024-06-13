##########################################################################-
#Práctica 3: Análisis exploratorio
#Autor: Armando Virto
#Fecha: 2014-06-12
#Dataset: Elaboración propia
##########################################################################-

# 1. Cargar paquetes ----

pacman::p_load(tidyverse,
               writexl, 
               haven,
               sjlabelled, 
               janitor,
               magrittr,
               GGally,
               wesanderson,
               gt,
               pollster,
               dineq)

# 2. Importar datos ----

dataset_retraso <- read_excel("~/Desktop/PROYECTOS/DRA. ESPINOZA/ANÁLISIS FINAL DRA. ESPINOZA/BASE DE DATOS RECORTADA DRA. ESPINOZA.xlsx",
col_types = c("numeric", "numeric", "numeric",
"date", "numeric", "date", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric", "numeric",
"numeric", "numeric"))

View(dataset_retraso)

# 3. Revisión de dataframes ----

names(dataset_retraso)
class(dataset_retraso)
head(dataset_retraso, n = 10)
tail(dataset_retraso)
str(dataset_retraso)
skimr::skim(dataset_retraso)

# 4. Limpiar nombres ----

dataset_retraso <- dataset_retraso |> 
        janitor::clean_names()

# 5. Segunda revisión (con glimpse) ----

dataset_retraso |> 
        dplyr::glimpse()

# 6. Crear etiquetas  ----

etiqueta_sexo <- c("Hombre", "Mujer")
etiqueta_mortalidad <- c("Sí", "No")
etiqueta_fiebre <- c("Sí", "No")
etiqueta_día_semana <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
etiqueta_turno <- c("Matutino", "Verpertino", "Nocturno")
etiqueta_diagnóstico <- c("Trauma", "Sepsis", "Quemaduras", "Neumonía", "Epilepsia", "Choque hipovolémico", "Cetoacidosis", "Otros")

# 7. Asignar etiquetas a las variables  ----

dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(sexo = sjlabelled::set_labels(sexo, labels = etiqueta_sexo)) 

# Corroborar asignación

class(dataset_retraso$sexo)
class(sjlabelled::as_label(dataset_retraso$sexo))

table(dataset_retraso$sexo)
table(sjlabelled::as_label(dataset_retraso$sexo))

dataset_retraso |> 
        dplyr::mutate(sexo = as_label(sexo)) |> 
        janitor::tabyl(sexo)

dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(mortalidad = sjlabelled::set_labels(mortalidad, labels = etiqueta_mortalidad)) 
dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(fiebre_y_neutropenia = sjlabelled::set_labels(fiebre_y_neutropenia, labels = etiqueta_fiebre)) 
dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(dia_de_la_semana_de_ingreso = sjlabelled::set_labels(dia_de_la_semana_de_ingreso, labels = etiqueta_día_semana)) 
dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(turno_de_ingreso = sjlabelled::set_labels(turno_de_ingreso, labels = etiqueta_turno)) 
dataset_retraso <- dataset_retraso |> 
        dplyr::mutate(diagnostico_clasificado_de_acuerdo_a_protocolo = sjlabelled::set_labels(diagnostico_clasificado_de_acuerdo_a_protocolo, labels = etiqueta_diagnóstico)) 


class(sjlabelled::as_label(dataset_retraso$mortalidad))
table(sjlabelled::as_label(dataset_retraso$mortalidad))
dataset_retraso |> 
        dplyr::mutate(mortalidad = as_label(mortalidad)) |> 
        janitor::tabyl(mortalidad)

class(sjlabelled::as_label(dataset_retraso$fiebre_y_neutropenia))
table(sjlabelled::as_label(dataset_retraso$fiebre_y_neutropenia))
dataset_retraso |> 
        dplyr::mutate(fiebre_y_neutropenia = as_label(fiebre_y_neutropenia)) |> 
        janitor::tabyl(fiebre_y_neutropenia)

class(sjlabelled::as_label(dataset_retraso$dia_de_la_semana_de_ingreso))
table(sjlabelled::as_label(dataset_retraso$dia_de_la_semana_de_ingreso))
dataset_retraso |> 
        dplyr::mutate(dia_de_la_semana_de_ingreso = as_label(dia_de_la_semana_de_ingreso)) |> 
        janitor::tabyl(dia_de_la_semana_de_ingreso)

class(sjlabelled::as_label(dataset_retraso$turno_de_ingreso))
table(sjlabelled::as_label(dataset_retraso$turno_de_ingreso))
dataset_retraso |> 
        dplyr::mutate(turno_de_ingreso = as_label(turno_de_ingreso)) |> 
        janitor::tabyl(turno_de_ingreso)

class(sjlabelled::as_label(dataset_retraso$diagnostico_clasificado_de_acuerdo_a_protocolo))
table(sjlabelled::as_label(dataset_retraso$diagnostico_clasificado_de_acuerdo_a_protocolo))
dataset_retraso |> 
        dplyr::mutate(diagnostico_clasificado_de_acuerdo_a_protocolo = as_label(diagnostico_clasificado_de_acuerdo_a_protocolo)) |> 
        janitor::tabyl(diagnostico_clasificado_de_acuerdo_a_protocolo)

# 8. Tablas de frecuencia y porcentaje de variables cualitativas  ----

tabla_sexo <- dataset_retraso |> 
        dplyr::mutate(sexo = as_label(sexo)) |> 
        janitor::tabyl(sexo, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

tabla_mortalidad <- dataset_retraso |> 
        dplyr::mutate(mortalidad = as_label(mortalidad)) |> 
        janitor::tabyl(mortalidad, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

tabla_fiebre <- dataset_retraso |> 
        dplyr::mutate(fiebre_y_neutropenia = as_label(fiebre_y_neutropenia)) |> 
        janitor::tabyl(fiebre_y_neutropenia, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

tabla_día_semana <- dataset_retraso |> 
        dplyr::mutate(dia_de_la_semana_de_ingreso = as_label(dia_de_la_semana_de_ingreso)) |> 
        janitor::tabyl(dia_de_la_semana_de_ingreso, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

tabla_turno <- dataset_retraso |> 
        dplyr::mutate(turno_de_ingreso = as_label(turno_de_ingreso)) |> 
        janitor::tabyl(turno_de_ingreso, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

tabla_diagnóstico <- dataset_retraso |> 
        dplyr::mutate(diagnostico_clasificado_de_acuerdo_a_protocolo = as_label(diagnostico_clasificado_de_acuerdo_a_protocolo)) |> 
        janitor::tabyl(diagnostico_clasificado_de_acuerdo_a_protocolo, show_missing_levels=F) |> 
        janitor::adorn_totals() |> 
        janitor::adorn_pct_formatting()

# 9. Exploraciones bivariadas de interés  ----

# sexo * mortalidad

tabla_sexo_mortalidad_1 <- dataset_retraso %>% 
        dplyr::mutate(sexo = as_label(sexo)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(sexo, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) 

tabla_sexo_mortalidad_2 <- dataset_retraso %>% 
        dplyr::mutate(sexo = as_label(sexo)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(sexo, mortalidad, show_missing_levels = F) %>%
        janitor::adorn_totals(c("col", "row")) %>% 
        janitor::adorn_percentages("all") %>% 
        janitor::adorn_pct_formatting()

# día de la semana * mortalidad

tabla_semana_mortalidad_1 <- dataset_retraso %>% 
        dplyr::mutate(dia_de_la_semana_de_ingreso = as_label(dia_de_la_semana_de_ingreso)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(dia_de_la_semana_de_ingreso, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row"))

tabla_semana_mortalidad_2 <- dataset_retraso %>% 
        dplyr::mutate(dia_de_la_semana_de_ingreso = as_label(dia_de_la_semana_de_ingreso)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(dia_de_la_semana_de_ingreso, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) |> 
        janitor::adorn_percentages("all") %>%
        janitor::adorn_pct_formatting()

# turno * mortalidad

tabla_turno_mortalidad_1 <- dataset_retraso %>% 
        dplyr::mutate(turno_de_ingreso = as_label(turno_de_ingreso)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(turno_de_ingreso, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) 

tabla_turno_mortalidad_2 <- dataset_retraso %>% 
        dplyr::mutate(turno_de_ingreso = as_label(turno_de_ingreso)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(turno_de_ingreso, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) |> 
        janitor::adorn_percentages("all") %>%
        janitor::adorn_pct_formatting()

# diagnóstico * mortalidad

tabla_mortalidad_diagnostico_1 <- dataset_retraso %>% 
        dplyr::mutate(diagnostico_clasificado_de_acuerdo_a_protocolo = as_label(diagnostico_clasificado_de_acuerdo_a_protocolo)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(diagnostico_clasificado_de_acuerdo_a_protocolo, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) 

tabla_mortalidad_diagnostico_2 <- dataset_retraso %>% 
        dplyr::mutate(diagnostico_clasificado_de_acuerdo_a_protocolo = as_label(diagnostico_clasificado_de_acuerdo_a_protocolo)) %>% 
        dplyr::mutate(mortalidad = as_label(mortalidad)) %>% 
        janitor::tabyl(diagnostico_clasificado_de_acuerdo_a_protocolo, mortalidad, show_missing_levels=F) %>% 
        janitor::adorn_totals(c("col", "row")) |> 
        janitor::adorn_percentages("all") %>%
        janitor::adorn_pct_formatting()

# 10. Estadísticos descriptivos de variables cuantitativas ----

tabla_retraso <- dataset_retraso %>%
        dplyr:: summarise(media_retraso = mean(minutos_de_restraso, na.rm = T),
                          sd_retraso = sd(minutos_de_restraso, na.rm = T),
                          mediana_retraso = median(minutos_de_restraso, na.rm = T),
                          minimo_retraso = min(minutos_de_restraso, na.rm = T),
                          maximo_retraso = max(minutos_de_restraso, na.rm =T))

tabla_días_terapia <- dataset_retraso %>%
        dplyr:: summarise(media_terapia = mean(dias_de_estancia_en_terapia, na.rm = T),
                          sd_terapia = sd(dias_de_estancia_en_terapia, na.rm = T),
                          mediana_terapia = median(dias_de_estancia_en_terapia, na.rm = T),
                          minimo_terapia = min(dias_de_estancia_en_terapia, na.rm = T),
                          maximo_terapia = max(dias_de_estancia_en_terapia, na.rm =T))

tabla_días_piso <- dataset_retraso %>%
        dplyr:: summarise(media_piso = mean(dias_de_hospitalizacion, na.rm = T),
                          sd_piso = sd(dias_de_hospitalizacion, na.rm = T),
                          mediana_piso = median(dias_de_hospitalizacion, na.rm = T),
                          minimo_piso = min(dias_de_hospitalizacion, na.rm = T),
                          maximo_piso = max(dias_de_hospitalizacion, na.rm =T))

tabla_horas_vasoactivo <- dataset_retraso %>%
        dplyr:: summarise(media_vaso = mean(horas_de_vasoactivo, na.rm = T),
                          sd_vaso = sd(horas_de_vasoactivo, na.rm = T),
                          mediana_vaso = median(horas_de_vasoactivo, na.rm = T),
                          minimo_vaso = min(horas_de_vasoactivo, na.rm = T),
                          maximo_vaso = max(horas_de_vasoactivo, na.rm =T))

tabla_horas_vm <- dataset_retraso %>%
        dplyr:: summarise(media_vm = mean(horas_de_ventilacion_mecanica, na.rm = T),
                          sd_vm = sd(horas_de_ventilacion_mecanica, na.rm = T),
                          mediana_vm = median(horas_de_ventilacion_mecanica, na.rm = T),
                          minimo_vm = min(horas_de_ventilacion_mecanica, na.rm = T),
                          maximo_vm = max(horas_de_ventilacion_mecanica, na.rm =T))

# 11. Exportar tablas para elaborar reporte ----

lista_resultados <- list(sexo = tabla_sexo,
                         mortalidad = tabla_mortalidad,
                         fiebre = tabla_fiebre,
                         día_semana = tabla_día_semana,
                         turno = tabla_turno,
                         diagnóstico = tabla_diagnóstico,
                         freq_sexo_mor = tabla_sexo_mortalidad_1,
                         porc_sexo_mor = tabla_sexo_mortalidad_2,
                         freq_semana_mor = tabla_semana_mortalidad_1,
                         porc_semana_mor = tabla_semana_mortalidad_2,
                         freq_turno_mor = tabla_turno_mortalidad_1,
                         porc_turno_mor = tabla_turno_mortalidad_2,
                         freq_diag_mor = tabla_mortalidad_diagnostico_1,
                         porc_diag_mor = tabla_mortalidad_diagnostico_2,
                         minutos_retraso = tabla_retraso,
                         días_terapia = tabla_días_terapia,
                         días_piso = tabla_días_piso,
                         horas_vasoactivo = tabla_horas_vasoactivo,
                         horas_vm = tabla_horas_vm)

writexl::write_xlsx(lista_resultados, path = "Datos/Resultados_practica_3.xlsx")

# 12. Visualización exploratoria de variables cuantitativas ----

hist(dataset_retraso$minutos_de_restraso)
hist(log(dataset_retraso$minutos_de_restraso))

hist(dataset_retraso$minutos_de_restraso, 
     main="Minutos de retraso en el ingreso a terapia intensiva",
     xlab="Minutos", 
     ylab="Frecuencia", col="blue") 

boxplot(dataset_retraso$minutos_de_restraso, 
     main="Minutos de retraso en el ingreso a terapia intensiva",
     ylab="Minutos", col="blue")

hist(dataset_retraso$dias_de_estancia_en_terapia,  
     main="Días de estancia en terapia intensiva",
     xlab="Días", 
     ylab="Frecuencia", col="blue") 

boxplot(dataset_retraso$dias_de_estancia_en_terapia, 
        main="Días de estancia en terapia intensiva",
        ylab="Días", col="blue")

hist(dataset_retraso$dias_de_hospitalizacion, 
     main="Días totales de estancia hospitalaria",
     xlab="Días", 
     ylab="Frecuencia", col="blue") 

boxplot(dataset_retraso$dias_de_hospitalizacion, 
        main="Días totales de estancia hospitalaria",
        ylab="Días", col="blue")

hist(dataset_retraso$horas_de_vasoactivo, 
     main="Horas de vasoactivo",
     xlab="Horas", 
     ylab="Frecuencia", col="blue") 

boxplot(dataset_retraso$horas_de_vasoactivo, 
        main="Horas de vasoactivo",
        ylab="Horas", col="blue")

hist(dataset_retraso$horas_de_ventilacion_mecanica, 
     main="Horas de ventilación mecánica",
     xlab="Horas", 
     ylab="Frecuencia", col="blue") 

boxplot(dataset_retraso$horas_de_ventilacion_mecanica, 
        main="Horas de ventilación mecánica",
        ylab="Horas", col="blue")