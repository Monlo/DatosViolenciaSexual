#------------------------------------------------------------------------------#
# Impunidad Cero
# proyecto:         Violencia sexual
# fecha:            mayo 2022
# autoras:          Monserrat López y Helga Jáuregui
# num:              01 código para limpiar datos de subsistema lesiones
#------------------------------------------------------------------------------#

## Setup ----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # locale para Mac
Sys.setlocale("LC_ALL", "Spanish") # locale para Windows
options(scipen = 999)

## Paquetes ----

pacman::p_load(tidyverse, janitor, rio)

## Datos ----
# Las tablas que integran la base vienen en .txt son csv

### Paso 1: importamos todos los datos ----
evento <- read_csv("1_Datos/Registros/D_Evento.txt")
evento %>% dim() # se registran 496,335 atenciones por lesiones

atencion <- read_csv("1_Datos/Registros/D_TipoAtencion.txt")
atencion %>% dim() # solo hay info del tipo de atención para 471,160 casos

efectos <- read_csv("1_Datos/Registros/D_BajoEfectos.txt")
efectos %>%  dim() # sólo hay info sobre sustancias para 438,646 casos

violencia <- read_csv("1_Datos/Registros/D_TipoViolencia.txt")
violencia %>% dim() # se registra violencia en 121,812 casos

diagnostico <- read_csv("1_Datos/Registros/D_Diagnosticos.txt")
diagnostico %>% dim() # registra el diagnóstico en 361,736 casos

### Paso 2: unimos los datos en 1 sola base ----
# OJO las dimensiones de la base crecen porque la relación es 1:m 
# hay que tener cuidado al reportar las cifras**


# unimos tipo de atención
bd <- evento %>% 
  left_join(y = atencion, by = c("ID", "Clues", "FechaAtencion")) 
bd %>% dim() # 44 variables

# unimos tipo de violencia
bd <- bd %>% 
  left_join(y = violencia, by = c("ID", "Clues", "FechaAtencion"))
bd %>% dim() # 45 variables

# unimos efectos por tipo de sustancia
bd <- bd %>% 
  left_join(y = efectos, by = c("ID", "Clues", "FechaAtencion" = "fecha_atencion"))
bd %>% dim() # 47 variables

# unimos con diagnósticos

bd <- bd %>% 
  left_join(y = diagnostico, by = c("ID", "Clues", "FechaAtencion"))
bd %>% dim() 3# 49 variables

### Paso 3: filtramos para conservar sólo registros de violencia sexual ----

bd <- bd %>% 
  filter(CodTipoViolencia == 7)

### Paso 4: Selección de variables de interés

bd %>% glimpse()
bd <- bd %>% 
  select(ID:SEXO, CodEscolaridad:CodMunicipio, CodDiscapacidad:CodAgente, CodAgresor:CodParentescoAgresor, CodMP,CodTipoAtencion:Orden)

bd %>% dim() # hay 40,715 filas

bd$ID %>% 
  unique() %>% 
  length() # solo 11,885 casos diferentes registrados

## Exportar base limpia ----

bd %>% 
  rio::export("3_Out/violencia_sexual_2020.csv")
