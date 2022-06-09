#------------------------------------------------------------------------------#
# Impunidad Cero
# proyecto:         Violencia sexual
# fecha:            mayo 2022
# autoras:          Monserrat López y Helga Jáuregui
# num:              02 código para analizar datos del subsistema de lesiones
#------------------------------------------------------------------------------#

## Setup ----

Sys.setlocale("LC_ALL", "es_ES.UTF-8") # locale para Mac
Sys.setlocale("LC_ALL", "Spanish") # locale para Windows
options(scipen = 999)

## Paquetes ----

pacman::p_load(tidyverse, janitor, treemapify, patchwork)

## Datos ----

bd <- read_csv("3_Out/violencia_sexual_2020.csv") %>% 
  clean_names()

# Exploramos datos:

bd %>% glimpse()
bd %>% head()

### Catálogos ----

estados <- read_csv("1_Datos/Catalogos_Lesiones_2020/cEstados.csv") %>% 
  clean_names()

agente <- read_csv("1_Datos/Catalogos_Lesiones_2020/cAgente.csv") %>% 
  clean_names()

escolaridad <- read_csv("1_Datos/Catalogos_Lesiones_2020/cEscolaridad.csv") %>% 
  clean_names()

sitio <- read_csv("1_Datos/Catalogos_Lesiones_2020/cSitio.csv") %>% 
  clean_names()

agresor <- read_csv("1_Datos/Catalogos_Lesiones_2020/cAgresor.csv") %>% 
  clean_names() 

atencion <- read_csv("1_Datos/Catalogos_Lesiones_2020/cTipoAtencion.csv") %>% 
  clean_names()

### Casos unicos ----

# vector:

ids_unicos <- bd$id %>% 
  unique()

# tamaño del vector para calcular proporciones correctamente:

num_ids_unicos <- bd$id %>% 
  unique() %>% 
  length()

## Calculo de descriptivos generales ----

#### ¿Cuántos casos son de mujeres y cuántos de hombres? ----

bd %>% 
  select(id, sexo) %>% 
  distinct() %>% # filtramos para coservar ids unicos
  count(sexo) %>% 
  mutate(porcentaje = round(n/sum(n)*100, 1)) %>% 
  filter(sexo != 9) %>% 
  mutate(sexo = if_else(condition = sexo ==1,
                        true = "Hombres", 
                        false = "Mujeres")) %>% 
  ggplot(aes(x = sexo, y = porcentaje, fill = sexo)) +
  geom_col(width = .7) +
  geom_text(aes(label = str_glue("{porcentaje}%")), 
            nudge_y = 4, size = 7) +
  scale_fill_manual(values = c("#949d6a","#4A1486"))+
  labs(title = "Las mujeres son las mayores víctimas de violencia sexual",
       subtitle = "Atenciones por lesiones asociadas a violencia sexual según sexo",
       caption = "Fuente: Base de Datos de Lesiones y Causas de Violencia, Secretaría de Salud 2020") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "none",
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 25),
        strip.text = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 22),
        plot.caption = element_text(size = 18))

ggsave("4_Graphs/casos_sexo.png", width = 16, height = 9, dpi = 200)  
  
  
### ¿Cuáles son los estados con más registros? ----
# Totales :
bd %>% 
  select(id, cod_estado, sexo) %>% 
  distinct() %>% # filtramos para conservar ids unicos
  group_by(cod_estado) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% 
  left_join(y = estados, by = "cod_estado") %>% 
  mutate(porcentaje = round(total/sum(total)*100,2))

## por sexo: 
bd %>% 
  select(id, cod_estado, sexo) %>% 
  distinct() %>% # filtramos para conservar ids unicos
  group_by(cod_estado,sexo) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% 
  left_join(y = estados, by = "cod_estado") %>% 
  mutate(porcentaje = round(total/sum(total)*100,2))

### ¿Cuál es el agente que se utilizó? ----

bd %>% 
  select(id, sexo, cod_agente) %>% 
  distinct() %>% 
  count(cod_agente) %>% 
  mutate(porcentaje = round(n/sum(n)*100, 2)) %>% 
  ungroup() %>% 
  left_join(y = agente, by = "cod_agente") %>% 
  arrange(-porcentaje)

## ¿En cuántos casos se dio aviso al MP? ----

# totales:
bd %>% 
  select(id, sexo, cod_mp) %>% 
  distinct() %>% 
  count(cod_mp) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))

# por sexo:
bd %>% 
  select(id, sexo, cod_mp) %>% 
  group_by(sexo) %>% 
  distinct() %>% 
  count(cod_mp) %>% 
  ungroup() %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))

# porcentaje donde si se dio aviso por sexo:
bd %>% 
  select(id, sexo, cod_mp) %>% 
  group_by(sexo) %>% 
  distinct() %>% 
  count(cod_mp) %>% 
  ungroup() %>% 
  filter(cod_mp ==1) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))

###  ¿Qué escolaridad tenían las personas atendidas? ----

bd %>% 
  select(id, sexo, cod_escolaridad) %>% 
  distinct() %>% 
  group_by(sexo) %>% 
  count(cod_escolaridad) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2)) %>% 
  left_join(y = escolaridad, by = "cod_escolaridad") %>% 
  arrange(sexo, -porcentaje) %>% 
  print(n = Inf)


### ¿Qué edad tenían las personas atendidad? ----

bd$cveedad %>% unique() # 3 días, 4 meses, 5 años, 9 no especificado

#### Para menores de 1 año de edad 
bd %>% 
  select(id, sexo, cveedad, edad) %>% 
  distinct() %>% 
  filter(cveedad %in% c(3,4)) %>%  # 16 casos registrados para menores de un año
count(sexo) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))
  

bd %>% 
  select(id, sexo, cveedad, edad) %>% 
  distinct() %>% 
  filter(cveedad %in% c(3,4)) %>%  # 16 casos registrados para menores de un año
  mutate(edad_dias = if_else(condition = cveedad == 3, true = edad, false = edad*30)) %>% 
  group_by(sexo) %>% 
  summarise(media = mean(edad_dias))

#### Para mayores de un año 

bd %>% 
  select(id, sexo, cveedad, edad) %>% 
  distinct() %>% 
  filter(cveedad == 5 & sexo!=9) %>%
  mutate(edad_cat = case_when( edad >= 1 & edad <= 12 ~ "1-12 años",
                               edad >= 13 & edad <= 19 ~ "13-19 años",
                               edad >= 20 & edad <= 30 ~ "20-30 años",
                               edad >= 31 & edad <= 40 ~ "31-40 años",
                               edad >= 41 & edad <= 50 ~ "41-50 años",
                               edad >= 51 & edad <= 60 ~ "51-60 años",
                               edad >= 61 ~ "Mayores de 61")) %>% 
  group_by(sexo) %>% 
  count(edad_cat) %>% 
  mutate(porcentaje = round(n/sum(n)*100, 2),
         sexo = if_else(condition = sexo == 1, true = "Hombres", "Mujeres")) %>% 
  arrange(sexo, -porcentaje) %>% 
  ggplot(aes(x = porcentaje, y = edad_cat, fill = sexo)) +
  geom_col() +
  geom_text(aes(label =str_glue("{porcentaje}%")), nudge_x = 5, size = 7) +
  scale_fill_manual(values = c("#949d6a","#4A1486"))+
  labs(title = str_wrap("Las niñas, niños y adolescentes son los que más atención médica reciben por violencia sexual",
                        width = 70),
       subtitle = "Porcentaje de lesiones asociadas a la violencia sexual, según sexo y edad",
    caption = "Fuente: Base de Datos de Lesiones y Causas de Violencia, Secretaría de Salud 2020") +
  facet_wrap(~sexo) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22),
        legend.position = "none",
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 25),
        strip.text = element_text(size = 22, face = "bold"),
        plot.caption = element_text(size = 18))

ggsave("4_Graphs/casos_edad_sexo.png", width = 16, height = 9, dpi = 200)  


### ¿Dónde ocurrieron las agresiones? ----

foo <- bd %>% 
  select(id, sexo, cod_sitio) %>% 
  distinct() %>% 
  filter(sexo !=9) %>% 
  group_by(sexo) %>% 
  count(cod_sitio) %>% 
  ungroup() %>% 
  left_join(y = sitio, by = "cod_sitio") %>% 
  mutate(descripcion = fct_other(f = as_factor(descripcion), 
                                 drop = c("GRANJA", "CLUB, CANTINA, BAR",
                                          "VEHÍCULO AUTOMOTOR PÚBLICO", 
                                          "INSTITUCIÓN RESIDENCIAL", 
                                          "COMERCIO Y ÁREAS DE SERVICIO", 
                                          "ÁREA DE DEPORTE Y ATLETISMO",
                                          "OTRO LUGAR"),
                                 other_level = "OTROS")) %>% 
  group_by(sexo, descripcion) %>% 
  mutate(n = sum(n)) %>% 
  ungroup() %>% 
  select(-cod_sitio) %>% 
  distinct() %>% 
  group_by(sexo) %>% 
  mutate(porcentaje = round(n/sum(n)*100,1)) %>% 
  ungroup() %>% 
  mutate(sexo = if_else(condition = sexo == 1, true = "Hombres", false = "Mujeres"))
t1 <- foo %>% 
  filter(sexo == "Hombres") %>% 
  ggplot(aes(area = porcentaje, fill = porcentaje)) +
  geom_treemap() +
  geom_treemap_text(aes(label = str_glue("{porcentaje}%")), 
                    color = "white", 
                    padding.y = unit(7, "mm"), 
                    min.size = 0, 
                    size = 13) +
  geom_treemap_text(aes(label = descripcion), 
                    color = "white",
                    fontface = "bold",
                    alpha = 1,
                    grow = F,
                    min.size = 0, 
                    size = 16) +
  scale_fill_gradient(low = "#b8bf95", high = "#949d6a") +
  labs(title = str_wrap("Hombres",width = 70)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 20),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 12))

t2 <- foo %>% 
  filter(sexo == "Mujeres") %>% 
  ggplot(aes(area = porcentaje, fill = porcentaje)) +
  geom_treemap() +
  geom_treemap_text(aes(label = str_glue("{porcentaje}%")), 
                    color = "white", 
                    padding.y = unit(7, "mm"), 
                    min.size = 0, 
                    size = 13) +
  geom_treemap_text(aes(label = descripcion), 
                    color = "white",
                    fontface = "bold",
                    alpha = 1,
                    grow = F,
                    min.size = 0, 
                    size = 16)+
  scale_fill_gradient(low = "#8D86C9", high = "#4A1486") +
  labs(title = str_wrap("Mujeres",width = 70)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 14),
        legend.position = "none",
        plot.title = element_text(size = 22, face = "bold"),
        plot.subtitle = element_text(size = 20),
        strip.text = element_text(size = 18, face = "bold"),
        plot.caption = element_text(size = 12))


t1+t2 +
  plot_annotation(title = "La mayoría de las lesiones por violencia sexual suceden en el hogar",
                  subtitle = "Porcentaje de lesiones por violencia sexual, según sitio de ocurrencia de la agresión",
                  caption = "Fuente: Base de Datos de Lesiones y Causas de Violencia, Secretaría de Salud 2020") &
  theme(plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 18))

ggsave("4_Graphs/casos_sitio_sexo_2.png", width = 16, height = 9, dpi = 200) 

### ¿Hubo casos con más de un agresor? ----

# totales:
bd %>% 
  select(id, sexo, cod_agresor) %>% 
  distinct() %>% 
  left_join(y = agresor, by = "cod_agresor") %>% 
  count(descripcion) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))
 

# Por sexo:

bd %>% 
  select(id, sexo, cod_agresor) %>% 
  distinct() %>% 
  left_join(y = agresor, by = "cod_agresor") %>% 
  group_by(sexo) %>% 
  count(descripcion) %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))

# UNICO y Más de uno por sexo 
bd %>% 
  select(id, sexo, cod_agresor) %>% 
  distinct() %>% 
  left_join(y = agresor, by = "cod_agresor") %>% 
  group_by(sexo) %>% 
  count(descripcion) %>% 
  ungroup() %>% 
  filter(descripcion == "MÁS DE UNO") %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))

bd %>% 
  select(id, sexo, cod_agresor) %>% 
  distinct() %>% 
  left_join(y = agresor, by = "cod_agresor") %>% 
  group_by(sexo) %>% 
  count(descripcion) %>% 
  ungroup() %>% 
  filter(descripcion == "ÚNICO") %>% 
  mutate(porcentaje = round(n/sum(n)*100,2))
  
### ¿En cuántos casos se suministró profilaxis?


bd %>% 
  count(cod_tipo_atencion)

foo <- bd %>% 
  filter(sexo ==2) %>% # conservar casos de mujeres
  select(id, cod_tipo_atencion) %>% 
  get_dupes(id)
  
foo %>% 
  arrange(-dupe_count) %>% 
  print(n = 50)

distinct() %>% # filtramos p/quitar duplicados
  count(cod_tipo_atencion)

 
   
 



