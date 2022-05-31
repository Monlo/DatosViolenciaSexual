rm(list=ls())
#------------------------------------------------------------------------------#
# Impunidad Cero
# proyecto:         Violencia sexual
# fecha:            mayo 2022
# autoras:          Monserrat López y Helga Jáuregui
# num:              04 código p/ calcular cifra negra en delitos sexuales
#------------------------------------------------------------------------------#

# 0. Configuración  -----------------------------------------------------
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # para mac
Sys.setlocale("LC_ALL", "Spanish") # para windows

pacman::p_load(readr, tidyverse, janitor, lubridate, purrr, tidyr, reshape2, df2, readxl,xlsx, srvyr, survey)
# Ajustes ponderación
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

# 1. Cargar datos  -----------------------------------------------------

# Cargar Tabla TMod_Vic
# Importarr la tabla, limpiar los nombres y convertir a numéricos
victim <- read_csv("1_Datos/conjunto_de_datos_ENVIPE_2021_csv/conjunto_de_datos_TMod_Vic_ENVIPE_2021/conjunto_de_datos/conjunto_de_datos_TMod_Vic_ENVIPE_2021.csv") %>% 
  clean_names() %>%
  mutate(upm_dis = as.numeric(upm_dis),
         est_dis = as.numeric(est_dis),
         fac_del = as.numeric(fac_del),
         sexo = as.numeric(sexo),
         edad = as.numeric(edad),
         bpcod = as.numeric(bpcod),
         bp1_20 = as.numeric(bp1_20),
         bp1_21 = as.numeric(bp1_21),
         bp1_24 = as.numeric(bp1_24),
         delitos_ocurridos= 1)
victim %>% glimpse()

# Cargar diccionarios
BPCOD <- read_csv("1_Datos/conjunto_de_datos_ENVIPE_2021_csv/conjunto_de_datos_TMod_Vic_ENVIPE_2021/catalogos/BPCOD.csv", locale = locale(encoding = "latin1")) %>% 
  clean_names() %>% 
  mutate(bpcod = as.numeric(bpcod))
BPCOD %>% glimpse()

# 2. Estimaciones  -----------------------------------------------------
# Construcción de la variable de Delitos Ocurridos
victim$DO <- ifelse(!victim$bpcod%in%"3",1,0)

# Construcción del Filtro de Cifra Negra
# Delitos No Denunciados
victim$DND <- ifelse(!victim$bpcod%in%"3" & (victim$bp1_20 %in% "2" & victim$bp1_21 %in% c("2",NA)),1,0) 

# Delitos Denunciados sin CI o AP
victim$DSAP <- ifelse(!victim$bpcod%in%"3" & (victim$bp1_20 %in% "1" | victim$bp1_21 %in% "1") & !victim$bp1_24 %in% "1",1,0)

# Delitos Denunciados en los cuales no fue especificado si se denunció o si se inició CI o AP
victim$DNE <- ifelse(!victim$bpcod%in%"3" & (victim$bp1_21 %in% "9" | victim$bp1_24 %in% "9"),1,0) 

# Construcción de la variable de Cifra Negra
victim$CN <- ifelse((victim$DND%in%"1" | victim$DSAP%in%"1" | victim$DNE%in%"1"),1,0)

victim%>% 
  select(bpcod,bp1_20, bp1_21,bp1_24,DND,DSAP,DNE, CN) %>% 
  print()

# Construcción de la variable que especifica el diseño de la encuesta

# Diseño muestral-
design <- 
  victim %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_del)

# Delitos 
delitos <- design %>% 
  group_by(bpcod) %>% 
  summarize(total_delitos = survey_total(DO)) %>% 
  filter(bpcod != 3) %>% 
  ungroup() %>% 
  print()

# No denunciados ni iniciaron una CI
cn <- design %>% 
  group_by(bpcod) %>% 
  summarize(no_denuncia = survey_total(CN)) %>% 
  filter(bpcod != 3) %>% 
  ungroup() %>% 
  print() 

cn_delitos <- cn %>% 
  left_join(delitos, by="bpcod") %>% 
  left_join(BPCOD, by="bpcod") %>% 
  mutate(delito = recode(descrip,
                         "Robo total de vehículo (automóvil, camioneta, camión)" = "Robo total de vehículo", 
                         "Robo de accesorios, refacciones o herramientas de vehículos (automóvil, camioneta, camión)" = "Robo parcial de vehículo", 
                         "Alguien entró a su casa o departamento sin permiso mediante el uso de la fuerza o por engaños y robó o intentó robar algo" = "Robo en casa habitación", 
                         "Robo o asalto en la calle o en el transporte público (incluye robo en banco o cajero automático)" = "Robo o asalto en calle o transporte público",
                         "Alguien usó su chequera, número de tarjeta o cuenta bancaria sin su permiso para realizar cargos o para extraer dinero de sus cuentas (fraude bancario) o le dio dinero falso" = "Fraude",
                         "Entregó dinero por un producto o un servicio que no recibió conforme a lo acordado (fraude al consumidor)" = "Fraude",
                         "Amenazas, presiones o engaños para exigirle dinero o bienes; o para que hiciera algo o dejara de hacerlo (extorsión)" = "Extorsión", 
                         "Amenazas verbales de alguien plenamente identificado o por escrito hacia su persona diciendo que le va a causar un daño a usted, a su familia, a sus bienes o su trabajo" = "Amenazas", 
                         "Alguien sólo por actitud abusiva o por una discusión lo(a) golpeó generándole una lesión física (moretones, fracturas, cortadas, etc.)" = "Lesiones", 
                         "Lo secuestraron para exigir dinero o bienes" = "Otros delitos","Alguien en contra de su voluntad lo(a) agredió mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violación" = "Hostigamiento o intimidación sexual",
                         "Fue obligado(a) mediante violencia física o amenaza por alguien conocido o desconocido a tener una actividad sexual no deseada (violación sexual)" = "Violación",
                         "Otros delitos distintos a los anteriores" = "Otros delitos")) %>% 
    group_by(delito) %>% 
    summarise(across(.cols = no_denuncia:total_delitos_se, .fns = sum)) %>% 
    ungroup() %>% 
    mutate(cn_per = (no_denuncia/total_delitos)*100)  

# Ordenar datos
graph1 <-  cn_delitos %>%  
  mutate(cn_per = round(cn_per, digits =1)) %>% 
  group_by(delito, no_denuncia, total_delitos) %>% 
  summarize(cn_per = sum(cn_per)) %>% 
  ungroup() %>% 
  print()

# Exportar  
write.csv(graph1,"3_Out/cifranegra_delito.csv",fileEncoding = "UTF-8", row.names=FALSE)

## Graficar ---- 

graph1 %>% 
  mutate(relleno = if_else(condition = delito %in% c("Hostigamiento o intimidación sexual", "Violación"),
                           true = "cambia", 
                           false = "no cambia")) %>% 
  ggplot(aes(x = cn_per, y = reorder(delito, cn_per), fill = relleno)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=str_glue("{cn_per}%")),
            color="black", size = 7, nudge_x = 5) +
  scale_fill_manual(values = c("#8D86C9", "#CAC4CE")) +
  labs(title = "La cifra negra varía de acuerdo al tipo de delito",
       subtitle = str_wrap("Porcentaje de delitos no denunciados o denunciados que iniciaron una carpeta de investigación",60),
       caption =  "Fuente: ENVIPE 2021") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22),
        plot.title = element_text(size = 30, face = "bold"),
        plot.subtitle = element_text( size = 25),
        plot.caption = element_text(size = 18))

ggsave("4_Graphs/cifra_negra_2.png", width = 16, height = 9, dpi = 200) 
