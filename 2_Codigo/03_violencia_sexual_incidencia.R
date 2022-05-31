rm(list=ls())
#------------------------------------------------------------------------------#
# Impunidad Cero
# proyecto:         Violencia sexual
# fecha:            mayo 2022
# autoras:          Monserrat López y Helga Jáuregui
# num:              04 código p/ calcular aumento en delitos sexuales 2015-2021
#------------------------------------------------------------------------------#

# 0. Configuración  -----------------------------------------------------
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Para Mac
Sys.setlocale("LC_ALL", "Spanish") # Para Windows
pacman::p_load(readxl, tidyverse, janitor)

# 1. Cargar datos  -----------------------------------------------------

# Cargar base de datos de carpetas de investigación (delitos)
inci_deli <- read_excel("1_Datos/Estatal-Delitos-2015-2022_abr2022.xlsx")%>% 
  clean_names()

# 2. Limpieza -----------------------------------------------------

# Cambiar nombres largos de estados carpetas
inci_deli$entidad <- gsub('Coahuila de Zaragoza','Coahuila',inci_deli$entidad)
inci_deli$entidad <- gsub('Veracruz de Ignacio de la Llave','Veracruz',inci_deli$entidad)
inci_deli$entidad <- gsub('Michoacán de Ocampo','Michoacán',inci_deli$entidad)
inci_deli$modalidad <- gsub('Otros delitos que atentan contra la libertad y la seguridad sexual','Otros',inci_deli$modalidad)

# Crear una nueva columna con la suma del total de cada mes
inci_deli$total <- rowSums(inci_deli[8:19])

# Filtrar por delitos de violencia sexual
violencia_sexual <- inci_deli %>% 
  filter(bien_juridico_afectado=="La libertad y la seguridad sexual") 

# 2. Descriptivos -----------------------------------------------------

# 2.1. Tipos de violencia por año

tipo_violencia <- violencia_sexual %>% 
  filter(ano != 2022) %>% 
  mutate(modalidad = fct_other(f = as_factor(modalidad), 
                               drop = c("Otros", "Incesto"), 
                               other_level = "Otros")) %>% # agrupamos incesto en "otros"
  group_by(ano, modalidad) %>% 
  summarize(total= sum(total, na.rm=T)) %>% 
  ungroup() 

tipo_violencia %>% glimpse()

# 2.2 Total de delitos por año 

total_anual <- tipo_violencia %>% 
  group_by(ano) %>% 
  summarise(total = sum(total, na.rm = T)) %>% 
  ungroup() 

# 2.3 Graficar

paleta <- c("#4A1486", "#8D86C9", "#CAC4CE", "#F7ECE1", "#b8bf95", "#949d6a")

tipo_violencia %>% 
ggplot() + 
  geom_bar(aes(fill = modalidad, y = total, x = ano),
           position="stack", stat="identity") +
  geom_text(data = total_anual, 
            mapping = aes(x = ano, y = total, label = total),
            nudge_y = 1350, size = 7) +
  scale_fill_manual(values = paleta) +
  scale_x_continuous(breaks=seq(2015,2021,1)) +
  ggtitle("Delitos contra la libertad y la seguridad sexual") +
  labs(title = str_wrap("Los delitos contra la libertad y seguridad sexual han ido en aumento",width = 60),
       subtitle = "Delitos registrados en las carpetas de investigación (2015-2021)",
       y="Total de casos de violencia sexual",
       fill="Modalidad",
       caption = "Fuente: Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SENSP)") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 30, family = "Tahoma", face = "bold"),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 18),
        text = element_text(family = "Tahoma"),
        axis.text.x = element_text(colour = "black", size = 22),
        axis.text.y = element_text(colour = "black", size = 22),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25)) 


  
ggsave("4_Graphs/incidencia_viol_sex.png", width = 16, height = 9, dpi = 200)   
