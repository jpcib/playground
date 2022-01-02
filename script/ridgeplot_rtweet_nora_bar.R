
#1. Cargo paquetes con pacman, que también los instala:
# install.packages("pacman")
pacman::p_load(tidyverse,rtweet,lubridate,ggridges)

# #1b. Sino con la clásica
# install.packages("rtweet")
# install.packages("ggridges")
# library(tidyverse)
# library(rtweet)
# library(lubridate)
# library(ggridges)

#2. Bajar los últimos 3200 tweets (este es el máximo que podemos ir para atrás en una TL)
tl_nora_1 <- get_timeline("norabar", n = 3200)

#3. selecciono sólo las columnas que me sirven, fecha del tweet y cuerpo,
# filtro tweets que contienen la frase "personas en terapia" y el número antes,
# fue UNA manera de filtrar ya que todos los reportes diarios comparten esta estructura.
#
#Luego extraigo algunos datos que me interesan (y un par de más por si los necesito usar después)
tls <- tl_nora_1 %>% 
  select(text,created_at) %>% 
  filter(str_detect(text, "\\d*(?= personas en terapia)")) %>% 
  mutate(text = str_to_lower(text)) %>% 
  mutate(casos = parse_number(str_extract(text, "\\d*.\\d*(?= notificados)"), 
                              locale = locale(decimal_mark = ",")),
         uti_covid = parse_number(str_extract(text, "\\d*(?= personas en terapia)"), 
                                  locale = locale(decimal_mark = ",")),
         tests_dia = parse_number(str_extract(text, "(?<=tests del d[í|i]a: )\\d*\\.\\d*"),
                                  locale = locale(decimal_mark = ",")),
         uti_nacion = parse_number(str_extract(text, "(?<=naci[ó|o]n )\\d*.\\d."),
                                   locale = locale(decimal_mark = ",")),
         uti_amba = parse_number(str_extract(text, "(?<=amba )\\d*.\\d."),
                                 locale = locale(decimal_mark = ","))
  ) %>%
  mutate(uti_amba = ifelse(uti_amba == 367, 36.7, uti_amba)) %>% #arreglo un pequeño error de parseo
  print()


#Saco % de positivos sobre tests del día, y ocupación de camas específicamente por covid.
data_covid_nora <- tls %>% 
  mutate(positividad = round((casos/tests_dia)*100,1),
         uti_covid = round((uti_covid/11668)*100,1)) %>%  
  #dato de camas: https://www.argentina.gob.ar/noticias/el-pais-aumento-en-un-37-la-cantidad-de-camas-de-terapia-intensiva-durante-el-aislamiento
  mutate(fecha = as_date(created_at)) %>% 
  select(-c(text,created_at)) %>%
  print()


# Plot
#pivoteo para hacer un ridgeplot con uti y positividad.

data_covid_nora %>% 
  pivot_longer(cols = c(uti_covid, uti_nacion, uti_amba, positividad),
               names_to = "variable", values_to = "valor") %>% 
  select(-c(casos, tests_dia)) %>% 
  group_by(variable) %>% 
  print() %>% 
  ggplot(aes(x = fecha, y = variable, height = valor,
             fill = variable, alpha = 0.7,
             color = variable)) +
  geom_density_ridges(stat = "identity",
                      scale = 0.9,
                      linetype = 1,
                      lwd = 0.9)+
  theme_ridges()+
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_discrete(position = "right")+
  theme(text = element_text(family =  "Encode Sans"),
        legend.position = "none",
        axis.title = element_blank(),
        axis.line = element_blank(),
  )
