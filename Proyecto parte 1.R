---
  title: "Proyecto - Concurso Guatecompras"
output: html_notebook
---
#instalar librerias

install.packages("tidyverse")
install.packages("arules")


# Cargar librerias

library(tidyverse)
library(arules)
library(caret)
library(readxl)
library(dplyr)
  
# Limpieza y carga de datos
C2016 <- read_excel("2016.xlsx")
C2017 <- read_excel("2017.xlsx")
C2018 <- read_excel("2018.xlsx")
C2019 <- read_excel("2019.xlsx")
C2020 <- read_excel("2020.xlsx")
C2021 <- read_excel("2021.xlsx")
C2022 <- read_excel("2022.xlsx")
C2023 <- read_excel("2023.xlsx")
C2024 <- read_excel("2024.xlsx")


##Eliminar la columna fechaCierreRecepciónOfertas
C2016 <- C2016 %>% select(-fechaCierreRecepciónOfertas)
C2017 <- C2017 %>% select(-fechaCierreRecepciónOfertas)
C2018 <- C2018 %>% select(-fechaCierreRecepciónOfertas)
C2019 <- C2019 %>% select(-fechaCierreRecepciónOfertas)
C2020 <- C2020 %>% select(-fechaCierreRecepciónOfertas)
C2021 <- C2021 %>% select(-fechaCierreRecepciónOfertas)
C2022 <- C2022 %>% select(-fechaCierreRecepciónOfertas)
C2023 <- C2023 %>% select(-fechaCierreRecepciónOfertas)
C2024 <- C2024 %>% select(-fechaCierreRecepciónOfertas)

##Combinar todos los data frames
concursos_g <- bind_rows(C2016, C2017, C2018, C2019, C2020, C2021, C2022, C2023, C2024)

##Pasar el nombre de las columnas a solo minusculas
colnames(concursos_g) <- tolower(colnames(concursos_g))

##seleccionar columnas que se van a utilizar
concursos <- concursos_g %>%
  select(no,tipoentidad,entidadcompradora,modalidad,nombre,monto,categorías,añodeadjudicación)

##Convertir la variable monto a numerica
concursos$monto <- as.numeric(concursos$monto)

##Eliminar los archivos que ya no se necesitan
remove(C2016,C2017,C2018,C2019,C2020,C2021,C2022,C2023,C2024)


## Discretiza todas las columnas no categóricas automáticamente
concursos_discretizados <- discretizeDF(concursos)
concursos_discretizados <- concursos_discretizados %>%
  mutate(across(where(is.character), as.factor))



# Aplicar algoritmo de apriori
reglas <- apriori(concursos_discretizados, parameter = list(support=0.1, confidence=0.2))

inspect(reglas[0:100])
inspect(reglas[101:200])
inspect(reglas[201:300])
inspect(reglas[301:400])
inspect(reglas[401:500])

# Aplicar algoritmo FP-GROWTH
reglas <- fim4r(concursos, method = "fpgrowth", target = "rules", supp = 0.2, conf= 0.5)

##Crear dataframe de las reglas
reglasframe <- as(reglas, "data.frame")



##filtrar la variable categorías por construcción
concursos_c <- subset(concursos,categorías=="Construcción y materiales afines")

##aplicar algoritmo
reglas_c <- fim4r(concursos_c, method = "fpgrowth", target = "rules", supp = 0.2, conf= 0.5)

##crear dataframe de las reglas
reglasframe_c <- as(reglas_c, "data.frame")



##filtrar la variable categorías por transporte
concursos_t <- subset(concursos,categorías %in% c("Transporte, repuestos y combustibles",
                                                  "Transporte, repuestos y combustibles,Otros tipos de bienes o servicios",
                                                  "Transporte, repuestos y combustibles,Otros tipos de bienes o servicios,Seguridad y armament",
                                                  "Transporte, repuestos y combustibles,Seguridad y armament"))
##aplicar algoritmo
reglas_t <- fim4r(concursos_t, method = "fpgrowth", target = "rules", supp = 0.2, conf= 0.5)
##crear dataframe de las reglas
reglasframe_t <- as(reglas_t, "data.frame")




#Análisis de Clúster

##filtrar los datos por entidades del setor salud
concursos_salud <- subset(concursos,
                          entidadcompradora %in% c("INSTITUTO GUATEMALTECO DE SEGURIDAD SOCIAL -IGSS-","MINISTERIO DE SALUD PÚBLICA"))

##filtrar variables a utilizar
concursos_salud <- concursos_salud %>% 
  select(no, entidadcompradora, añodeadjudicación, monto)

##convertir los datos categoricos a factores
concursos_salud$entidadcompradora <- as.factor(concursos_salud$entidadcompradora)
##ahora convertir a numericos
concursos_salud$entidadcompradora_num <- as.numeric(concursos_salud$entidadcompradora)

##seleccionar nuevaente las variables a utilizar
concursos_salud2 <- concursos_salud %>% 
  select(no, entidadcompradora_num, añodeadjudicación, monto)

##aplicar algoritmo de Kmeans
cluster <- kmeans(concursos_salud2, centers = 4)

concursos_salud$cluster <- as.factor(cluster$cluster)

# graficar

ggplot(concursos_salud, aes(x = añodeadjudicación, y = monto, color = entidadcompradora, shape = cluster)) +
  geom_point() +
  geom_point(data = as.data.frame(cluster$centers), aes(x = añodeadjudicación, y = monto), color = "black", size = 4, shape = 17) +
  labs(title = "Clustering de Concursos de Salud: Año de Adjudicación vs Monto",
       x = "Año de Adjudicación", y = "Monto") +
  scale_y_continuous(labels = scales::comma_format()) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8),  
    axis.title = element_text(size = 6),   
    axis.text = element_text(size = 6),
    legend.position = "bottom",          
    legend.key.size = unit(0.5, "cm"),   
    legend.text = element_text(size = 4), 
    legend.title = element_text(size = 4), 
    legend.box = "horizontal",           
    legend.box.spacing = unit(0.2, "cm") 
  ) +
    guides(color = guide_legend(ncol = 1))

