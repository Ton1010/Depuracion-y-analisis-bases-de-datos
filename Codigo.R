#INSTALACION LIBRERIAS
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")
install.packages("sf")
install.packages("rgdal")
install.packages("terra")
install.packages("rmarkdown")
install.packages("knitr")
library(knitr)
library(rmarkdown)
library(ggplot2)
library(readr)
library(tidyverse)
library(sf)
library(lubridate)
library(rgdal)
library(terra)
library(dplyr)
library(magrittr)
library(modelr)
library(scales)

#Primera fase, depuraciOn de la base de datos principal
base %<>% rename("CODIGO.DANE"="CODIGO DANE")
base %<>% rename("ARMAS.MEDIOS"="ARMAS MEDIOS")

base %<>% rename("GRUPO.ETARIO"="GRUPO ETARIO")
base$GRUPO.ETARIO[base$GRUPO.ETARIO=="ADOLECENTES"]<-"ADOLESCENTES"

base$GRUPO.ETARIO[base$GRUPO.ETARIO=="NO APLICA"]<-NA
base$GRUPO.ETARIO[base$GRUPO.ETARIO=="NO REPORTA"]<-NA

base$ARMAS.MEDIOS[base$ARMAS.MEDIOS=="-"]<-"NO REPORTADO"
base$ARMAS.MEDIOS[base$ARMAS.MEDIOS=="ARMAS BLANCAS"]<-"ARMA BLANCA / CORTOPUNZANTE"
base$ARMAS.MEDIOS[base$ARMAS.MEDIOS=="CORTANTES"]<-"ARMA BLANCA / CORTOPUNZANTE"
base$ARMAS.MEDIOS[base$ARMAS.MEDIOS=="CORTOPUNZANTES"]<-"ARMA BLANCA / CORTOPUNZANTE"


base$GENERO[base$GENERO=="NO APLICA"]<-NA
base$GENERO[base$GENERO=="-"]<-NA
base$GENERO[base$GENERO=="NO REPORTA"]<-NA

base   %<>% na.omit(base)

#Segunda fase: Creacion de nuevas bases de datos para el tratado de la informacion
genero<-select(base, GENERO, CANTIDAD)
genero %<>% count(GENERO, sort=TRUE)
genero %<>% rename("CANTIDAD"="n")

ARMAS <-select(base,ARMAS.MEDIOS,CANTIDAD)
ARMAS %<>% count(ARMAS.MEDIOS, sort=TRUE)
ARMAS %<>% rename("CANTIDAD"="n")
ARMAS <- arrange(ARMAS, -CANTIDAD)

delito <- select(base, delito, CANTIDAD)
delito %<>% count(delito, sort=TRUE)


date <-  as.Date(base$`FECHA HECHO`,'%d/%m/%Y')
year<- as.numeric(format(date,'%Y'))
year <- as.data.frame(year)
year %<>% count(year, sort=TRUE)
year <- arrange(year, year)


grupo<-select(base, GRUPO.ETARIO, CANTIDAD)


etario <- grupo %>% count(GRUPO.ETARIO, sort=TRUE)

muni <- select(base,MUNICIPIO, CANTIDAD)
muni %<>% count(MUNICIPIO,sort = TRUE)
muni <- arrange(muni, -n)
muni<-muni[1:5,]


tipos_de_delito<-delito[1:5,]

#Tercera fase: Depuracion de las nuevas bases de datos
etario %<>% rename("CANTIDAD"="n")
tipos_de_delito %<>% rename("DELITA"="delito")
tipos_de_delito %<>% rename("CANTIDAD"="n")
year %<>% rename("CANTIDAD"="n")

etario$GRUPO.ETARIO[etario$GRUPO.ETARIO=="NO REPORTA"]<-NA
genero$GENERO[genero$GENERO=="NO REPORTA"]<-NA




tipos_de_delito$DELITA[tipos_de_delito$DELITA==
                         "ARTÍCULO 210 A. ACOSO SEXUAL"]<-"ACOSO SEXUAL"
tipos_de_delito$DELITA[tipos_de_delito$DELITA==
                         "ARTÍCULO 209. ACTOS SEXUALES CON MENOR DE 14 AÑOS"]<-"ACTOS SEXUALES CON MENOR DE 14 AÑOS"
tipos_de_delito$DELITA[tipos_de_delito$DELITA==
                         "ARTÍCULO 208. ACCESO CARNAL ABUSIVO CON MENOR DE 14 AÑOS"]<-"ACCESO CARNAL ABUSIVO CON MENOR DE 14 AÑOS"
tipos_de_delito$DELITA[tipos_de_delito$DELITA==
                         "ARTÍCULO 205. ACCESO CARNAL VIOLENTO"]<-"ACCESO CARNAL VIOLENTO"
tipos_de_delito$DELITA[tipos_de_delito$DELITA==
                         "ARTÍCULO 206. ACTO SEXUAL VIOLENTO"]<-"ACTO SEXUAL VIOLENTO"


# DATOS PARA GRAFICA de pastel sobre genero
plotdata <- genero %>%
  arrange(desc(GENERO)) %>%
  mutate(prop = round(CANTIDAD * 100 / sum(CANTIDAD), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

# DATOS PARA GRAFICA de pastel sobre municipio

plotdata1 <- muni %>%
  arrange(desc(MUNICIPIO)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

#Cuarta fase: Elaboracion de las graficas

#GRAFICO DE BARRA GRUPO ETARIO
options(scipen=9)
ggplot(etario, aes(x = GRUPO.ETARIO, y=CANTIDAD, fill=GRUPO.ETARIO)) +
  geom_bar(stat = "identity", color="black") + 
  scale_fill_manual(values=c("#b4edd2","#a0cfd3","#8d94ba","#9a7aa0","#87677b")) +
  labs(x = "Grupo", 
       y = "Cantidad",
       fill= "Grupo Etario",
       title = "Cantidad de Reportes por Grupo Etario",
       caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022 \n Grafica 2") + 
  theme(plot.title=element_text(size=18, face="bold"),
        legend.position = "none")+ylim(0,150000) 


#GRAFICO DE TORTA PARA GENERO
ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = GENERO,
           color="white")) +
  geom_bar(width = 5, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(label=percent(prop/100)),
            position=position_stack(vjust=0.55),color="black",size=4)+
  
  scale_fill_manual(values=c("#9A7AA0","#A0CFD3")) +
  coord_polar("y", 
              start = 0, 
              direction = -1) + theme_void() +
  labs(y=NULL,
       x=NULL,
       title = "Porcentaje de Delitos \n Sexuales por Genero",caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022 \n Grafica 1") + 
  theme(plot.title=element_text(size=18, face='bold',hjust=0.5),plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1))

# GRAFICA DE TIPOS DE DELITOS
ggplot(tipos_de_delito, aes(x = reorder(DELITA,+CANTIDAD), y=CANTIDAD, fill=DELITA)) +
  geom_bar(stat = "identity", color="black") + 
  scale_fill_manual(values=c("#b4edd2","#a0cfd3","#8d94ba","#9a7aa0","#87677b")) +
  labs(x = "Tipo de Delito cometido", 
       y = "Frecuencia de delitos cometidos",
       fill= "Tipo de Delito",
       title = "Delitos Mas Repetidos",
       caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022 \n Grafica 3") + 
  theme(plot.title=element_text(face='bold'),
        legend.position = "none",
        axis.title.x =element_text(size=11),
        axis.title.y = element_text(size=11))+ coord_flip() 

#GRAFICA ARMAS

ggplot(ARMAS, aes(x = reorder(ARMAS.MEDIOS,-CANTIDAD), y=CANTIDAD, fill=ARMAS.MEDIOS)) +
  geom_bar(stat = "identity", color="black") + 
  scale_fill_manual(values=c("#b4edd2","#aaded3","#a0cfd3","#97b2c7","#8d94ba","#9487ad","#9a7aa0","#91718e","#87677b")) +
  labs(x = "Tipo de Arma Utilizada", 
       y = "Frecuencia de uso",
       fill= "Tipo de Arma",
       title = "Cantidad de Armas Empleadas",
       caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022\n Grafica 6") + 
  theme(plot.title=element_text(face='bold'),
        legend.position = "none",
        axis.title.x =element_text(size=11),
        axis.title.y = element_text(size=11),
        axis.text.x=element_text(angle=45, hjust=0.9))+ ylim(0,150000)

#GRAFICA AÑOS

ggplot(year, aes(x = year, y = CANTIDAD)) +
  geom_line(color='#87677b') +
  labs(title = "Cantidad de Delitos Sexuales Respecto al Tiempo",
       x = "Año",
       y = "Cantidad",
       caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022\n Grafica 7") + 
  theme(plot.title=element_text(size=18, face='bold',hjust=0.5),  plot.margin = margin(t = 20,
                                                                                       r = 40,
                                                                                       b = 40,
                                                                                       l = 15)) + 
  coord_cartesian(xlim = c(2010, 2022), expand = FALSE)+ ylim(0,40000) 


#GRAFICA MUNICIPIO
plotdata1 <- muni %>%
  arrange(desc(MUNICIPIO)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata1, 
       aes(x = "", 
           y = prop, 
           fill = MUNICIPIO,
           color="white")) +
  geom_bar(width = 5, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(label=percent(prop/100)),
            position=position_stack(vjust=0.5),color="black",size=5)+
  coord_polar("y", 
              start = 0, 
              direction = -1) + theme_void() +
  labs(y=NULL,
       x=NULL,
       title = "Porcentaje de Delitos \n Sexuales por Municipio",caption = "FUENTE: DIJIN - Policia Nacional. Reporte Delitos sexuales Policia Nacional - 2022\n Grafica 5") + 
  theme(plot.title=element_text(size=18, face='bold',hjust=0.5),plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1)) + scale_fill_manual(values=c("#b4edd2","#a0cfd3","#8d94ba","#9a7aa0","#87677b"))
