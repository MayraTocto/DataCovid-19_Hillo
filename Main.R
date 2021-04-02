############################################
# PROGRAMA PARA CONSTRUIR LA BASE DE DATOS #
# COVID-19 DE HERMOSILLO.                  #
############################################

# LIBRERIAS
library(dplyr)
library(tidyr)

# LECTURA DE LA BASE DE DATOS DESCARGADA DE LA PAGINA 
# https://coronavirus.gob.mx/datos/

BDMexico <- read.csv("200827COVID19MEXICO.csv")
names(BDMexico)

# CONSTRUCCIÓN DE LA BASE DE DATOS DE HERMOSILLO
BDMexico %>% 
  filter(ENTIDAD_UM==26 & ENTIDAD_RES==26 & MUNICIPIO_RES==30) -> BDHillo

BDHillo %>%
  group_by(RESULTADO) %>%
  summarise(n=n())

write.csv(BDHillo,file="200827COVID19HILLO.csv",row.names = FALSE) 

# CONSTRUCCION DE LA BASE DE DATOS PARA ARTICULO
BDHillo <- read.csv("200827COVID19HILLO.csv")

BDHillo %>%
  filter(RESULTADO==1) %>%
  select(TIPO_PACIENTE,FECHA_INGRESO,FECHA_SINTOMAS,FECHA_DEF,
         RESULTADO) -> BDHillo_Positivos

# CREACION DE VARIABLES IS, Q, Q Y D POR FECHA
BDHillo_Positivos %>%  
  group_by(FECHA_SINTOMAS) %>% 
  summarise(n = n()) %>%
  select(FECHA_SINTOMAS,n) %>% 
  rename(FECHA=FECHA_SINTOMAS, INFEC_SINTO=n) -> Datos_Is

BDHillo_Positivos %>%  
  group_by(FECHA_INGRESO,TIPO_PACIENTE) %>% 
  summarise(n = n()) -> Datos_TIPO_PACIENTE

Datos_TIPO_PACIENTE %>%
  filter(TIPO_PACIENTE==1) %>% 
  select(FECHA_INGRESO,n) %>% 
  rename(FECHA=FECHA_INGRESO, AMBULATORIOS=n) -> Datos_Q

Datos_TIPO_PACIENTE %>%
  filter(TIPO_PACIENTE==2) %>% 
  select(FECHA_INGRESO,n) %>% 
  rename(FECHA=FECHA_INGRESO, HOSPITALIZADOS=n) -> Datos_H

BDHillo_Positivos %>%  
  group_by(FECHA_DEF) %>% 
  summarise(n = n()) %>%
  select(FECHA_DEF,n) %>% 
  rename(FECHA=FECHA_DEF, MUERTES=n) -> Datos_D

# UNION DE VARIABLES EN UNA SOLA TABLA
Datos_Is %>%
  full_join(Datos_Q, by = "FECHA")%>%
  full_join(Datos_H, by = "FECHA")%>%
  full_join(Datos_D, by = "FECHA")%>%
  arrange(FECHA) -> Datos

# COMPLETAR E ELIMINAR FECHAS NO VÁLIDAS
factor(Datos$FECHA)
Datos$FECHA[1]
Datos$FECHA[length(Datos$FECHA)]
n<-dim(Datos)[1]

InicioDias<-as.Date(Datos$FECHA[1])
FinDias<-as.Date(Datos$FECHA[n-1])
Dias<-as.character(seq(InicioDias,FinDias,by='days'))
IndiceDiasFaltantes<-which(Dias %in% Datos$FECHA == FALSE)
Dias[IndiceDiasFaltantes] # 12 indices
# [1] "2020-03-13" "2020-03-14" "2020-03-18" "2020-03-20"
# [5] "2020-03-21" "2020-03-22" "2020-03-29" "2020-03-30"
# [9] "2020-04-02" "2020-04-03" "2020-04-13" "2020-04-14"

Datos %>% add_row(FECHA=Dias[IndiceDiasFaltantes]) %>% 
          mutate(INFEC_SINTO = replace_na(INFEC_SINTO, 0),
                 AMBULATORIOS = replace_na(AMBULATORIOS, 0),
                 HOSPITALIZADOS = replace_na(HOSPITALIZADOS, 0),
                 MUERTES = replace_na(MUERTES, 0)) %>%
          filter(FECHA!="9999-99-99") %>%
          arrange(FECHA) -> DatosFinal_temp

DatosFinal_temp %>% mutate(DIA=0:(dim(DatosFinal_temp)[1]-1)) %>%
                    select(FECHA,DIA,INFEC_SINTO,HOSPITALIZADOS,
                           AMBULATORIOS,MUERTES) -> DatosFinal

# GUARDAR EN UN ARCHIVO LA BASE CREADA
write.csv(DatosFinal,file="DatosHillo200827.csv",row.names = FALSE)

par(mfrow=c(2,2))
plot(DatosFinal$DIA,DatosFinal$INFEC_SINTO,type="l",col="blue",main="Is",xlab="Dia",
     ylab="")
lines(DatosFinal$DIA,DatosFinal$INFEC_SINTO,type="p",pch=15,cex=0.8,col="blue")
plot(DatosFinal$DIA,DatosFinal$AMBULATORIOS,type="l",col="green",main="Q",xlab="Dia",
     ylab="")
lines(DatosFinal$DIA,DatosFinal$AMBULATORIOS,type="p",pch=15,cex=0.8,col="green")
plot(DatosFinal$DIA,DatosFinal$HOSPITALIZADOS,type="l",col="red",main="H",xlab="Dia",
     ylab="")
lines(DatosFinal$DIA,DatosFinal$HOSPITALIZADOS,type="p",pch=15,cex=0.8,col="red")
plot(DatosFinal$DIA,DatosFinal$MUERTES,type="l",main="D",xlab="Dia",
     ylab="")
lines(DatosFinal$DIA,DatosFinal$MUERTES,type="p",pch=15,cex=0.8)

