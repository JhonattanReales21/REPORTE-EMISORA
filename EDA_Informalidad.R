library(tidyverse)
library(Hmisc)
library(gridExtra)
library(extrafont)
font_import()
loadfonts(device = "win")

Dane_paramodelos$Rama.actividad <- as.factor(Dane_paramodelos$Rama.actividad)
Dane_paramodelos <- read.csv("Dane_paramodelos.csv", sep=",", fileEncoding = "Latin1")
Y4_description <- describe(Dane_paramodelos)


                            ##### GENERAL OVERVIEW OF OCCUPATION ######

# TOTAL DATAFRAME
Y4_description <- data.frame("Ocupación"=Y4_description$Y_modelo4$values$value, 
                             "Frecuencia"=Y4_description$Y_modelo4$values$frequency, 
                             "Proporción"=c(0.140, 0.659, 0.2))
ggplot(Y4_description, aes(x=Ocupación, y=Frecuencia)) + geom_col(fill="slategray") + geom_text(aes(x=Ocupación, y=Frecuencia, label=Frecuencia), vjust=-0.3) + 
  ggtitle("Ocupación de inmigrantes en Colombia") + 
  theme(title = element_text(hjust=0.5), text = element_text(size=12,family = "Tahoma")) + 
  theme_classic() 

# PER DEPARTAMENTO
Depto <- c(5, 8,11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76 )
Y4_per_departamento <- NULL
Dep=5
for (Dep in Depto){
  set_dept <- Dane_paramodelos %>% filter(Departamento == Dep )%>% group_by(Departamento, Y_modelo4) %>% summarise(Frecuencia=n()) %>% mutate(Proporción=set_dept$Frecuencia/sum(set_dept$Frecuencia))
   Y4_per_departamento <- rbind(Y4_per_departamento, set_dept)
}
Y4_per_departamento$Departamento <- as.factor(Y4_per_departamento$Departamento)
Y4_per_departamento$Departamento <- fct_collapse(Y4_per_departamento$Departamento, "Antioquia"="5", "Atlantico"="8", "Bogota"= "11", "Bolivar"=  "13", 
                                    "Boyaca"= "15", "Caldas"="17", "Caqueta"= "18", "Cauca"= "19",
                                    "Cesar"=  "20", "Cordoba"= "23", "Cundinamarca"="25", "Choco"="27", 
                                    "Huila"= "41", "La Guajira"="44","Magdalena"= "47", "Meta"="50",
                                    "Nariño"="52", "Norte de Santander"="54", "Quindio"="63", 
                                    "Risaralda"="66", "Santander"="68", "Sucre"="70","Tolima"=  "73", "Valle"="76")

ggplot(Y4_per_departamento, aes(x=Y_modelo4, y=Frecuencia)) + geom_col(fill="slategray") + geom_text(aes(x=Y_modelo4, y=Frecuencia, label=Frecuencia), vjust=-0.3) + 
  ggtitle("Ocupación de inmigrantes en Colombia según departamento") + 
  theme(title = element_text(hjust=0.5), text = element_text(size=12,family = "Tahoma")) + 
  theme_classic()  + 
  facet_wrap(~Departamento) + 
  xlab("Ocupación")




                        ##### ANALYSIS WITH RESPECT TO ACTIVITY CATEGORY ######
Y4_Informal <- filter(Dane_paramodelos, Y_modelo4=="Informal")
Y4_Informal <- as.data.frame(table(as.factor(Y4_Informal$Rama.actividad)))
Y4_Informal <- arrange(Y4_Informal, desc(Freq))
Y4_Informal <- Y4_Informal[1:10, ]
ggplot(Y4_Informal, aes(x=reorder(Var1, Freq), y=Freq)) + geom_col(fill="slategray") + geom_text(aes(x=Var1, y=Freq, label=Freq), hjust=-0.1) + 
  ggtitle("Ramas de actividad en empleados Informales") + 
  theme(title = element_text(hjust=0.5), text = element_text(size=12,family = "Tahoma")) + 
  theme_classic() + labs(x="Ocupación" ,y="Frecuencia", caption="Tomando las 10 ramas más relevantes") +
  coord_flip()

                      ##### ANALYSIS WITH RESPECT TO GENDER ######
Y4_gender <- Dane_paramodelos %>% group_by(Genero, Y_modelo4) %>% summarise(Frecuencia=n()) %>% mutate(Proporcion = Y4_gender$Frecuencia/sum(Y4_gender$Frecuencia))
Y4_gender <- Y4_gender 
