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
Dane_paramodelos$Genero <- as.factor(Dane_paramodelos$Genero)
table(Dane_paramodelos$Genero)
Y4_gender <- Dane_paramodelos %>%  group_by(Genero, Y_modelo4) %>% summarise(Frecuencia=n())
Y4_gender$Proporcion <- c(Y4_gender[1,3]/9479, Y4_gender[2,3]/9479, Y4_gender[3,3]/9479, Y4_gender[4,3]/11653, Y4_gender[5,3]/11653, Y4_gender[6,3]/11653)
Y4_female <- Y4_gender[1:3, ]
Y4_male <- Y4_gender[4:6, ]

# Compute the cumulative percentages (top of each rectangle)
Y4_female$ymax = cumsum(Y4_female$Proporcion)
Y4_male$ymax = cumsum(Y4_male$Proporcion)

# Compute the bottom of each rectangle
Y4_female$ymin = c(0, head(Y4_female$ymax, n=-1))
Y4_male$ymin = c(0, head(Y4_male$ymax, n=-1))

# Compute label position
Y4_female$labelPosition <- (Y4_female$ymax + Y4_female$ymin) / 2
Y4_male$labelPosition <- (Y4_male$ymax + Y4_male$ymin) / 2

# Compute a good label
Y4_female$label <- paste0("\n Frecuencia: ", Y4_female$Frecuencia)
Y4_male$label <- paste0("\n Frecuencia: ", Y4_male$Frecuencia)

female_plot <- ggplot(Y4_female, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Y_modelo4)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=3.3) +
  scale_fill_brewer(name= "Tipo de Ocupación", palette = "Blues")  + 
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(0.2, 4)) +  # Try to remove that to see how to make a pie chart
  theme_void() +  
  ggtitle("Ocupación de mujeres inmigrantes") + 
  theme(axis.text  = element_blank(),
        plot.title = element_text(color = "darkblue", size = 12, face = "bold", hjust = 0.7, vjust=0.5))


male_plot <- ggplot(Y4_male, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Y_modelo4)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=3.3) +
  scale_fill_brewer(name= "Tipo de Ocupación", palette = "Blues")  + 
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(0.2, 4)) +  # Try to remove that to see how to make a pie chart
  theme_void() +  
  ggtitle("Ocupación de hombres inmigrantes") + 
  theme(axis.text  = element_blank(),
        plot.title = element_text(color = "darkblue", size = 12, face = "bold", hjust = 0.7, vjust=0.5))
grid.arrange(female_plot, male_plot,nrow=1)


                                ##### ANALYSIS WITH RESPECT TO TIME IN COLOMBIA ######
