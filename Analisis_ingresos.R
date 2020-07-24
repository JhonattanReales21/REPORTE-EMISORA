library(tidyverse)
library(funModeling)
library(DT)
library(RColorBrewer)
library(ggthemes)
library(viridis)




   #------------------------------    ANALISIS EXPLORATORIO DE INGRESOS  ---------------------------------------
Dane <- read.csv("Dane_Paramodelos_regresion.csv", header = T, sep = ",", dec = ".")

Dane$Departamento <- as.factor(Dane$Departamento)
Dane$Departamento <- fct_collapse(Dane$Departamento, 
                                  "Antioquia"= "5", "Atlántico"="8", "Bogotá"="11", "Bolívar"="13", "Boyaca"="15",
                                  "Caldas"="17", "Caquetá"="18", "Cauca"="19", "Cesar"="20", "Córdoba"="23", 
                                  "Cundinamarca"="25", "Chocó"="27","Huila"="41", "La Guajira"="44", "Magdalena"="47", 
                                  "Meta"="50", "Nariño"="52", "Norte de Santander"="54","Quindío"="63", "Risaralda"="66",
                                  "Santander"="68", "Sucre"="70", "Tolima"="73", "Valle"="76")


#----------  Comentario inicial----------

# La base de datos cuenta con 17,948 observaciones, se han excluido los encuestados menores de 11 años
# y las personas que no declararon ningún tipo de ingreso, es decir, que no quisieron responder estas preguntas.

# A lo largo del analisis se realizarán diferentes filtros para obtener mejores graficas y presentar
# La información de manera mas concreta, los principales filtros son:

17948-Dane %>% filter(Ingresos_total<=1500000) %>% nrow() # 718 ganan mas de $1.500.000
17948-Dane %>% filter(Ingresos_total<=1800000) %>% nrow() # 428 ganan mas de $1.800.000
17948-Dane %>% filter(Ingresos_total<=2500000) %>% nrow() # 217 Ganan mas de $2.500.000

# Hemos obtado por trabajar con las observaciones que ganan menos de $2.500.000
# sin embargo, un analisis de las caracteristicas de esta población, se presenta a continuación:


                                                       #--------.---------
                            #----------  Exploración de población con mayores ingresos ----------
                            
pob_mayor_ingreso <- Dane %>% filter(Ingresos_total>2500000) %>% droplevels()


# Analizamos el ingreso con respecto a la ocupación
Ingresos_prom_mayor <- pob_mayor_ingreso %>% group_by(ocupacion) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),0),
                                                                               `Ingreso máximo`=max(Ingresos_total),
                                                                               "# De observaciones"=n())

DT::datatable(Ingresos_prom_mayor,  class = 'cell-border stripe', filter = "none", width = 500, height = 500,
              options = list(searching=F, lengthChange=F) )

Dane %>% filter(Ingresos_total>2500000) %>% 
  ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 60,fill="skyblue", color="black", alpha=0.8)+
  ggtitle("Distribución de ingresos según la ocupación", subtitle = "(observaciones con mayores ingresos)")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(0.8),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(2500000,15000000), breaks = c(3.0e+06,5.0e+06,7.0e+06,9.0e+06,11.0e+06,13.0e+06,15.0e+06))+
  geom_vline(data=Ingresos_prom_mayor, aes(xintercept=`Ingreso promedio`, color=ocupacion),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("black", "black"))+
  facet_grid(ocupacion~.)


# Analizamos el ingreso con respecto al nivel educativo, grafico y tabla

Tabla_educ_mayor <- Dane %>% filter(Ingresos_total>2500000)%>%  group_by(Nivel.educativo.alcanzado) %>% 
  summarise("# De migrantes"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% Con respecto al total de la muestra" = paste(round((`# De migrantes`/sum(`# De migrantes`)) *100, 2),"%")) %>%
  select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`))

datatable(Tabla_educ_mayor,  class = 'cell-border stripe')

Tabla_educ_mayor$Nivel.educativo.alcanzado <- factor(Tabla_educ_mayor$Nivel.educativo.alcanzado, levels = Tabla_educ_mayor$Nivel.educativo.alcanzado)

Tabla_educ_mayor %>% ggplot(aes(x=Nivel.educativo.alcanzado, y= `Promedio de ingresos`, color=Nivel.educativo.alcanzado))+
  geom_point(size=10) + geom_segment(aes(x = Nivel.educativo.alcanzado,
                                        xend= Nivel.educativo.alcanzado,
                                        y=3000000,
                                        yend=`Promedio de ingresos`))+
  theme_bw()+ labs(title = "Ingreso promedio según nivel educativo alcanzado", subtitle ="(observaciones con mayores ingresos)")+ 
  xlab("")+ylab("")+scale_x_discrete(breaks="")+
  scale_y_continuous(breaks = c(3.0e+06,4.0e+06,5.0e+06,6.0e+06,7.0e+06, 8.0e+06), limits = c(3.0e+06, 8.0e+06))+
  theme(text =element_text(size = 14), legend.position = "bottom", legend.title = element_text(size = 0))+
  theme(plot.title = element_text(size = rel(1), vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(0.8), vjust = 1.5, color="black") )+ 
  theme(axis.title.y = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.y = element_text(face = "bold")) + scale_color_viridis(option = "D", discrete = T )+
  geom_text(aes(x=Nivel.educativo.alcanzado, label= `Promedio de ingresos`),
            size=4.5, vjust=-1.5, hjust=0.5, col="black" )


# Analizamos las ramas de actividad 

Tabla_rama_mayor <- pob_mayor_ingreso %>%  group_by(Rama.actividad) %>% 
  summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% de la muestra (>$2.5M) " = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>%
  select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`))

datatable(Tabla_rama_mayor, class = 'cell-border stripe')

# Analizamos la ubicación 

Tabla_dep_mayor <- pob_mayor_ingreso %>%  group_by(Departamento) %>% 
  summarise("# De migrantes en el departamento"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% de la muestra (>$2.5M) " = paste(round((`# De migrantes en el departamento`/sum(`# De migrantes en el departamento`)) *100, 2),"%")) %>%
  select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`))

datatable(Tabla_dep_mayor, class = 'cell-border stripe')

# Analizamos la ubicación y la ocupación

Tabla_dep2_mayor <- pob_mayor_ingreso %>%  group_by(Departamento, ocupacion) %>% 
  summarise("# De observaciones"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% de la muestra (>$2.5M) " = paste(round((`# De observaciones`/sum(`# De observaciones`)) *100, 2),"%")) %>%
  select(1,2,6,3,9,4,5,7,8)%>% arrange(Departamento, desc(`Promedio de ingresos`))

datatable(Tabla_dep2_mayor, class = 'cell-border stripe')
                                                  #--------.---------
                        #----------  Analisis por segmentos de ocupacion y genero ----------

# Observamos la frecuencia de ocupación en la base de datos
tabla1 <- Dane %>% group_by(ocupacion) %>% summarise(Frecuencia=n()) %>% ungroup() %>% 
  mutate("%"=paste(round(Frecuencia/sum(Frecuencia), 2), "%")) %>% arrange(desc(Frecuencia))

datatable(tabla1, width = 350, class = 'cell-border stripe', options = list(searching=F, lengthChange=F))


                      ##### 1. ASALARIADOS #####

# Filtramos las observaciones que ganen menos de 2.5 millones
filter(Dane, Ingresos_total>=2500000 & ocupacion=="Asalariado") %>% nrow() # Solo 104 asalariados ganan mas de $2.500.000
prueba <- Dane %>% filter(ocupacion=="Asalariado", Ingresos_total<=2500000)
summary(prueba$Ingresos_total)

# Observamos la distribución de ingresos de los asalariados 
prueba %>% ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 70,fill="skyblue", color="black")+
  ggtitle("Ingresos para Asalariados")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.4),hjust=0.5, vjust = 1.5, face = "bold", color="red2"))+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1)))+ theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+
  geom_vline(aes(xintercept=mean(Ingresos_total)),
             color="black", linetype="dashed", size=1.2)

#Ingreso prom por genero
Ingresos_1_gen <- prueba %>% group_by(Genero) %>% summarise(`Ingreso promedio para asalariados`=round(mean(Ingresos_total),2),
                                                           "# De observaciones"=n()) %>% arrange(desc(`Ingreso promedio para asalariados`))

DT::datatable(Ingresos_1_gen,  class = 'cell-border stripe',  width = 450, height = 300,
              options = list(searching=F, lengthChange=F) )


# Ingresos de asalariados dependiendo del genero
  prueba %>% ggplot(aes(x=Ingresos_total, fill=Genero)) +
  geom_histogram(bins = 30,color="black", position = "identity", alpha=0.5)+
  ggtitle("Ingresos de asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(face="italic",size = rel(1.1),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+scale_y_continuous(breaks= seq(0,800, by=100))+
  geom_vline(data=Ingresos_1_gen, aes(xintercept=`Ingreso promedio para asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
 scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue")) 

 
  prueba %>% ggplot(aes(x=Ingresos_total)) +
    geom_histogram(bins = 30,color="black", fill="skyblue", position = "identity", alpha=0.5)+
    ggtitle("Ingresos de asalariados", subtitle = "Con respecto al género")+
    xlab("Ingresos totales")+ylab("") + theme_bw() + 
    theme(text =element_text(size = 14), legend.position = "none")+
    theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
    theme(plot.subtitle = element_text(face="italic", size = rel(1.1),hjust=0.5, vjust = 1.5, color="black") )+ 
    theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
    theme(axis.text.x = element_text(face = "bold"))+
    theme(axis.text.y = element_text(face = "bold"))+ 
    scale_x_continuous(limits = c(0,2500000))+
    geom_vline(data=Ingresos_1_gen, aes(xintercept=`Ingreso promedio para asalariados`, color=Genero),
               linetype="dashed", size=1.2)+
    scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
    facet_grid(Genero~.)
  
  
                      ##### 2. NO ASALARIADOS ####

# Filtramos las observaciones que ganen menos de 2.5 millones
filter(Dane, Ingresos_total>=2500000 & ocupacion=="No asalariado") %>% nrow() # Solo 117 asalariados ganan mas de $2.500.000
prueba2 <- Dane %>% filter(ocupacion=="No asalariado" & Ingresos_total<=2500000)
summary(prueba2$Ingresos_total)

# Observamos la distribución de ingresos de los No asalariados 
prueba2 %>% ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 70,fill="skyblue", color="black")+
  ggtitle("Ingresos para No asalariados")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.4),hjust=0.5, vjust = 1.5, face = "bold", color="red2"))+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1)))+ theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+
  geom_vline(aes(xintercept=mean(Ingresos_total)),
             color="black", linetype="dashed", size=1.2)

#Ingreso prom por genero para No asalariados
Ingresos_2_gen <- prueba2 %>% group_by(Genero) %>% summarise(`Ingreso promedio para No asalariados`=round(mean(Ingresos_total),2),
                                                             "# De observaciones"=n()) %>% arrange(desc(`Ingreso promedio para No asalariados`))

DT::datatable(Ingresos_2_gen,  class = 'cell-border stripe', filter = "none", width = 430, height = 300,
              options = list(searching=F, lengthChange=F) )


# Ingresos de No asalariados dependiendo del genero
prueba2 %>% ggplot(aes(x=Ingresos_total, fill=Genero)) +
  geom_histogram(bins = 30,color="black", position = "identity", alpha=0.5)+
  ggtitle("Ingresos de No asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(face="italic",size = rel(1.1),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingresos_2_gen, aes(xintercept=`Ingreso promedio para No asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue")) 


prueba2 %>% ggplot(aes(x=Ingresos_total)) +
  geom_histogram(bins = 30,color="black", fill="skyblue", position = "identity", alpha=0.8)+
  ggtitle("Ingresos de No asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(face="italic", size = rel(1.1),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingresos_2_gen, aes(xintercept=`Ingreso promedio para No asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
  facet_grid(Genero~.)




                                ##### 3. AMBOS SECTORES ####

# Grafico y tabla de la distribución de ingresos dependiendo de la ocupación

Ingresos_prom <- Dane %>%filter(Ingresos_total<=2500000) %>%  group_by(ocupacion) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2))

DT::datatable(Ingresos_prom,  class = 'cell-border stripe', filter = "none", width = 300, height = 500,
              options = list(searching=F, lengthChange=F) )

Dane %>% filter(Ingresos_total<=2500000) %>% 
  ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 60,fill="skyblue", color="black", alpha=0.8)+
  ggtitle("Distribución de ingresos según la ocupación")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2000000))+
  geom_vline(data=Ingresos_prom, aes(xintercept=`Ingreso promedio`, color=ocupacion),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("black", "black"))+
  facet_grid(ocupacion~.)

# Grafico y tabla de la distribución de ingresos para ambos sectores con respecto al género y la ocupación

Ingresos_ambos <- Dane %>%filter(Ingresos_total<=2500000) %>% group_by(Genero, ocupacion) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2),
                                                         "# De observaciones"=n()) %>% arrange(desc(`Ingreso promedio`))

DT::datatable(Ingresos_ambos,  class = 'cell-border stripe', filter = "none", width = 500, height = 500,
              options = list(searching=F, lengthChange=F) )

Dane %>% filter(Ingresos_total<=2000000) %>% 
  ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 60,fill="skyblue", color="black", alpha=0.8)+
  ggtitle("Distribución de ingresos según la ocupación \ny el género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2000000))+
  geom_vline(data=Ingresos_ambos, aes(xintercept=`Ingreso promedio`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
  facet_grid(ocupacion~Genero)

                                #--------.---------
                  #----------  Analisis por departamentos ----------

rm(list = ls())

Dane <- read.csv("Dane_Paramodelos_regresion.csv", header = T, sep = ",", dec = ".")
Dane$Departamento <- as.factor(Dane$Departamento)
Dane$Departamento <- fct_collapse(Dane$Departamento, 
                                  "Antioquia"= "5", "Atlántico"="8", "Bogotá"="11", "Bolívar"="13", "Boyaca"="15",
                                  "Caldas"="17", "Caquetá"="18", "Cauca"="19", "Cesar"="20", "Córdoba"="23", 
                                  "Cundinamarca"="25", "Chocó"="27","Huila"="41", "La Guajira"="44", "Magdalena"="47", 
                                  "Meta"="50", "Nariño"="52", "Norte de Santander"="54","Quindío"="63", "Risaralda"="66",
                                  "Santander"="68", "Sucre"="70", "Tolima"="73", "Valle"="76")


#Construimos una tabla con datos referentes al ingreso por persona en cada departamento SIN FILTROS
Tabla_dep <- Dane %>% group_by(Departamento) %>% 
  summarise("# De migrantes"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),1),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% Con respecto al total de la muestra" = paste(round((`# De migrantes`/sum(`# De migrantes`)) *100, 2),"%")) %>%
  select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`)) 

datatable(Tabla_dep, class = 'cell-border stripe')

# Tabla con datos referentes al ingreso por persona en cada departamento 
# EXCLUYENDO A LOS QUE GANAN DE 2.5 MILLONES

Tabla_dep2 <- Dane %>% filter(Ingresos_total<=2500000)%>%  group_by(Departamento) %>% 
  summarise("# De migrantes"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
  mutate("% Con respecto al total de la muestra" = paste(round((`# De migrantes`/sum(`# De migrantes`)) *100, 2),"%")) %>%
  select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`))

datatable(Tabla_dep2,  class = 'cell-border stripe')

# Graficas de ingreso promedio por departamento
t2 <- Tabla_dep2 %>% mutate("rank"=min_rank(desc(`Promedio de ingresos`))) %>%
  filter(rank %in% 1:12) 

t2$Departamento <- factor(t2$Departamento, levels = t2$Departamento)
  
# Tabla con los 12 departamentos con MAYOR ingreso promedio por persona
 t2 %>% ggplot(aes(x=Departamento, y= `Promedio de ingresos`, color=Departamento))+
  geom_point(size=7) + geom_segment(aes(x = Departamento,
                                        xend= Departamento,
                                        y=670000,
                                        yend=`Promedio de ingresos`))+ ylim(670000,815000)+
   theme_bw()+ labs(title = "Ingreso económico promedio", subtitle = "De migrantes por departamento")+ 
   xlab("")+ylab("")+scale_x_discrete(breaks="")+
   scale_y_continuous(breaks = c(680000, 700000, 720000, 740000, 760000,780000, 800000  ))+
   theme(text =element_text(size = 14))+
   theme(plot.title = element_text(size = rel(1.1), vjust = 1.5, face = "bold", color="red2") )+ 
   theme(plot.subtitle = element_text(size = rel(1), vjust = 1.5, color="black") )+ 
   theme(axis.title.y = element_text(face = "bold", size= rel(1.1)))+ 
   theme(axis.text.y = element_text(face = "bold")) + scale_color_brewer(palette ="Paired" )+
   geom_text(aes(x=Departamento, label= `Promedio de ingresos`),
             size=3.5, vjust=-1, hjust=0.5, col="black" )
 
 # Tabla con los 12 departamentos con MENOR ingreso promedio por persona
 t3 <- Tabla_dep2 %>% mutate("rank"=min_rank(desc(`Promedio de ingresos`))) %>%
   filter(rank %in% 13:24) 
 
 t3$Departamento <- factor(t3$Departamento, levels = t3$Departamento)
 
 t3 %>% ggplot(aes(x=Departamento, y= `Promedio de ingresos`, color=Departamento))+
   geom_point(size=7) + geom_segment(aes(x = Departamento,
                                         xend= Departamento,
                                         y=460000,
                                         yend=`Promedio de ingresos`))+ ylim(460000,700000)+
   theme_bw()+ labs(title = "Ingreso económico promedio", subtitle = "De migrantes por departamento")+ 
   xlab("")+ylab("")+scale_x_discrete(breaks="")+
   scale_y_continuous(breaks = c(480000, 510000, 540000, 570000, 600000,630000, 660000,690000 ))+
   theme(text =element_text(size = 14))+
   theme(plot.title = element_text(size = rel(1.1), vjust = 1.5, face = "bold", color="red2") )+ 
   theme(plot.subtitle = element_text(size = rel(1), vjust = 1.5, color="black") )+ 
   theme(axis.title.y = element_text(face = "bold", size= rel(1.1)))+ 
   theme(axis.text.y = element_text(face = "bold")) + scale_color_brewer(palette ="Paired" )+
   geom_text(aes(x=Departamento, label= `Promedio de ingresos`),
             size=3.5, vjust=-1, hjust=0.5, col="black" )
 

                                #--------.---------
                    #----------  Analisis por edad ----------
 
<<<<<<< HEAD
 #Para este analisis nos quedamos con los migrantes con un ingreso menor a $2.500.0000
=======
 #Para este analisis nos quedamos con los migrantes con un ingreso menor a $1.800.0000
>>>>>>> Rama-Jose
 t4 <- Dane %>% filter(Ingresos_total<=2500000)%>% group_by(Edad) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total), 0),`# De observaciones`=n() ) %>% 
   arrange(desc(`Ingreso promedio`))
 
 datatable(t4, width = 450, options = list(searching=F, lengthChange=F), class ='cell-border stripe' )
 
 Dane %>% filter(Ingresos_total<=2500000)%>% 
   ggplot(aes(x=Edad, y=Ingresos_total, fill=Edad))+geom_boxplot()+theme_stata(base_size = 15 )+
   labs(title = "INGRESOS SEGÚN RANGO DE EDAD")+xlab("")+ scale_x_discrete(breaks="")+ylab("Ingresos por persona")+
   theme(plot.title = element_text(size = rel(1.1), vjust = 1.5, face = "bold", color="black") )+ 
   theme(axis.title.y = element_text(face = "bold", size= rel(1.1)))+ 
   theme(axis.text.y = element_text(face = "bold"))+ scale_fill_viridis(discrete = T, option = "E", alpha = 0.8)+
   coord_flip()+stat_summary()
   
 
 

                                  #--------.---------
                  #----------  Analisis por nivel educativo ----------
 
  t5 <- Dane %>% filter(Ingresos_total<=2500000)%>% group_by(Nivel.educativo.alcanzado) %>% summarise(Ingreso_promedio=round(mean(Ingresos_total),0),
                                                                                                      mediana=median(Ingresos_total),
                                                                                                      `# De observaciones`=n()) %>% arrange(desc(mediana))
 datatable(t5, options = list(searching=F, lengthChange=F), class ='cell-border stripe' )
 
 Dane$Nivel.educativo.alcanzado <- factor(Dane$Nivel.educativo.alcanzado, levels = t5$Nivel.educativo.alcanzado)
 
 Dane %>% filter(Ingresos_total<=2500000)%>% 
   ggplot(aes(x=Nivel.educativo.alcanzado, y=Ingresos_total, fill=Nivel.educativo.alcanzado))+geom_boxplot()+theme_stata(base_size = 15 )+
   labs(title = "INGRESOS SEGÚN NIVEL EDUCATIVO")+xlab("")+ scale_x_discrete(breaks="")+ylab("Ingresos por persona")+
   theme(plot.title = element_text(size = rel(1.1), vjust = 1.5, face = "bold", color="black") )+ 
   theme(axis.title.y = element_text(face = "bold", size= rel(1.1)))+ 
   theme(axis.text.y = element_text(face = "bold"))+ scale_fill_viridis(discrete = T, option = "E", alpha = 0.8)+
   coord_flip()+stat_summary()
 




                                 #--------.---------
                  #----------  Analisis por Rama de actividad ----------

 # Tabla filtrando los migrantes con ingresos menores a $2.5000.000
 Tabla_rama1 <- Dane %>% filter(Ingresos_total<=2500000)%>%  group_by(Rama.actividad) %>% 
   summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
             "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
             "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
   mutate("Porcentaje de la muestra" = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>%
   mutate("Rama.actividad"=unique(Rama.actividad)) %>% select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`)) 
 

# Tabla sin ningún tipo de filtro
  Tabla_rama2 <- Dane %>%  group_by(Rama.actividad) %>% 
   summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
             "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
             "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>%
   mutate("Porcentaje de la muestra" = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>%
   mutate("Rama.actividad"=unique(Rama.actividad)) %>% select(1,5,2,8,3,4,6,7)%>% arrange(desc(`Promedio de ingresos`))
 
 
datatable(Tabla_rama1,  class = 'cell-border stripe')
datatable(Tabla_rama2,  class = 'cell-border stripe')


                                 #--------.---------
                #----------  Analisis por variables en conjunto ----------
# Tabla de departamento y rama de actividad filtrando las personas con ingresos menores a $2.500.000
# y filtrando las ramas que tengan mas de 5 observaciones en cada departamento
Tabla_Dep_Rama_3 <- Dane %>% filter(Ingresos_total<=2500000)%>%  group_by(Departamento, Rama.actividad) %>% 
  summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>% group_by(Departamento) %>% 
  mutate("Porcentaje del departamento" = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>% 
  filter(`# De migrantes en esta rama` >=5) %>%
  mutate("rank"=min_rank(desc(`Promedio de ingresos`))) %>% filter(rank %in% 1:3) %>% arrange(Departamento, desc(`Promedio de ingresos`)) %>% 
  select(1,2,6,3,9,4,5,7,8) 

#Tabla de departamento y rama de actividad filtrando las observaciones con ingresos menores a $2.5000.000
Tabla_Dep_Rama_2 <- Dane %>%  filter(Ingresos_total<=2500000)%>% group_by(Departamento, Rama.actividad) %>% 
  summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
            "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
            "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% 
  ungroup() %>% group_by(Departamento) %>% 
  mutate("Porcentaje del departamento" = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>% 
  mutate("rank"=min_rank(desc(`Promedio de ingresos`))) %>% filter(rank %in% 1:3) %>% arrange(Departamento, desc(`Promedio de ingresos`)) %>% 
  select(1,2,6,3,9,4,5,7,8)

#Tabla de departamento y rama de actividad sin ningún filtro de ingresos
  Tabla_Dep_Rama_1 <- Dane %>%  group_by(Departamento, Rama.actividad) %>% 
    summarise("# De migrantes en esta rama"=n(), "Q.1"=quantile(Ingresos_total, 0.25),
              "Mediana"=median(Ingresos_total), "Promedio de ingresos"=round(mean(Ingresos_total),0),
              "Q.3"=quantile(Ingresos_total, 0.75), "Ingreso maximo"=max(Ingresos_total)) %>% ungroup() %>% group_by(Departamento) %>% 
    mutate("Porcentaje del departamento" = paste(round((`# De migrantes en esta rama`/sum(`# De migrantes en esta rama`)) *100, 2),"%")) %>% 
    mutate("rank"=min_rank(desc(`Promedio de ingresos`))) %>% filter(rank %in% 1:3) %>% arrange(Departamento, desc(`Promedio de ingresos`)) %>% 
      select(1,2,6,3,9,4,5,7,8)
  
  datatable(Tabla_Dep_Rama_3, class ='cell-border stripe' )
  datatable(Tabla_Dep_Rama_2, class ='cell-border stripe' )
  datatable(Tabla_Dep_Rama_1, class ='cell-border stripe' )
  
  
    # Grafico y tabla de ingresos según género y edad

  Ingresos <- Dane  %>% filter(Ingresos_total<=1800000, Edad!="(11,14]") %>%  group_by(Genero, Edad) %>% 
    summarise(`Ingreso promedio`=round(mean(Ingresos_total),2), `# De observaciones`=n()) %>% 
    arrange(desc(`Ingreso promedio`))

  datatable(Ingresos, class ='cell-border stripe' )
  
  Dane %>% filter(Ingresos_total<=1800000, Edad!="(11,14]") %>% 
    ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 60,fill="skyblue", color="black", alpha=0.8)+
    ggtitle("Distribución de ingresos por edad y genéro")+
    xlab("Ingresos totales")+ylab("") + theme_bw() + 
    theme(text =element_text(size = 14), legend.position = "none")+
    theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
    theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
    theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
    theme(axis.text.x = element_text(face = "bold", size = rel(0.9)))+
    theme(axis.text.y = element_text(face = "bold"))+ 
    scale_x_continuous(limits = c(0,1800000))+
    geom_vline(data=Ingresos, aes(xintercept=`Ingreso promedio`, color=Genero),
               linetype="dashed", size=1.2)+
    scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
    facet_grid(Genero~Edad)

  
  
  
  # Tabla y grafico de ingresos según la edad y el nivel educativo
  Ingresos2 <- Dane %>% filter(Ingresos_total<=2500000) %>% filter(!(Nivel.educativo.alcanzado=="Preescolar")) %>% 
    group_by(Edad, Nivel.educativo.alcanzado) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2), `# De observaciones`=n()) %>% 
    arrange(desc(`Ingreso promedio`))
  
  datatable(Ingresos2, class ='cell-border stripe')
  
  p <-  Dane %>% filter(Ingresos_total<=2500000,Edad!="(11,14]") %>% filter(!(Nivel.educativo.alcanzado=="Preescolar")) %>% 
    group_by(Edad) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2), `# De observaciones`=n()) %>% 
    arrange((`Ingreso promedio`))
  
  Dane$Edad <- factor(Dane$Edad, levels = p$Edad)
  
  I <- Dane %>% filter(Ingresos_total<=2500000, Edad!="(11,14]") %>% filter(!(Nivel.educativo.alcanzado=="Preescolar")) %>% 
    group_by(Edad, Nivel.educativo.alcanzado) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2), `# De observaciones`=n()) %>% 
    arrange(desc(`Ingreso promedio`))
  
  Dane %>% filter(Ingresos_total<=2000000, Edad!="(11,14]") %>% filter(!(Nivel.educativo.alcanzado=="Preescolar")) %>% 
    ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 60,fill="skyblue", color="black", alpha=0.8)+
    ggtitle("Distribución de ingresos por edad y nivel educativo")+
    xlab("Ingresos totales")+ylab("") + theme_bw() + 
    theme(text =element_text(size = 14), legend.position = "none")+
    theme(plot.title = element_text(size = rel(1.3),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
    theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
    theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
    theme(axis.text.x = element_text(face = "bold", size=rel(0.8)))+
    theme(axis.text.y = element_text(face = "bold"))+ 
    scale_x_continuous(limits = c(0,2000000))+
    geom_vline(data=I, aes(xintercept=`Ingreso promedio`, color=Edad),
               linetype="dashed", size=1.2)+scale_color_brewer(palette = "Set1")+
   facet_grid(Edad~Nivel.educativo.alcanzado)


  