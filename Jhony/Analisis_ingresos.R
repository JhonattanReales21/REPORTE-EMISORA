library(tidyverse)
library(funModeling)
library(DT)
library(RColorBrewer)

Departamentos <- c("Antioquia", "Atlantico", "Bogota", "Bolivar", "Boyaca", "Caldas",
                   "Caqueta", "Cauca", "Cesar", "Cordoba", "Cundinamarca", "Choco", 
                   "Huila", "La Guajira", "Magdalena", "Meta", "Nariño", "Norte de Santander",
                   "Quindio", "Risaralda", "Santander", "Sucre", "Tolima", "Valle")
                   
Depto <- c(5, 8,11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 52, 54, 63, 66, 68, 70, 73, 76 )


#------------------------------------------    EDA INCOMES   --------------------------------------------------------

#Dane <- read.csv("Dane_Reducida_Total.csv", header = T, sep = ",", dec = ".")

# Eliminamos a todas las personas que no declararon ingresos, es decir, tenian N.A o 98/99
#Dane <- Dane %>% filter(!((ocupacion =="Asalariado") & (is.na(P6500.Cuanto.ganó.el.ultimo.mes.)) |
                  #(ocupacion =="No asalariado") & (is.na(P6750.Ganancia.neta.por.honorarios.o.negocio)) |
                  #(ocupacion =="Desempleado") & (is.na(P7422S1))|
                  #P7422S1 %in% c(98,99)))

#Nos quedamos con 17,950


Dane <- read.csv("Dane_Paramodelos_regresion.csv", header = T, sep = ",", dec = ".")


                                              #--------.---------
                        #----------  Analisis por segmentos de ocupacion ----------

# Observamos la frecuencia de ocupación en la base de datos
describe(Dane$ocupacion)

# Miramos el estado de las variables
df_status(Dane)


                      ##### 1. ASALARIADOS #####

# Filtramos las observaciones que ganen menos de 2.5 millones
filter(Dane, Ingresos_total>=2500000 & ocupacion=="Asalariado") %>% nrow() # Solo 104 asalariados ganan mas de $2.500.000
prueba <- Dane %>% filter(ocupacion=="Asalariado", Ingresos_total<=2500000)
describe(prueba$Ingresos_total)
summary(prueba$Ingresos_total)


# Observamos los valores outliers del ingreso 
prueba %>% ggplot(aes(x=ocupacion, y=Ingresos_total)) + geom_boxplot()+
  ggtitle("Ingresos en el ultimo mes para Asalariados")+
  xlab("")+ylab("Monto $") + theme_bw()

# Observamos la distribución de ingresos de los asalariados 
prueba %>% ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 70,fill="skyblue", color="black")+
  ggtitle("Ingresos para Asalariados")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2"))+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1)))+ theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+
  geom_vline(aes(xintercept=mean(Ingresos_total)),
             color="darkblue", linetype="dashed", size=1.2)

#Ingreso prom por genero
Ingresos_1 <- prueba %>% group_by(Genero) %>% summarise(`Ingreso promedio para asalariados`=round(mean(Ingresos_total),2),
                                                           `Mediana de ingresos`=round(median(Ingresos_total),0))

DT::datatable(Ingresos_1,  class = 'cell-border stripe', filter = "none", width = 350, height = 300,
              options = list(searching=F, lengthChange=F) )


# Ingresos de asalariados dependiendo del genero
  prueba %>% ggplot(aes(x=Ingresos_total, fill=Genero)) +
  geom_histogram(bins = 30,color="black", position = "identity", alpha=0.5)+
  ggtitle("Ingresos de asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(face="italic",size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingreso_prom1, aes(xintercept=`Ingreso promedio para asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
 scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue")) 

  
  prueba %>% ggplot(aes(x=Ingresos_total)) +
    geom_histogram(bins = 30,color="black", fill="skyblue", position = "identity", alpha=0.5)+
    ggtitle("Ingresos de asalariados", subtitle = "Con respecto al género")+
    xlab("Ingresos totales")+ylab("") + theme_bw() + 
    theme(text =element_text(size = 14), legend.position = "none")+
    theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
    theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
    theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
    theme(axis.text.x = element_text(face = "bold"))+
    theme(axis.text.y = element_text(face = "bold"))+ 
    scale_x_continuous(limits = c(0,2500000))+
    geom_vline(data=Ingreso_prom1, aes(xintercept=`Ingreso promedio para asalariados`, color=Genero),
               linetype="dashed", size=1.2)+
    scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
    facet_grid(Genero~.)
  
  
                      ##### 2. NO ASALARIADOS ####

# Filtramos las observaciones que ganen menos de 2.5 millones
filter(Dane, Ingresos_total>=2500000 & ocupacion=="No asalariado") %>% nrow() # Solo 117 asalariados ganan mas de $2.500.000
prueba2 <- Dane %>% filter(ocupacion=="No asalariado" & Ingresos_total<=2500000)
describe(prueba2$Ingresos_total)
summary(prueba2$Ingresos_total)

# Observamos los valores outliers del ingreso 
prueba2 %>% ggplot(aes(x=ocupacion, y=Ingresos_total)) + geom_boxplot()+
  ggtitle("Ingresos en el ultimo mes para No asalariados")+
  xlab("")+ylab("Monto $") + theme_bw()

# Observamos la distribución de ingresos de los No asalariados 
prueba2 %>% ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 70,fill="skyblue", color="black")+
  ggtitle("Ingresos para No asalariados")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2"))+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1)))+ theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+
  geom_vline(aes(xintercept=mean(Ingresos_total)),
             color="darkblue", linetype="dashed", size=1.2)

#Ingreso prom por genero
Ingresos_prom2 <- prueba2 %>% group_by(Genero) %>% summarise(`Ingreso promedio para No asalariados`=round(mean(Ingresos_total),2),
                                                        `Mediana de ingresos`=round(median(Ingresos_total),0))

DT::datatable(Ingresos_2,  class = 'cell-border stripe', filter = "none", width = 350, height = 300,
              options = list(searching=F, lengthChange=F) )


# Ingresos de asalariados dependiendo del genero
prueba2 %>% ggplot(aes(x=Ingresos_total, fill=Genero)) +
  geom_histogram(bins = 30,color="black", position = "identity", alpha=0.5)+
  ggtitle("Ingresos de No asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14))+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(face="italic",size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingreso_prom1, aes(xintercept=`Ingreso promedio para asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue")) 


prueba2 %>% ggplot(aes(x=Ingresos_total)) +
  geom_histogram(bins = 30,color="black", fill="skyblue", position = "identity", alpha=0.8)+
  ggtitle("Ingresos de No asalariados", subtitle = "Con respecto al género")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingresos_prom2, aes(xintercept=`Ingreso promedio para No asalariados`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
  facet_grid(Genero~.)




                                ##### 3. AMBOS SECTORES ####


# Observamos la distribución de ingresos de los No asalariados 

Ingresos <- Dane %>% group_by(Genero, ocupacion) %>% summarise(`Ingreso promedio`=round(mean(Ingresos_total),2),
                                                         `Mediana de ingresos`=round(median(Ingresos_total),0))

DT::datatable(Ingresos,  class = 'cell-border stripe', filter = "none", width = 500, height = 500,
              options = list(searching=F, lengthChange=F) )

Dane %>% filter(Ingresos_total<=2500000) %>% 
  ggplot(aes(x=Ingresos_total)) + geom_histogram(bins = 50,fill="skyblue", color="black", alpha=0.8)+
  ggtitle("Distribución de ingresos")+
  xlab("Ingresos totales")+ylab("") + theme_bw() + 
  theme(text =element_text(size = 14), legend.position = "none")+
  theme(plot.title = element_text(size = rel(1.5),hjust=0.5, vjust = 1.5, face = "bold", color="red2") )+ 
  theme(plot.subtitle = element_text(size = rel(1.2),hjust=0.5, vjust = 1.5, color="black") )+ 
  theme(axis.title.x = element_text(face = "bold", size= rel(1.1)))+ 
  theme(axis.text.x = element_text(face = "bold"))+
  theme(axis.text.y = element_text(face = "bold"))+ 
  scale_x_continuous(limits = c(0,2500000))+
  geom_vline(data=Ingresos, aes(xintercept=`Ingreso promedio`, color=Genero),
             linetype="dashed", size=1.2)+
  scale_fill_brewer(palette = "Set1")+scale_color_manual(values = c("red", "blue"))+
  facet_grid(ocupacion~Genero)


















