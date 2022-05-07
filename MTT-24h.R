library ("pacman")

p_load("ggplot2", #Para gráficos
       "vroom", #Para cargar archivos
       "dplyr", #para usar magriter
       "ggrepel", #
       "ggsci",
       "scales",
       "Rmisc",
       "tidyverse",
       "forcats")  #Para paleta de colores

#los datos que se presentan son evaluaciones de 8-OH en células C6, fueron sembradas
#se les permitió adherencia y a las 24 h se le spuso tratamiento o DMSO
#a las 24 h se administró MTT y se midió absorbancias
#Los resultados de las absorbancias, se normalizaron contra un pozo sin tratamiento
#Los datos mostrados son para obtener el valor cercano a la IC50 

datos_8OH <- vroom(file = "https://raw.githubusercontent.com/ManuelLaraMVZ/Datos-8-OH-Gas1/main/MTT-24h_8-OH.csv")

head(datos_8OH)


#Generar nuevas columnas donde incluímos el promedio

agrupados <- datos_8OH %>% 
  group_by( Nombre ) %>% 
  mutate(promedio_concentraciones=mean(Porcentaje)) %>% 
  ungroup()

head(agrupados)
#seleccionamos solo las columnas de interés

segmentos_promedio <- agrupados %>% 
  select(Nombre,
         promedio_concentraciones,
         Orden) %>% 
  unique (  )
segmentos_promedio

#necesitamos sacar los datos y convertirlos a un objeto
#Names<- segmentos_promedio %>% 
#  select(Nombre)
  
#Names

  #No se pudo, mejor se intenta como caracteres
#Names2 <- as.character(Names)

#obtenemos los promedios y e.e.m.
resumen <- summarySE(data = agrupados, measurevar="Porcentaje", groupvars=c("Orden"))

resumen

resumen2 <- resumen %>% 
  select("Orden", "Porcentaje", "se")

resumen2
#le agregamos los nombres de las columnas

valores <- c("Control", "DMSO", "0.98 µM", "1.95 µM", "3.91 µM", "7.81 µM", 
             "16.63 µM", "31.25 µM", "62.50 µM", "125.00 µM", "250.00 µM",
             "500.00 µM",
             "750.00 µM", "1,000.00 µM")
valores

#agregamos la columna de nombres
resumen3 <- resumen2 %>% 
  mutate(Nombres=valores)

resumen3

#gráfica

grafica1 <- ggplot(data=resumen3,
                   mapping = aes(x=fct_inorder(Nombres),
                                 y=Porcentaje,
                                 fill=Nombres))+
  geom_col(stat = "identity",colour="black", size=.8)+
  theme_classic()
grafica1

grafica2 <- grafica1+
  scale_y_continuous(breaks = seq(from=0, to=120, by=20), 
                     labels=comma)+
  geom_errorbar( aes(x=Nombres, ymin=Porcentaje-se, 
                     ymax=Porcentaje+se), 
                 width=0.2, colour="black", alpha=1, size=.8)+
  xlab("8-HQ Concentration")+
  ylab("Viability by MTT (%)")


grafica2  

grafica3 <- grafica2+
  theme(plot.title= element_text(size=10))+
  scale_x_discrete(expand = c(0.05,0))+
  theme(axis.line = element_line(size = 0.8))+
  theme(axis.ticks.x = element_line(size = 0.8))+
  theme(axis.ticks.y = element_line(size = 0.8))
grafica3  

grafica4 <- grafica3+
  theme(text = element_text(size=20))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position="none")+
  theme(axis.text.x=element_text(angle=45,
                                 hjust=1))+
  scale_fill_simpsons()


grafica4

#ajustar ejes
miny=0
maxy=110

marcasy <- seq(from=miny,
               to=maxy,
               by=10)

grafica5 <- grafica4+
  scale_y_continuous(limits=c(miny,maxy), #colocamos los límites del eje y
                     breaks=marcasy,
                     expand=c(0,0.2)) # que marcas queremos

grafica5 

ggsave(filename="Tratamiento_24h.png",
       plot = grafica5,
       dpi = 300,
       height = 7,
       width = 7)

                    
