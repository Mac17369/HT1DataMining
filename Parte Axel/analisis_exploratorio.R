movies<-read.csv("D:/AxelFolder/University/Minería de Datos/HT1DataMining/data/tmdb-movies.csv")
View(movies)
str(movies)
#4.10
directores <- movies[!is.na(movies$director), ]
directores <-  directores[!directores$director == "",]
directores <- directores[order(-directores$vote_average), ]
directores <- head(directores, 20)
tabla_directores <- directores[c("director", "vote_average")]
names(tabla_directores) <- c("Director", "Calificación")
View(tabla_directores)
#4.11
correlacion <- movies[c("budget", "revenue")]
library(psych)
pairs.panels(correlacion)
#4.12
library(lubridate)
movies$month <- vapply(strsplit(movies$release_date, "/"), `[`, 1, FUN.VALUE=character(1))
asociacion <- movies[c("month", "revenue")]
pairs.panels(asociacion)
#4.13
library(plyr)
lanzamientos <- count(movies, "month")
lanzamientos <- lanzamientos[order(-lanzamientos$month), ]
names(lanzamientos) <- c("Mes", "Lanzamientos")
View(lanzamientos)
#4.14
correlacion1 <- movies[c("vote_average", "revenue")]
library(psych)
pairs.panels(correlacion1)
#4.15
duracion <- aggregate(duracion$runtime, by=list(Genero=duracion$genres), sum)
duracion <- duracion[order(-duracion$x), ]
names(duracion) <- c("Genero", "Duración")
