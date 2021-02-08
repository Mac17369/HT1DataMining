
library(nortest)
data <-read.csv("C:/Users/LENOVO/Desktop/Clases/Miner�a de datos/HT/HT1DataMining/data/tmdb-movies.csv")

# Haga una exploraci�n r�pida de sus datos,para eso haga un resumen de su conjunto de datos.

summary(data)

# Diga el tipo de cada una de las variables(cualitativaordinal o nominal, cuantitativa continua, cuantitativa discreta)  
## Solucion

{r }
str(data)

# Investigue si las variables cuantitativas siguen una distribuci�n normal y haga una tabla de frecuencias de las variables cualitativas. Explique todos los resultados. 
## Soluci�n:  
#Antes que todo, es importante remarcar que las variables id, imbd_id, original_title, cast, tagline y overview, son meras variables cuantitativas que no exiben ning�n comportamiento  estad�stico. Por ejemplo,  si hicieramos una tabla de  frecuencias o un test de normalidad estas variables no exibir�an ning�n comportamiento debido a que son etiquetas para cada pel�cula.

### Variables cuantitativas.

#### Popularidad: 


hist(data$popularity[data$popularity != 0],
     main = "Histograma sobre popularidad de pel�culas en imdb",
     xlab = "Indice de popularidad ",
     breaks = 105)

qqnorm(data$popularity, pch = 1, frame = FALSE)
qqline(data$popularity, col = "steelblue", lwd = 2)
lillie.test((data$popularity))

#### Presupuesto 
data2 = data[data$budget > 1000000,]
hist(data2$budget,
     main = "Histograma para el presupuesto de las pel�culas",
     xlab = "Presupuesto",
     breaks = 105)

qqnorm(data2$budget, pch = 1, frame = FALSE)
qqline(data2$budget, col = "steelblue", lwd = 2)
lillie.test((data2$budget))

#### Ganancias 
data21 = data[data$revenue > 1000000,]
hist(data21$budget,
     main = "Histograma para las ganancias de las pel�culas",
     xlab = "Presupuesto",
     breaks = 100)
qqnorm(data21$revenue, pch = 1, frame = FALSE)
qqline(data2$revenue, col = "steelblue", lwd = 2)

#### Duraci�n

data3 = data[data$runtime > 30,]
hist(data2$runtime,
     main = "Histograma para la duraci�n de las pel�culas",
     xlab = "Tiempo (m)",
     breaks = 105)

# Variables cualitativas: 

### Generos
splitPopGenres <- c(data$genres)

splitPopGenres <- strsplit(splitPopGenres, "|", fixed = TRUE)

splitPopGenres <- unlist(splitPopGenres)

splitPopGenres <- table(splitPopGenres)

barplot(splitPopGenres, xlab = 'Frecuencia', main = ' Generos' )

### Directores.  
directores = c(data$director)
directores <- unlist(directores)
directores <- table(directores)

barplot(directores, xlab = 'Frecuencia', main = ' directores' )

###Compa�ias de producci�n 

production_companies = c(data$production_companies)
production_companies <- unlist(production_companies)
production_companies <- table(production_companies)

barplot(production_companies, xlab = 'Frecuencia', main = ' Compa�ias de producci�n' )


### Elenco 
splitcasts <- c(data$cast)

splitcasts <- strsplit(splitcasts, "|", fixed = TRUE)

splitcasts <- unlist(splitcasts)

splitcasts <- table(splitcasts)

barplot(splitcasts, xlab = 'Frecuencia', main = 'Actores' )

# Responda las siguientes preguntas . 

## C�ales son las pel�culas que costaron m�s presupuesto?

data5 <- data[order(-data$budget),]
head(data5$original_title,10)

## �Cu�les son las 10 pel�culas que m�s ingresos tuvieron?  

  

data6 <- data[order(-data$revenue),]
head(data6$original_title,10)


#setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT1DataMining/data")
movies <- read.csv('D:/AxelFolder/University/Miner�a de Datos/HT1DataMining/data/tmdb-movies.csv')
show(movies)

library(tidyr)
mov <- separate(movies, col = "genres", into = c("primary_genre", "secondary_genre", sep = "|"))


#pelicula mas votada
maxVote <- which(movies$vote_count == max(movies$vote_count))
movies[maxVote,]

#pelicula peor votada
worstMovie <- which(movies$vote_average == min(movies$vote_average))
movies[worstMovie,]

#anios en los que mas se hicieron peliculas
library(RColorBrewer)
col1 <- brewer.pal(12,'Set3')
realiseYear <- table(movies$release_year)
barplot(realiseYear, ylab = 'Frecuencia', main = 'Peliculas por anio', col=col1, las=3)

#20 peliculas mas populares y genero
popMovies <- mov[order(-vote_average),]
popMovies <- head(popMovies, 20)
popMovies <- table(popMovies$primary_genre)
library(dplyr)

col2 <- brewer.pal(12,'Paired')
barplot(popMovies, ylab = 'Frecuencia', main = 'Generos de 20 peliculas mas populares', col=col2, las=3)
#hay una pelicula que no tiene genero registrado Dr, Who

#generos que predominan en la base de datos
#Separando generos 
splitPopGenres <- c(movies$genres)
splitPopGenres
splitPopGenres <- strsplit(splitPopGenres, "|", fixed = TRUE)
splitPopGenres
splitPopGenres <- unlist(splitPopGenres)
splitPopGenres
splitPopGenres <- table(splitPopGenres)
splitPopGenres

col3 <- brewer.pal(9,'Pastel1')
barplot(splitPopGenres, horiz = TRUE, xlab = 'Frecuencia', main = '10 Generos mas frecuentes en base de datos', col=col3, las = 1)

#las peliculas de que genero prinipal obtuvieron mayores ganancias
mostRevenue <- mov[order(-revenue),]
mostRevenue <- head(mostRevenue, 20)
select(mostRevenue, original_title, revenue, primary_genre)

#las peliculas de que genero prinipal obtuvieron menores ganancias
cleanMov <- mov[mov$revenue != 0,]
revenueOrder <- order(cleanMov$revenue)
cleanMov <- cleanMov[revenueOrder,]
lessRevenue <- cleanMov[order(revenue),]
lessRevenue <- head(lessRevenue, 20)
select(lessRevenue, original_title, revenue, primary_genre)

movies<-read.csv("D:/AxelFolder/University/Minería de Datos/HT1DataMining/data/tmdb-movies.csv")
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