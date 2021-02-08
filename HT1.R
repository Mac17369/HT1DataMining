
library(nortest)
data <-read.csv("C:/Users/LENOVO/Desktop/Clases/Minería de datos/HT/HT1DataMining/data/tmdb-movies.csv")

# Haga una exploración rápida de sus datos,para eso haga un resumen de su conjunto de datos.

summary(data)

# Diga el tipo de cada una de las variables(cualitativaordinal o nominal, cuantitativa continua, cuantitativa discreta)  
## Solucion

{r }
str(data)

# Investigue si las variables cuantitativas siguen una distribución normal y haga una tabla de frecuencias de las variables cualitativas. Explique todos los resultados. 
## Solución:  
#Antes que todo, es importante remarcar que las variables id, imbd_id, original_title, cast, tagline y overview, son meras variables cuantitativas que no exiben ningún comportamiento  estadístico. Por ejemplo,  si hicieramos una tabla de  frecuencias o un test de normalidad estas variables no exibirían ningún comportamiento debido a que son etiquetas para cada película.

### Variables cuantitativas.

#### Popularidad: 


hist(data$popularity[data$popularity != 0],
     main = "Histograma sobre popularidad de películas en imdb",
     xlab = "Indice de popularidad ",
     breaks = 105)

qqnorm(data$popularity, pch = 1, frame = FALSE)
qqline(data$popularity, col = "steelblue", lwd = 2)
lillie.test((data$popularity))

#### Presupuesto 
data2 = data[data$budget > 1000000,]
hist(data2$budget,
     main = "Histograma para el presupuesto de las películas",
     xlab = "Presupuesto",
     breaks = 105)

qqnorm(data2$budget, pch = 1, frame = FALSE)
qqline(data2$budget, col = "steelblue", lwd = 2)
lillie.test((data2$budget))

#### Ganancias 
data21 = data[data$revenue > 1000000,]
hist(data21$budget,
     main = "Histograma para las ganancias de las películas",
     xlab = "Presupuesto",
     breaks = 100)
qqnorm(data21$revenue, pch = 1, frame = FALSE)
qqline(data2$revenue, col = "steelblue", lwd = 2)

#### Duración

data3 = data[data$runtime > 30,]
hist(data2$runtime,
     main = "Histograma para la duración de las películas",
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

###Compañias de producción 

production_companies = c(data$production_companies)
production_companies <- unlist(production_companies)
production_companies <- table(production_companies)

barplot(production_companies, xlab = 'Frecuencia', main = ' Compañias de producción' )


### Elenco 
splitcasts <- c(data$cast)

splitcasts <- strsplit(splitcasts, "|", fixed = TRUE)

splitcasts <- unlist(splitcasts)

splitcasts <- table(splitcasts)

barplot(splitcasts, xlab = 'Frecuencia', main = 'Actores' )

# Responda las siguientes preguntas . 

## Cúales son las películas que costaron más presupuesto?

data5 <- data[order(-data$budget),]
head(data5$original_title,10)

## ¿Cuáles son las 10 películas que más ingresos tuvieron?  

  

data6 <- data[order(-data$revenue),]
head(data6$original_title,10)


#setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT1DataMining/data")
movies <- read.csv('D:/AxelFolder/University/Minería de Datos/HT1DataMining/data/tmdb-movies.csv')
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

