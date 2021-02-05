setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT1DataMining/data")
movies <- read.csv('tmdb-movies.csv')
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

