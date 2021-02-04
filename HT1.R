setwd("C:/Users/Kevin Macario/Desktop/Uvg/9no Semestre/Mineria de Datos/HT1DataMining/data")
movies <- read.csv('tmdb-movies.csv')
show(movies)

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

#20 peliculas mas populares y generp
popMovies <- movies[order(-movies$vote_average),]
popMovies <- head(popMovies, 20)
library(dplyr)
popMovies <- select(popMovies, original_title, genres)
popMovies
popMoviesPerGenre <- table(popMovies$genres)
col2 <- brewer.pal(12,'Paired')
barplot(popMoviesPerGenre, ylab = 'Frecuencia', main = 'Generos de 20 peliculas mas populares', col=col2, las=3)

#generos que predominan en la base de datos
popGenres <- table(movies$genres)
popGenres <- popGenres[order(-popGenres)]
popGenres <- head(popGenres,10)
col3 <- brewer.pal(9,'Pastel1')
barplot(popGenres, horiz = TRUE, xlab = 'Frecuencia', main = '10 Generos mas frecuentes', col=col3, las = 1)

#las peliculas de que genero prinipal obtuvieron mayores ganancias
maxRevenue <- table(movies$revenue)
maxRevenue <- maxRevenue[order(-maxRevenue)]
maxRevenue <- head(maxRevenue,10)
maxRevenue
col2 <- brewer.pal(12,'Paired')
barplot(maxRevenuePerGenre$genres, ylab = maxRevenuePerGenre$revenue, main = 'Generos de 20 peliculas mas populares', col=col2, las=3)

