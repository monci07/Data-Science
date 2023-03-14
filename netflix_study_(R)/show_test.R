#Para instalar y utilizar paquetes:
# install.packages('ggplot2')
# library(ggplot2)
df <- read.csv('titles.csv')
df <- df[complete.cases(df[,c('imdb_score','tmdb_score')]),]
netflix_s <- df[df$type == 'SHOW',]
# si me quiero quedar con los renglones es: df[c(),]
# si me quiero quedar con las columnas es: df[,c()]
netflix_s_t <- netflix_s[,c('runtime','imdb_score')]

netflix_s_t

# si me quiero una columna como:
#  -vector: nombre_t$nombre_c
#  -dataframe: nombre_t['nombre_c']
runtime_std = sd(netflix_s_t$runtime)
runtime_mean = mean(netflix_s_t$runtime) 
imdb_score_std = sd(netflix_s_t$imdb_score)
imdb_score_mean = mean(netflix_s_t$imdb_score)

netflix_s_t_su <- data.frame(
  "runtime_SU"= (netflix_s_t$runtime-runtime_mean)/runtime_std,
  "imdb_score_SU"= (netflix_s_t$imdb_score-imdb_score_mean)/imdb_score_std
)

r = mean(netflix_s_t_su$runtime_SU*netflix_s_t_su$imdb_score_SU)
slope = r * (imdb_score_std/runtime_std)
intersect = imdb_score_mean - slope * runtime_mean

plot(netflix_s_t_su$runtime_SU,netflix_s_t_su$imdb_score_SU,col="blue", pch=19)
netflix_s_t_su$prediction = r * netflix_s_t_su$runtime_SU
points(netflix_s_t_su$runtime_SU,netflix_s_t_su$prediction,col="red", pch=19)
legend(5,-4, legend=(c('Data in standard units', 'regresion line')), pch=c(19,19), col=c('blue','red'))

netflix_s_t$prediction = (netflix_s_t_su$prediction*imdb_score_std)+imdb_score_mean
plot(netflix_s_t$runtime,netflix_s_t$imdb_score,col="blue", pch=19)
points(netflix_s_t$runtime,netflix_s_t$prediction,col="red", pch=19)
legend(145,3, legend=(c('Real data', 'regresion line')), pch=c(19,19), col=c('blue','red'))