rm(list = ls())

library(NbClust)
library(factoextra)
library(cluster)
library(stringr)
library(ggdendro)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

maxmin = function(list) {
  min = min(list)
  max = max(list)
  
  list = (list - min(list))/(max(list) - min(list))
  
  return(list)
}

euclidean_distance <- function(df) {
  distances <- dist(df, method = "euclidean")
  
  return(distances)
}

euclidean_matrix <- function(df) {
  distances <- euclidean_distance(df)
  distancesMatrix <- as.matrix(distances)
  rownames(distancesMatrix) <- colnames(distancesMatrix) <- rownames(df)
  
  return(distancesMatrix)
}

movieDatabase = read.csv('resources/tmdb_5000_movies.csv')
audience = read_delim("resources/letterboxd.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

audience$publicscore = as.numeric(gsub(",",".",audience$publicscore))

movies = merge(movieDatabase, audience, by="id")
movies = movies[complete.cases(movies$publicscore), ]
movies = movies[order(movies$title),]

for (movie in seq_len(length(movies$title)-1)) {
  if (movies$title[movie] == movies$title[movie+1]) {
    movies$title[movie] = paste(movies$title[movie],"-",substr(movies$release_date[movie],1,4))
    movies$title[movie+1] = paste(movies$title[movie+1],"-",substr(movies$release_date[movie+1],1,4))
  }
}

movies$genres = str_replace(
  str_replace(
    str_replace(
      str_replace(
        str_replace(movies$genres,'\\[','')
        ,'\\]','')
      ,'\\{\"id\": ','')
    ,'\"name\": ','')
  ,'\\}','')

for (genre in seq_len(length(movies$title)-1)){
  movies$genres[genre] = 
    str_replace(str_replace(strsplit(movies$genres,",")[[genre]][2]," \"",''),"\"",'')
}

movies = movies[movies$budget > 100000 & movies$revenue > 50000,]

movies = movies %>%
  mutate(across(c("budget",
                  "popularity",
                  "revenue",
                  "runtime",
                  "vote_average",
                  "publicscore"), as.numeric))

movies = movies %>%
  mutate(en_flag = ifelse(original_language == "en", 1, 0))

movies = movies %>%
  mutate(decade = 10 * (year(release_date) %/% 10))

movies = subset(movies, genres != "Foreign")

moviesList = split(movies, movies$genres)

moviesList = lapply(moviesList, function(df) {
  df[, c("budget",
         "popularity",
         "revenue",
         "runtime",
         "vote_average",
         "publicscore",
         "decade")] = lapply(df[, c("budget",
                                    "popularity",
                                    "revenue",
                                    "runtime",
                                    "vote_average",
                                    "publicscore",
                                    "decade")], maxmin)
  return(df)
})

normList = lapply(moviesList, function(df) df[, c("budget",
                                                  "popularity",
                                                  "revenue",
                                                  "runtime",
                                                  "vote_average",
                                                  "publicscore",
                                                  "decade",
                                                  "en_flag")])

#normList = lapply(normList, function(df) row.names(df) = moviesList$df$id)

distancesMatrix = lapply(normList, euclidean_matrix)
distancesList = lapply(normList, euclidean_distance)

wardList = vector("list", length = length(distancesList))

for (euc in seq_len(length(distancesList))) {
  ward_euc <- hclust(d = distancesList[[euc]], method = "ward.D")
  wardList[[paste0("ward.d", euc)]] <- ward_euc
}

#ggdendrogram(wardList$ward.d18, rotate = TRUE, size = 0.5) + theme(text = element_text(size = 8))


