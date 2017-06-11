tags <- read.csv('data/tags.csv')
print(head(tags))
print(nrow(tags))

movies <- read.csv('data/movies.csv')
print(head(movies))
print(nrow(movies))

ratings <- read.csv('data/ratings.csv')
print(head(ratings))
print(nrow(ratings))

total <- merge(tags,movies,by="movie_id")
total <- merge(total,ratings,by="movie_id")
print(head(total))
print(nrow(total))
