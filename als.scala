var rawData = sc.textFile("data/ratings.csv")
var header = rawData.first()
val data = rawData.filter(row => row != header)
val rawRatings = data.map(_.split(",").take(3))


