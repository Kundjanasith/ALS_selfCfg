import scala.io.Source
import scala.collection.mutable.ListBuffer

val filename = "data/rat.csv"
val users = new ListBuffer[Int]()
val movies = new ListBuffer[Int]()
// Define rating
class Rating( uid: Int, mid: Int, rat: Double ){
    var user_id: Int = uid
    var movie_id: Int = mid
    var rate: Double = rat
    users += uid
    movies += mid
    def getUserId(): Int = user_id
    def getMovieId(): Int = movie_id
    def getRate(): Double = rate
    override def toString: String = s" user_id: $user_id , movie_id: $movie_id, rate: $rate"  
}
// Read file assign rating
var ratings = new ListBuffer[Rating]()
var ratingArr = ratings.to[Array]
var numLine: Int = 0
for (line <- Source.fromFile(filename).getLines()) {
  numLine += 1
  if(numLine>1) {
    val uid = line.split(",")(0).toInt 
    val mid = line.split(",")(1).toInt
    val rat = line.split(",")(2).toDouble
    var temp = new Rating(uid,mid,rat)
    ratings += temp
  }
}
// check Pivot
var ratingArr = ratings.to[Array]
def createPivotQ( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = r.getRate()
    }
  }
  return res
}
def createPivotW( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratingArr ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = 1.0
    }
  }
  return res
}
// Parameter
val lambda_ = 0.1
val n_factors = 100
val m = ( users.to[Array].reduceLeft(_ max _) - users.to[Array].reduceLeft(_ min _) ) + 1
val n = ( movies.to[Array].reduceLeft(_ max _) - movies.to[Array].reduceLeft(_ min _) ) + 1
val n_iterations = 20

// val m = 2
// val n = 3
// val n_factors = 4

def mult[A](a: Array[Array[A]], b: Array[Array[A]])(implicit n: Numeric[A]) = {
  import n._
  for (row <- a)
  yield for(col <- b.transpose)
  yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
}
// Matrix X
var matrixX = new ListBuffer[Array[Double]]()
for( i <- 0 to m - 1 ){
  var lineArr = new Array[Double](n_factors)
  for( j <- 0 to n_factors - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  matrixX += lineArr
}
val XmatrixX = matrixX.to[Array]
// Matrix Y
var matrixY = new ListBuffer[Array[Double]]()
for( i <- 0 to n_factors - 1 ){
  var lineArr = new Array[Double](n)
  for( j <- 0 to  n - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  matrixY += lineArr
}
val YmatrixY = matrixY.to[Array]
// XY = X * Y
val matrixXY = mult(XmatrixX,YmatrixY)
// MatrixQ
var matrixQ = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotQ( i, j )
  }
  matrixQ += lineArr
}
// MatrixW
var matrixW = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotW( i, j )
  }
  matrixW += lineArr
}
// Q - XY
var matrixQXY = new ListBuffer[Array[Double]]()
for( i <- 0 to m - 1 ){
  var lineArr = new Array[Double](n)
  for( j <- 0 to n - 1 ){
    lineArr(j) = matrixQ.apply(i).apply(j) - matrixXY.apply(i).apply(j)
  }
  matrixQXY += lineArr
}
// W * ( Q - XY )
var matrixWQXY2 = new ListBuffer[Array[Double]]()
for( i <- 0 to m - 1 ){
  var lineArr = new Array[Double](n)
  for( j <- 0 to n - 1 ){
    lineArr(j) = matrixQ.apply(i).apply(j) * matrixQXY.apply(i).apply(j)
  }
  matrixWQXY2 += lineArr
}
val errorArr = mult(matrixWQXY2.to[Array],matrixWQXY2.to[Array])
def sumArr( arr: Array[Double] ): Double =  {
  var res = 0.0 
  for(a <- 0 to arr.length -1 ){
    res += arr(a)
  }
  return res
}
var errorSum  = sumArr(errorArr)

