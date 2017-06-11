import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.control._

def print_readable( arr: ListBuffer[Array[Double]] ){
    print("[")
    for( i <- 0 to arr.length - 1 ){
        print("(")
        for( j <- 0 to arr.apply(0).length - 1 ){
            print( arr.apply(i).apply(j) + ":" )
        }
        print(")")
    }
    println("]")
}

val filename = "data_exp/rate001-010.csv"
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
// Read file
var ratings = new ListBuffer[Rating]()
var ratingArr = ratings.to[Array]
var numLine: Int = 0
println("Start read file ")
for (line <- Source.fromFile(filename).getLines()) {
  numLine += 1
  if(numLine>1) {
    val uid = line.split(",")(0).toInt 
    val mid = line.split(",")(1).toInt
    val rat = line.split(",")(2).toDouble
    var temp = new Rating(uid,mid,rat)
    println(temp)
    ratings += temp
  }
}
println("Stop read file ")

val lambda_ = 0.1
val n_factors = 10
val m = ( users.to[Array].reduceLeft(_ max _) - users.to[Array].reduceLeft(_ min _) ) + 1
val n = ( movies.to[Array].reduceLeft(_ max _) - movies.to[Array].reduceLeft(_ min _) ) + 1
val n_iterations = 10

// Check Pivot Q
def createPivotQ( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratings ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = r.getRate()
    }
  }
  return res
}
var Q = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotQ( i, j )
  }
  Q += lineArr
}
// Check Pivot W
def createPivotW( uid: Int, mid: Int ): Double = {
  var res: Double = 0.0 
  for( r <- ratings ){
    if(r.getUserId()==uid&&r.getMovieId()==mid){
      res = 1.0
    }
  }
  return res
}
var W = new ListBuffer[Array[Double]]()
for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
  var lineArr = new Array[Double](n)
  for( j <- movies.to[Array].reduceLeft(_ min _) to movies.to[Array].reduceLeft(_ max _) ){
    lineArr(j-movies.to[Array].reduceLeft(_ min _)) = createPivotW( i, j )
  }
  W += lineArr
}
// Matrix X
var X = new ListBuffer[Array[Double]]()
for( i <- 0 to m - 1 ){
  var lineArr = new Array[Double](n_factors)
  for( j <- 0 to n_factors - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  X += lineArr
}
// Matrix Y
var Y = new ListBuffer[Array[Double]]()
for( i <- 0 to n_factors - 1 ){
  var lineArr = new Array[Double](n)
  for( j <- 0 to  n - 1 ){
    lineArr(j) = ( Math.random * 5 )
  }
  Y += lineArr
}
// Transe
def transe( arr: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    val temp = arr
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr.apply(0).length - 1 ){
        var lineArr = new Array[Double](arr.length)
        for( j <- 0 to arr.length - 1 ){
            lineArr(j) = temp.apply(j).apply(i)
        }
        res += lineArr
    }
    return res
}
// Mult
def sub_mul( i: Int, j: Int, arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): Double = {
    var res = 0.0
    for( k <- 0 to arr1.apply(0).length - 1 ){
        res += arr1.apply(i).apply(k) * arr2.apply(k).apply(j)
    }
    return res
}
def mult( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var m = arr1.length
    var n = arr2.apply(0).length
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to m-1 ){
        var lineArr = new Array[Double](n)
        for( j <- 0 to n-1 ){
            lineArr(j) = sub_mul( i, j, arr1, arr2 ) 
        }
        res += lineArr
    }
    return res
}
// Inverse 
def assign( posI: Int, posJ: Int, value: Double, arr: ListBuffer[Array[Double]]): ListBuffer[Array[Double]] = {
   var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr.length - 1 ){
        var lineArr = new Array[Double](arr.apply(0).length)
        for( j <- 0 to arr.apply(0).length - 1 ){
            if(i==posI&&j==posJ){
                lineArr(j) = value
            }
            else{
                lineArr(j) = arr.apply(i).apply(j)
            }
        } 
        res += lineArr
    }
    return res
}
def inverse( arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var I = new ListBuffer[Array[Double]]
    var C = new ListBuffer[Array[Double]]
    if(arr1.length!=arr1.apply(0).length) { return null }
    for( i <- 0 to arr1.length - 1 ){
        var lineArrI = new Array[Double](arr1.length)
        var lineArrC = new Array[Double](arr1.length)
        for( j <- 0 to arr1.length - 1){
            if(i==j){
                lineArrI(j) = 1
            }
            else{
                lineArrI(j) = 0
            }
            lineArrC(j) = arr1.apply(i).apply(j)
        }
        I += lineArrI
        C += lineArrC
    }
    for( i <- 0 to arr1.length - 1 ){
        var e = C.apply(i).apply(i)
        if(e==0){
            val loop = new Breaks;
            loop.breakable {
                for( ii <- i+1 to arr1.length - 1 ){
                    if(C.apply(ii).apply(i)!=0){
                        for( j <- 0 to arr1.length - 1 ){
                            e = C.apply(i)(j)
                            C = assign(i,j,C.apply(ii).apply(j),C)
                            C = assign(ii,j,e,C)
                            e = I.apply(i)(j)
                            I = assign(i,j,I.apply(ii).apply(j),I)
                            I = assign(ii,j,e,I)
                        }
                        loop.break
                    }
                }
            }
            e = C.apply(i)(i)
            if(e==0){ return null}
        }
        for( j <- 0 to arr1.length - 1 ){
            C = assign(i,j,C.apply(i).apply(j)/e,C)
            I = assign(i,j,I.apply(i).apply(j)/e,I)
        }
        for( ii <- 0 to arr1.length - 1 ){
            if(ii!=i){
                e = C.apply(ii).apply(i)
                for( j <- 0 to arr1.length - 1 ){
                    C = assign(ii,j,C.apply(ii).apply(j) - e*C.apply(i).apply(j),C)
                    I = assign(ii,j,I.apply(ii).apply(j) - e*I.apply(i).apply(j),I)
                }
            }
        } 
    } 
    return I
}
// Solve
def solve( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    var in_arr1 = inverse(arr1)
    res = mult(inverse(arr1),arr2)
    return res
}
// Eye
def eyeStar( lambda: Double, n: Int): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to n - 1 ){
        var lineArr = new Array[Double](n)
        for( j <- 0 to n - 1 ){
            if(i==j){
                lineArr(j) = 1 * lambda
            }
            else{
                lineArr(j) = 0
            }
        }
        res += lineArr
    }
    return res 
}
// Plus 
def plus( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) + arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// Minus
def minus( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) - arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// Star
def star( arr1: ListBuffer[Array[Double]], arr2: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length - 1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) * arr2.apply(i).apply(j)
        }
        res += lineArr
    }
    return res 
}
// sumR
def sumR( arr: ListBuffer[Array[Double]]): Double = {
    var res = 0.0
    for( i <- 0 to arr.length - 1 ){
        for( j <- 0 to arr.apply(0).length - 1){
            res = res + arr.apply(i).apply(j)
        }
    }
    return res
}
// Get error
def get_error( q: ListBuffer[Array[Double]], x: ListBuffer[Array[Double]], y: ListBuffer[Array[Double]], w: ListBuffer[Array[Double]] ): Double = {
    var xy = mult(x,y)
    var qxy = minus(q,xy)
    var wqxy = star(w,qxy)
    var wqxy2 = star(wqxy,wqxy)
    return sumR(wqxy2)
}   
var errors = new ListBuffer[Double]()
for( ii <- 0 to n_iterations - 1 ){
    println("===================================================")
    var x1 = plus( mult( Y, transe(Y) ) ,  eyeStar( lambda_, n_factors ) )
    var x2 = mult( Y, transe(Q) )
    var x3 = solve(x1,x2)
    X = transe(x3)
    var y1 = plus( mult( transe(X), X ) ,  eyeStar( lambda_, n_factors ) )
    var y2 = mult( transe(X), Q )
    var y3 = solve(y1,y2)
    Y = y3 
    println(ii+" iteration is completed")
    var error = get_error(Q, X, Y, W)
    println("Error of rated movies: "+error)
    errors += error
    println("===================================================")
}
var Q_hat = mult( X, Y )
val movies_id = new ListBuffer[Int]()
val movies_title = new ListBuffer[String]()
class Movie( mid: Int, mti: String){
    movies_id += mid
    movies_title += mti
    def getMovieId(): Int = mid
    def getMovieTitle(): String = mti
    override def toString: String = s" movie_id: $mid , movie_title: $mti"  
}
val filename2 = "data/movies.csv"
var movieZ = new ListBuffer[Movie]()
var numLine1: Int = 0
println("Start read file ")
for (line <- Source.fromFile(filename2).getLines()) {
  numLine1 += 1
  if(numLine1>1) {
    val movie_id = line.split(",")(0).toInt 
    val movie_title = line.split(",")(1).toString
    var temp = new Movie(movie_id,movie_title)
    println(temp)
    movieZ += temp
  }
}
println("Stop read file ")
// Min
def minimum( arr1: ListBuffer[Array[Double]] ): Double = {
    var res = arr1.apply(0).apply(0)
    for( i <- 0 to arr1.length - 1 ){
        for( j <- 0 to arr1.apply(0).length - 1 ){
            if( arr1.apply(i).apply(j) < res ){
                res = arr1.apply(i).apply(j)
            }
        }
    }
    return res 
}
// Max
def maximum( arr1: ListBuffer[Array[Double]] ): Double = {
    var res = arr1.apply(0).apply(0)
    for( i <- 0 to arr1.length - 1 ){
        for( j <- 0 to arr1.apply(0).length - 1 ){
            if( arr1.apply(i).apply(j) > res ){
                res = arr1.apply(i).apply(j)
            }
        }
    }
    return res 
}
// minus_const
def minus_const( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length -1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) - const
        }
        res += lineArr
    }
    return res
}
// const_minus
def const_minus( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length -1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = const - arr1.apply(i).apply(j)
        }
        res += lineArr
    }
    return res
}
// star_const
def star_const( const: Double, arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    for( i <- 0 to arr1.length -1 ){
        var lineArr = new Array[Double](arr1.apply(0).length)
        for( j <- 0 to arr1.apply(0).length - 1 ){
            lineArr(j) = arr1.apply(i).apply(j) * const
        }
        res += lineArr
    }
    return res
}
// argmax
def indiceMax( arr1: Array[Double] ): Int = {
    var index = 0
    var max = arr1.apply(0)
    for( i <- 0 to arr1.length - 1 ){
        if(arr1.apply(i)>max){
            max = arr1.apply(i)
            index = i
        }
    }   
    return index
}
def argmax( arr1: ListBuffer[Array[Double]] ): ListBuffer[Array[Double]] = {
    var res = new ListBuffer[Array[Double]]
    var lineArr = new Array[Double](arr1.length)
    for( j <- 0 to arr1.length - 1 ){
        lineArr(j) = indiceMax(arr1.apply(j))
    }
    res += lineArr
    return res
}
def map_movie_title( mid: Int ): String = {
    var res = ""
    for( i <- movieZ ){
        if(i.getMovieId()==mid){
            res = i.getMovieTitle()
        }
    }
    return res
}
def print_movie_rank( arr: Array[Double]){
    var movie_index = new Array[Int](5)
    var rate = new Array[Double](5)
    for( i <- 0 to 4 ){  
        var rat = arr.reduceLeft(_ max _)
        val loop1 = new Breaks;
        loop1.breakable {
            for( j <- 0 to arr.length - 1 ){              
                if(arr(j)==rat){
                    movie_index(i) = j + 1 
                    rate(i) = rat
                    arr(j) = 0
                    loop1.break
                }
            }
        }
    }
    for( i <- 0 to 4 ){
        println( movie_index(i)+":"+map_movie_title(movie_index(i))+"-"+rate(i))
    }
}
def print_recommendation( W: ListBuffer[Array[Double]], Q: ListBuffer[Array[Double]], Qhat: ListBuffer[Array[Double]]){
    var Qhat1 = minus_const(minimum(Qhat),Qhat)
    var Qhat2 = star_const(5.0/maximum(Qhat1),Qhat1)
    println("Result : ")
    for( i <- users.to[Array].reduceLeft(_ min _) to users.to[Array].reduceLeft(_ max _) ){
        println("############################")
        println("[ User : "+i+" ]")
        print_movie_rank(Qhat2.apply(i-1))
        println("############################")
    }
}
print_recommendation( W, Q , Q_hat)
System.exit(0)
