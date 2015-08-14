// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object Fold {

  def foldLeft[A,B] (f : (A,B) => B) (z :B) (l :List[A]) :B =
    l match {
      case Nil => z
      case x::xs => foldLeft[A,B] (f) (f(x,z)) (xs)
    }

   def foldRight[A,B] (f : (A,B) => B) (z :B) (l :List[A]) :B =
     l match {
       case x::xs => f(x, foldRight (f) (z) (xs))
       case Nil => z
     }
}

object FoldMain extends App {

  import Fold._ // use our folds instead of standard library

  // a test case
  val l1 = List (1,2,3,4,5,6)
  // compute a sum of list l1
  val sum = foldLeft[Int,Int] (_+_) (0) (l1)
  // compute the inner product of vector stored in list l1
  val product = foldLeft[Int,Int] (_*_) (1) (l1)

  // define the map HOF using foldRight
  def map[A,B] (f :A=>B) =
    foldRight[A,List[B]] ((x, z) => f(x)::z) (Nil) _

  // test the map, by incrementing l1
  val l2 = map[Int,Int] (_+1) (l1)

  // show some output if you are not running this in REPL
  println (sum)
  println (product)
  println (l2)
}
