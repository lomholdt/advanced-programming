package dk.itu.adpro
// Advanced Programming, 2015
// Andrzej Wąsowski, IT University of Copenhagen

// An ADT of Lists
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Additional functions on lists
object List {

  def sum(ints :List[Int]) :Int =
    ints match { case Nil => 0
                 case Cons(x,xs) => x + sum(xs) }

  // override function application
  // to provide a factory of lists
  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}


// Main for demonstration
object ListMain extends App {

  // use the factory method
  val l = List(1,-2,3,-4,5,-6,7)

  // compute the sum
  import List.sum
  println (sum (l))
}
