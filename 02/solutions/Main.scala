// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

println (l1.headOption)
println (l2.headOption)
println (l3.headOption)

// Exercise 1
//
def from (n: =>Int) :Stream[Int] =
  cons(n, from (n+1))

def to (n : Int) :Stream[Int] =
  if (n<0) Empty else cons(n, to (n-1))

lazy val nat :Stream[Int] = from (1)

// Ex 5.10
val fibs :Stream[BigInt] = {
  def next_fib (a :BigInt) (b :BigInt) :Stream[BigInt] =
    cons(a, next_fib (b) (a+b))
  next_fib (0) (1)
}

// testing Ex 5.11
lazy val naturals = unfold (1) (n =>  Some(n,n+1))
lazy val fibs1 :Stream[BigInt] =
  unfold (0,1) { case (m,n) => Some(m,(n,n+m)) }
