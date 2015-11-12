// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// comment out all the lines below to test Scala Standard Library implementation
// of Streams. Interestingly the standard library streams are stricter than
// those from the book, so some laziness tests fail on them :)
//
 import stream00._ // uncomment to test the book laziness solution implementation
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption


// From 05 folder compile
// scalac -cp src/main/scala/:src/test/scala/ src/test/scala/fpinscala/laziness/StreamSpecWasowski.scala
// Then run scala org.scalatest.run

class StreamSpecWasowski extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  // a scenario test:
  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)


  // a property test:
  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )
  }

  it should "not force the tail of the stream. (03)" in {
    cons(1, cons(2, cons(2, cons(4, cons(5, cons(throw new RuntimeException, empty)))))).headOption
  }


  behavior of "take"

  it should "not force any heads nor any tails of the Stream it manipulates (04)" in {
    cons(throw new RuntimeException, cons(throw new RuntimeException, cons(throw new RuntimeException, throw new RuntimeException))).take(0)
  }

  it should "not force (n+1)st head ever (even if we force all elements of take(n)) (05)" in {
    cons(1, cons(2, cons(throw new RuntimeException, cons(3, throw new RuntimeException)))).take(1)
  }

  it should "s.take(n).take(n) == s.take(n) for any Stream s and any n (06)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int]);
    ("s.take(n).take(n) == s.take(n)" |: Prop.forAll(Gen.choose(0,1000),genNonEmptyStream[Int]) {
      (n :Int, s: Stream[Int]) => s.take(n).take(n).toList == s.take(n).toList
      })
  }



  behavior of "drop"

  it should "s.drop(n).drop(m) == s.drop(n+m) for any n, m (additivity) (07)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("drop(n).drop(m)" |: Prop.forAll(Gen.choose(0, 1000),Gen.choose(0,1000),genNonEmptyStream[Int]) {
      (n :Int, m :Int, s: Stream[Int]) => s.drop(n).drop(m).toList == s.drop(n+m).toList
      })
  }

  it should "s.drop(n) does not force any of the dropped elements heads (08)" in {
    val s = cons(throw new RuntimeException, cons(throw new RuntimeException, 
      cons(throw new RuntimeException, throw new RuntimeException)))
    s drop 0
  }

  it should "the above should hold even if we force some stuff in the tail (08)" in {
    // TODO implement
  }

  behavior of "map"

  it should "x.map(id) == x (where id is the identity function) (09)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("x.map(id) == x" |: Prop.forAll(genNonEmptyStream[Int]) {
      (x: Stream[Int]) => x.map(n => n).toList == x.toList
      })
  }

  it should "map terminates on infinite streams (10)" in {
    val s = from(0)
    s.map(n => n)
  }


  behavior of "append"

  // it should "append two stream together with a.length+b.length == c.length (11)" in {
  // //   val a = cons(1, cons(2, cons(3, empty)))
  // //   val b = cons(4, cons(5, cons(6, empty)))
  // //   val c = a.append(b)
  // //   assert(a.length + b.length == c.length)
  // }

  it should "append streams bla bla (12)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("x.append(empty) == x" |: Prop.forAll(genNonEmptyStream[Int]) {
      (x: Stream[Int]) => x.append(empty).toList == x.toList
      })
  }


}
