// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monoids
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  def associative[A :Arbitrary] (m: Monoid[A]) :Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1,a2), a3) == m.op(a1,m.op(a2,a3)) } :| "associativity"

  def unit[A :Arbitrary] (m :Monoid[A]) =
    forAll { (a :A) => m.op(a, m.zero) == a } :| "right unit" &&
    forAll { (a :A) => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A :Arbitrary] (m :Monoid[A]) :Prop = associative (m) && unit (m)


  // Exercise 4: test listMonoid, intAddition, intMultiplication, booleanOr,
  // booleanAnd and optionMonoid.

  property ("stringMonoid is a monoid") = monoid (stringMonoid)
  property ("listMonoid is a monoid") = monoid (listMonoid[Int])
  property ("intAddition is a monoid") = monoid (intAddition)
  property ("intMultiplication is a monoid") = monoid (intMultiplication)
  property ("booleanOr is a monoid") = monoid (booleanOr)
  property ("booleanAnd is a monoid") = monoid (booleanAnd)
  property ("optionMonoid is a monoid") = monoid (optionMonoid[Int])
  
  // Exercise 7



  def homomorphism[A :Arbitrary,B :Arbitrary]
    (ma: Monoid[A]) (f: A => B) (mb: Monoid[B]) = 
    forAll { (x: A, y: A) => mb.op(f(x), f(y)) == f(ma.op(x,y)) }

  def isomorphism[A :Arbitrary, B :Arbitrary] 
    (ma: Monoid[A]) (f: A => B, g: B => A) (mb: Monoid[B]) = 
    homomorphism(ma)(f)(mb) && homomorphism(mb)(g)(ma)

  property ("stringMonoid and listMonoid[Char] are isomorphic") = 
    isomorphism[String, List[Char]](stringMonoid)(_.toList, _.mkString)(listMonoid)

  // Exercise 8

  // property ("booleanOr and booleanAnd are isomorphic") =

  // Exercise 9 (the testing part)

  // property ("productMonoid is a monoid") =
}
