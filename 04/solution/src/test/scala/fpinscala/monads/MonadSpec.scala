// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monads
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.language.higherKinds
import Monad._


object  MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
  def associative[A,F[_]] (m: Monad[F]) (implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A,A] (m.flatMap[A,A] (x) (f)) (g) ==
      m.flatMap (x) (a => m.flatMap (f(a)) (g))
    }

  def identity[A, F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]): Prop =
      forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A,A] (x) (m.unit[A] (_)) == x } :| "right unit" &&
    forAll { (y :A, f: A => F[A]) =>
      m.flatMap[A,A] (m.unit[A](y)) (f) == f(y) } :| "left unit"

  def monad[A,F[_]] (m :Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]) :Prop =
    associative[A,F] (m) && identity[A,F] (m)

  property ("of optionMonad") = monad[Int,Option] (optionMonad)

  /// Exercise 17

  property ("of listMonad") = monad[Int,List] (listMonad)
  property ("of streamMonad[Int]") = monad[Int,Stream] (streamMonad)
  property ("of streamMonad[String]") = monad[String,Stream] (streamMonad)

  // Exercise 19

  // this formulation is more proper than the one in the example above
  // (I should not have restricted the one above to F over the same type A)
  def kleisliAssociative[A,B,C,D,F[_]] (m: Monad[F]) (implicit
    arbA: Arbitrary[A],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbFD: Arbitrary[F[D]]
  ) :Prop =
      forAll { (x: A, f: A => F[B], g: B => F[C], h: C => F[D]) =>
        m.compose(m.compose(f,g),h) (x) ==
          m.compose(f, m.compose(g,h)) (x)
      }

  def kleisliIdentity[A,B,F[_]] (m: Monad[F]) (implicit arbA :Arbitrary[A],
    arbFB :Arbitrary[F[B]]) :Prop =
    forAll { (x: A, f: A => F[B]) =>
       m.compose[A,B,B] (f, m.unit[B] (_)) (x) == f (x) } :| "right unit" &&
    forAll { (x: A, f: A => F[B]) =>
       m.compose[A,A,B] (m.unit[A] (_), f) (x) == f (x) } :| "left unit"

  def kleisliMonad[A,B,C,D,F[_]] (m :Monad[F]) (implicit
    arbA: Arbitrary[A],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbFD: Arbitrary[F[D]]
  ) :Prop =
    kleisliAssociative[A,B,C,D,F] (m) && kleisliIdentity[A,B,F] (m)

  property ("of optionMonad [Kleisli]") =
    kleisliMonad[Int,Int,Int,Int,Option] (optionMonad)
  property ("of listMonad [Kleisli]") =
    kleisliMonad[Int,Int,Int,Int,List] (listMonad)
  property ("of streamMonad[Int] [Kleisli]") =
    kleisliMonad[Int,Int,Int,Int,Stream] (streamMonad)
  property ("of streamMonad[String] [Kleisli]") =
    kleisliMonad[String,String,String,String,Stream] (streamMonad)

  // let's add one crazy
  property ("of streamMonad [Kleisli over String,Int,Double and Boolean]") =
    kleisliMonad[String,Int,Double,Boolean,Stream] (streamMonad)
}
