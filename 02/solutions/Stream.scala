// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {

  def headOption () :Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail :Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f :(A, =>B) => B) :B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  def find (p :A => Boolean) :Option[A] = this.filter (p).headOption


  /* Ex 5.1 */
  def toList () :List[A] = this match {
    case Empty => Nil
    case Cons(h,t) => h() :: t().toList
  }

  /* Ex 5.2 */
  def take (n :Int) :Stream[A] = this match {
      case Empty => Empty
      case Cons (h, t) => if (n==0) Empty else Cons (h, () => t().take (n-1))
  }

  def drop (n : Int) :Stream[A] = this match {
      case Empty => Empty
      case Cons (_,t) => if (n==0) this else t().drop (n-1)
  }

  // Ex 5.3
  def takeWhile (p :A =>Boolean) :Stream[A] = this match {
      case Empty => Empty
      case Cons (h,t) => if (p (h())) Cons(h, () => t() takeWhile p) else Empty
  }

  // Ex 5.4
  def forAll (p :A => Boolean) :Boolean = this match {
      case Empty => true


      case Cons (h,t) => p(h()) && t().forAll (p)
  }

  // Ex 5.5
  def takeWhile55 (p :A => Boolean) :Stream[A] =
    foldRight[Stream[A]] (Empty) ((a,z) => if (p(a)) cons (a,z) else z)

  // Ex 5.6
  def headOption56 :Option[A] = foldRight[Option[A]] (None) ((a,_) => Some(a))

  // Ex 5.7 map
  def map[B] (f :A => B) :Stream[B] =
    foldRight[Stream[B]] (Empty) ((a,z) => cons(f(a),z))

  // filter
  def filter[B] (p :A => Boolean) :Stream[A] =
    foldRight[Stream[A]] (Empty) ((a,z) => if (p(a)) cons(a,z) else z)

  // append
  //
  // Note 1: A is co-variant so the variance annotation actually works, because
  // if we want to take a subclass C of A and append it, then a Stream[C] is
  // also a Stream[A] and we just use the append[A] method. So we can append
  // both streams of super types and of subtypes and always get the weaker type
  // as a result.  The function is as usable as it gets
  def append[B>:A] (that : => Stream[B]) :Stream[B] = foldRight (that) (cons (_,_))

  // this is an attempt to explain why a non-solution does not work
  //
  // Note 2: A is a contravariant position as an argument of c so this code is
  // illegal. Now since A is covariant, I could promote a stream[A] to
  // Stream[Anyref] and then append a Stream[B] of some values unrelated to A,
  // only related to AnyRef; this could break the implementation of append, or
  // specifically the implementation of c, since it expects something smaller
  // than A.
  //
  //def append1[B<:A] (that :Stream[B]) :Stream[A] = {
  //  def c (a : A, s : => Stream[A]) = cons (a,s)
  //  foldRight (that :Stream[A]) (c _)
  //}

  // Ex 5.7 flatMap
  def flatMap[B] (f :A => Stream[B]) :Stream[B] =
    foldRight[Stream[B]] (Empty) ((a,z) => f(a).append(z))

  // Ex 5.13
  def map513[B] (f :A =>B) :Stream[B] = unfold (this) {
    case Empty => None
    case Cons(h,t) => Some(f(h()), t())
  }

  // Ex 5.13
  def take513 (n :Int) :Stream[A] = unfold (this,n) {
    case (Empty,    _) => None
    case (Cons(_,_),0) => None
    case (Cons(h,t),n) => Some(h(),(t(),n-1))
  }

  // Ex 5.13
  def takeWhile513 (p :A =>Boolean) :Stream[A] = unfold (this) {
    case (Empty)     => None
    case (Cons(h,t)) => if (p(h())) Some(h(), t()) else None
  }

  def zipWith[B,C] (ope : (=>A,=>B) => C) (bs :Stream[B]) :Stream[C] =
    unfold (this,bs) {
      case (Empty,_) => None
      case (_,Empty) => None
      case (Cons(h1,t1), Cons(h2,t2)) => Some (ope(h1(),h2()), (t1(),t2()))
    }

  def zipAll[B] (that: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold (this,that) {
      case (Empty,Empty) => None
      case (Cons(h,t),Empty) => Some((Some(h()),None) -> (t(),Empty))
      case (Empty,Cons(h,t)) => Some((None,Some(h())) -> (Empty,t()))
      case (Cons(h1,t1), Cons(h2,t2)) => Some((Some(h1()),Some(h2())) -> (t1(),t2()))
    }

  // Ex 5.14
  def startsWith[A] (that: Stream[A]): Boolean =
    this.zipAll (that).takeWhile { case (_,None) => false; case _ => true }.
      forAll  (ab => ab._1==ab._2)

  // a slightly different solution that emphasizes that Options are very
  // similar to lists:
  def startsWith1[A] (that: Stream[A]): Boolean =
    this.zipAll (that).takeWhile (!_._2.isEmpty).forAll (ab => ab._1==ab._2)

  // Ex 5.15 9.13
  def tails: Stream[Stream[A]] = this match {
    case Empty => Empty
    case Cons(h,t) => cons(this,t().tails)
  }
}



case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq

  // Ex 5.11
  def unfold[A, S] (z: S) (f: S => Option[(A, S)]) :Stream[A] =
    f (z).map {case (a,z1) => cons (a, unfold (z1) (f))}.getOrElse (Empty)

}

// vim:tw=0:cc=80:nowrap
