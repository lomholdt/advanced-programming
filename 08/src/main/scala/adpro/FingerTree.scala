package adpro
import scala.language.higherKinds

// The implementation is based on Section 3 of the paper.
//
// This implementation is designed to be eager, following the regular strictness
// of Scala.  However it would be an interesting exercise to extend it so that
// it is possibly lazy, like in the paper of Hinze and Paterson.  The obvious
// choice is to make values of elements stored in the queue lazy.  Then there is
// also a discussion of possible suspension of the middle element of the tree on
// page 7.

// QUESTION I: Complete the implementation of Finger Trees below.  Incomplete
// places are marked ...
//
// I am Simulating a package with an object, because type declarations
// can only be placed in objects (so this allows me to place Digit on top).

object data {

  // The interface spec for reducible structures, plus two useful derived
  // reducers that the paper introduces (toList and toTree)

  // I changed the type of reducers to not use curried operators, but regular
  // binary operators.  This is more natural in Scala, and gives easier to read
  // syntax of expressions.  Curried style is preferred in Haskell.

  trait Reduce[F[_]] {
    def reduceR[A,B] (opr: (A,B) => B) (fa: F[A], b: B) :B
    def reduceL[A,B] (opl: (B,A) => B) (b: B, fa: F[A]) :B

    // page 3

    def toList[A] (fa: F[A]) :List[A] = {
      reduceR[A, List[A]](_::_)(fa, Nil)
    }

    // page 6
    
    def toTree[A] (fa: F[A]) :FingerTree[A] = {
      reduceR[A, FingerTree[A]](FingerTree.addL)(fa, Empty())
    }
  }

  // Types for Finger trees after Hinze and Pattersoni (page 4)

  type Digit[A] = List[A]

  sealed trait Node[+A] {

    // uncomment the delagation once Node.toList is implemented
    //
    def toList :List[A] = Node.toList (this)
  }

  case class Node2[A] (l :A, r :A) extends Node[A]
  case class Node3[A] (l: A, m: A, r: A) extends Node[A]

  sealed trait FingerTree[+A] {

    // The following methods are convenience delagation so we can use
    // the operations both as methods and functions.
    // Uncomment them once you have implemented the corresponding functions.

    def addL[B >:A] (b: B) :FingerTree[B] = FingerTree.addL (b,this)
    def addR[B >:A] (b: B) :FingerTree[B] = FingerTree.addR (this,b)
    def toList :List[A] = FingerTree.toList (this)

    def headL :A = FingerTree.headL (this)
    def tailL :FingerTree[A] = FingerTree.tailL (this)
    def headR :A = FingerTree.headR (this)
    def tailR :FingerTree[A] = FingerTree.tailR (this)

    // page 7 (but this version uses polymorphis for efficiency, so we can
    // implement it differently; If you want to follow the paper closely move them to
    // FingerTree object and delegate the methods, so my tests still work.
    //
    def empty :Boolean = this match {
      case NilTree() => true
      case ConsL(_,_) => false
    }

    def nonEmpty :Boolean = !empty
  }
  case class Empty () extends FingerTree[Nothing] {

    // page 7
    //
    // override def empty = ...
    // override def nonEmpty = ...
  }
  case class Single[A] (data: A) extends FingerTree[A]
  // paramter names: pr - prefix, m - middle, sf - suffix
  case class Deep[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]

  // page 6
  //
  // Types of views on trees
  // The types are provided for educational purposes.  I do not use the view
  // types in my implementation. I implement views as Scala extractors.
  // But you may want to implement views first like in the paper, and then
  // conver them to Scala extractors.

  // In the paper views are generic in the type of tree used. Here I make them
  // fixed for FingerTrees.

   // sealed trait ViewL[+A]
   // case class NilTree () extends ViewL[Nothing]
   // case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]

  // Left extractors for Finger Trees (we use the same algorithm as viewL in the
  // paper). You can do this, once you implemented the views the book way.
  // Once the extractors are implemented you can pattern match on NilTree, ConsL
  // and ConsR
  //
  // See an example extractor implemented for Digit below (Digit.unapply)

  object NilTree { // we use the same extractor for both left and right views
    def unapply[A] (t: FingerTree[A]) :Boolean = t match {
      case Empty() => true
      case _ => false
    }
  }

  object ConsL {
    def unapply[A] (t: FingerTree[A]) :Option[(A,FingerTree[A])] = t match {
      case NilTree() => None
      case Single(x) => Some(x, Empty())
      case Deep(pr, m, sf) => Some(pr.head, FingerTree.deepL(pr.tail, m, sf))
    }
  }

  object ConsR {
    def unapply[A] (t: FingerTree[A]) :Option[(FingerTree[A],A)] = t match {
      case NilTree() => None
      case Single(x) => Some(Empty(), x)
      case Deep(sf, m, pr) => Some(FingerTree.deepR(sf.tail, m, pr), sf.head)
    }
  }

  // several convenience operations for Digits.
  //
  object Digit extends Reduce[Digit] { // uncomment once the interfaces are provided

    // page 3, top
    //
    def reduceR[A,Z] (opr: (A,Z) => Z) (d: Digit[A], z: Z) :Z = {
      d.foldRight(z)(opr)
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, d: Digit[A]) :Z = {
      d.foldLeft(z)(opl)
    }

    // Digit inherits toTree from Reduce[Digit] that we will also apply to other
    // lists, but this object is a convenient place to put it (even if not all
    // lists are digits)

    // This is a factory method that allows us to use Digit (...) like a
    // constructor
    def apply[A] (as: A*) : Digit[A] = List(as:_*)

    // This is an example of extractor, so that we can use Digit(...) in pattern
    // matching.  Case classes have extractors automatically, but Digit defined
    // as above is not a case class, but just a type name.
    def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] = Some (d)
  }


  object Node extends Reduce[Node] {

    // page 5, top
    def reduceR[A,Z] (opr: (A,Z) => Z) (n :Node[A], z: Z) :Z = n match {
      case Node2(l, r) => opr(l, opr(r, z))
      case Node3(l, m , r) => opr(l, opr(m, opr(r, z)))
    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, n :Node[A]) :Z = n match {
      case Node2(l, r) => opl(opl(z, l), r)
      case Node3(l, m, r) => opl(opl(opl(z, l), m),r)
    }
  }



  // Most of the paper's key functions are in the module below.

  object FingerTree extends Reduce[FingerTree] { // uncomment once the interface is implemented

    // page 5
    def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z) :Z = t match {
      case Empty() => z
      case Single(x) => opr(x,z)
      case Deep(pr, m, sf) => Digit.reduceR(opr)(pr, FingerTree.reduceR(Node.reduceR(opr) _)(m, Digit.reduceR(opr)(sf, z)))

    }

    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]) :Z = t match {
      case Empty() => z
      case Single(x) => opl(z,x)
      case Deep(pr, m, sf) => Digit.reduceL(opl)((FingerTree.reduceL(Node.reduceL(opl) _)(Digit.reduceL(opl)(z,pr),m)),sf)

    }

    // page 5 bottom (the left triangle); Actually we could use the left
    // triangle in Scala but I am somewhat old fashioned ...

    def addL[A] (a: A, t: FingerTree[A]): FingerTree[A] = t match {
      case Empty() => Single(a)
      case Single(b) => Deep(Digit(a), Empty(), Digit(b))
      case Deep(Digit(b,c,d,e), m, sf) => Deep(Digit(a,b), addL(Node3(c,d,e),m), sf) 
      case Deep(pr, m, sf) => Deep(a::pr, m, sf)
    }

    def addR[A] (t: FingerTree[A], a: A): FingerTree[A] = t match {
      case Empty() => Single(a)
      case Single(b) => Deep(Digit(b), Empty(), Digit(a))
      case Deep(pr, m, Digit(e,d,c,b)) => Deep(pr, addR(m, Node3(e,d,c)), Digit(b,a))
      case Deep(pr, m, sf) => Deep(pr, m, sf++Digit(a))
    }

    // page 6
    //
    // This is a direct translation of view to Scala. You can replace it later
    // with extractors in Scala, see above objects NilTree and ConsL (this is an
    // alternative formulation which is more idiomatic Scala, and slightly
    // better integrated into the language than the Haskell version).
    // In Haskell we need to call viewL(t) to pattern match on views.  In Scala,
    // with extractors in place, we can directly pattern match on t.
    //
    // def viewL[A] (t: FingerTree[A]) :ViewL[A] = t match {
    //   case Empty() => NilTree
    //   case Single(x) => ConsL(x, Empty())
    //   case Deep(pr, m, sf) => ConsL(pr.head, deepL(pr.tail, m, sf))

    // }

    // page 6
    //
    // A smart constructor that allows pr to be empty
    def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) :FingerTree[A] = pr match {
      case List() => m match {
        case NilTree() => Digit.toTree(sf)
        case ConsL(a, m) => Deep(a.toList, m, sf)
        }
      case _ => Deep(pr, m, sf)

    }

    def deepR[A] (sf: Digit[A], m: FingerTree[Node[A]], pr: Digit[A]) :FingerTree[A] = sf match{
      case List() => m match {
        case NilTree() => Digit.toTree(pr)
        case ConsR(m, a) => Deep(a.toList, m, pr)
      }
      case _ => Deep(sf, m, pr)
    }
    // page 7

    def headL[A] (t: FingerTree[A]): A = t match {
      case ConsL(a,_) => a
    }
    def tailL[A] (t: FingerTree[A]): FingerTree[A] = t match {
      case ConsL(_,x) => x
    }

    def headR[A] (t: FingerTree[A]): A = t match {
      case ConsR(_,a) => a
    }
    def tailR[A] (t: FingerTree[A]): FingerTree[A] = t match {
      case ConsR(x,_) => x
    }
  }
}

/* QUESTION II
 *
 * Compare efficiency of your FingerTrees and DoubleLinkedList (imperative)
 * implementation in the standard library.  Try storing integers and lists of
 * various sizes (short, middle, long).  You can use scalacheck to generate long
 * lists of random values (or write generators manually).  Then try also to
 * measure performance of 3 interesting lists length (established using
 * integers) and 3 different sizes of random string data (short, middle, and
 * very long, say in MB).
 *
 * One interesting idea is to generate a random list of operations: addR (n),
 * addL(n), popL, popR, and compare performance of the two structures on many
 * random lists. Measuring just insertions, is not stressing the FingerTrees
 * enough.  The trees are also restructured on popping. (NB. popL is the same as
 * tailL, and popR is the same as tailR)
 *
 * Recall that due to just-in-time compilation, it is useful to run a
 * computation several times before you start measuring.  Then run it several
 * times and take average (or sum).  All these run have to happen in the same
 * virtual machine session.
 *
 * Summarize the results below in a simple table listing times for 4 selected
 * lengths of integer lists (for DoubleLinkedList and for Finger Trees). Before
 * the table write approximately 100 words explaining what you measured.
 *
 * ==========================================================================
 * We've measured the time it takes to do the operations addL, addR, popL and
 * popR 5 times in random order on different size FingerTrees and 
 * DoubleLinkedList. We measure the time in nano seconds with start right before
 * we call the test function and stop right after the function is terminated.
 * The results shows that DoubleLinkedList becomes remarkably slower as
 * the size increases compared to FingerTree, which has a significantly lower 
 * order of growth.
 *
 * Time is in us
 *
 * Size | FingerTree time | DoubleLinkedList
 * -----------------------------------------
 *   10 |           51915 |           56278
 * -----------------------------------------
 *  100 |         2044325 |        66554384
 * -----------------------------------------
 *  200 |         1836266 |       617108318
 * -----------------------------------------
 *  500 |         6829843 |     27237805879
 * -----------------------------------------
 *
 * Then report the string numbers in the table like the above
 *
 * Short:            10 chars
 * Medium:       10.000 chars
 * Very Long: 1.000.000 chars
 *
 * FingerTree:
 * Size of List \ Size of String | short   | medium  | very long
 * ----------------------------------------------------------
 * enter list size           100 |  806488 |  721805 |  247305
 * ----------------------------------------------------------
 * enter list size           200 | 5279387 | 1259447 | 1955842
 * ----------------------------------------------------------
 * enter list size           500 | 1453064 | 1234743 |  959729
 * ----------------------------------------------------------
 *
 * DoubleLinkedList:
 * Size of List \ Size of String | short       | medium      | very long
 * ----------------------------------------------------------
 * enter list size           100 |    53116252 |    43295251 |    28343400
 * ----------------------------------------------------------
 * enter list size           200 |   446079333 |   455404970 |   419902950
 * ----------------------------------------------------------
 * enter list size           500 | 16297745133 | 18126477930 | 20715856275
 * ----------------------------------------------------------
 *
 * Repeat the table for FingerTrees and for doubly linked list.  Remember to
 * include units of time, and of size in all tables.  Replace list sizes, and
 * short/medium/very-long above with concrete numbers.
 *
 * Finally summarize your conclusion in approximately 100 words.
 * 
 * We ran the test 5 times and took the average of that.
 * We get inconsistent results on almost every run. Particularly 
 * on the 100 and 200 sizes, where the times differ radically.
 * The tendency is that performing operations on FingerTrees of size 100 
 * is slower than lists of size 200, and also lists of size 500 is faster
 * than lists of size 200. 
 * It seems that the DoubleLinkedList does not scale well with increased 
 * input size, as where the FingerTree doesn't seem to care much about it.
 * Here it is easy to see, why DoubleLinkedList has been deprecated.
 *    
 */

