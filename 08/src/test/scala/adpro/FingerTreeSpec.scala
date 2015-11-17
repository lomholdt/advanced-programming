// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
//
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

// Places to complete are marked ...

package adpro

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import scala.collection.mutable.DoubleLinkedList
import scala.util.Random

import language.implicitConversions


class FingerTreeSpecWasowski extends FlatSpec with Checkers {

  import adpro.data._
  import adpro.data.FingerTree._

  // Generator of arbitrary trees of given size for scala check (you can use it
  // in your properties)

  def fingerTreeOfN[A] (n: Int, gen: Gen[A]) :Gen[FingerTree[A]] = {
    Gen.listOfN[A](n, gen).map(x => Digit.toTree(x))
  }
  // generate it using Gen.listOfN of Integers between 0 and 1000, and then map
  // it to finger trees using toTree.

  // Arbitrary trees of size between zero and 100
  //
  // Pick up a generated integer n btw 0 and 100 and then use it to generate a
  // tree of this size using fingerTreeOfN (can be done using flatMap or for
  // comprehensions).
  def fingerTree[A] (gen: Gen[A]) :Gen[FingerTree[A]] = {
    // Gen.choose(0,100).flatMap(x => fingerTreeOfN(x, gen));
    for{
      x <- Gen.choose(0,100)
      y <- fingerTreeOfN(x, gen)
    } yield y
  }

  // The same as above but as an instance of Arbitrary
  // Uncomment to make available once you have the fingerTree function
  //
  implicit def arbFingerTree[A] (implicit arb: Arbitrary[A]) =
    Arbitrary[FingerTree[A]](fingerTree[A] (arbitrary[A]))


  behavior of "basic FingerTree constructors"

  // Why do we distinguish Empty from Digit() without any contents?
  // You can experiment below with writing different terms (or do it in the
  // REPL)

  it should "compile" in {
    Empty ()
    Single[Int] (1)
    Digit('t')
    Digit('t','h')
    Deep (Digit('t','h'), Empty(), Digit ('r','e','e'))
    Deep[Char] (Digit('t','h'), Empty(), Digit ('r','e','e'))
    Deep(Digit(),Empty(),Digit())
    // the following is the tree from page 4, it seems to render well without
    // any type annotations
    Deep (
      Digit('t','h'),
      Deep(
        Digit(Node2 ('i','s'), Node2('i','s')),
        Empty(),
        Digit(Node3 ('n','o','t'), Node2('a','t'))
      ),
      Digit('r','e','e')
    )
  }

  behavior of "addL"

  it should "produce a queue containing the inserted element" in {
    assert(Empty().addL(42).toList == List(42))
  }

  it should "produce a queue containing the inserted elements" in check {
    forAll (Gen.listOfN(100, Gen.choose[Int](0,1000))) {
      (l :List[Int]) =>
        l.foldRight[FingerTree[Int]] (Empty()) (FingerTree.addL).toList == l
    }
  }

  behavior of "addR"

  it should "produce a queue containing the inserted element" in {
    assert(Empty().addR(42).toList == List(42))
  }

  it should "produce a queue containing the inserted elements" in check {
    forAll (Gen.listOfN(100, Gen.choose[Int](0,1000))) {
      (l :List[Int]) =>
        l.foldLeft[FingerTree[Int]] (Empty()) (FingerTree.addR).toList == l
    }
  }

  behavior of "toTree"

  it should "be an identitity on trees" in check {
    forAll (fingerTreeOfN(100, Gen.choose[Int](0,1000))) {
      (t :FingerTree[Int]) => toTree (t) == t
    }
  }

  behavior of "left views (extractors)"

  // the tests can be easily rewritten to paper-style views

  it should "be NilTree on Empty" in {
    Empty() match {
      case NilTree () => assert(Empty().empty)
      case _ => fail()
    }
  }

  it should "be ConsL(_,Nil) on Single" in {
    Single(42) match {
      case ConsL(_,NilTree()) => assert(Single(42).nonEmpty)
      case _ => fail()
    }
  }

  it should "be ConsL(_,ConsL(_,_)) on any tree larger than 3" in check {
    val ft3plus = Gen.choose(3,100) flatMap { fingerTreeOfN(_,arbitrary[Int]) }
    forAll (ft3plus) { (t: FingerTree[Int]) => t match {
      case ConsL (a, ConsL(b,_)) => true
      case _ => false
      }
    }
  }

  it should "have the right prefix on any tree larger than 3" in check {
    val list3plus = Gen.choose(3,100) flatMap { Gen.listOfN(_,arbitrary[Int]) }
    forAll (list3plus) { (l: List[Int]) =>
      val t = Digit.toTree (l)
      t.headL == l.head && t.tailL.headL == l.tail.head &&
      t.tailL.tailL.headL == l.tail.tail.head
    }
  }

  behavior of "right views"

  it should "be NilTree on Empty" in {
    Empty() match {
      case NilTree () => assert(Empty().empty)
      case _ => fail()
    }
  }

  it should "be ConsR(Nil,_) on Single" in {
    Single(42) match {
      case ConsR(NilTree(),_) => assert(Single(42).nonEmpty)
      case _ => fail()
    }
  }

  it should "be ConsR(_,ConsR(_,_)) on any tree larger than 3" in check {
    val ft3plus = Gen.choose(3,100) flatMap { fingerTreeOfN(_,arbitrary[Int]) }
    forAll (ft3plus) { (t: FingerTree[Int]) => t match {
      case ConsR (ConsR(_,b), a) => true
      case _ => false
      }
    }
  }

  it should "have the right prefix on any tree larger than 3" in check {
    val list3plus = Gen.choose(3,100) flatMap { Gen.listOfN(_,arbitrary[Int]) }
    forAll (list3plus) { (l: List[Int]) =>
      val t = Digit.toTree (l)
      t.headR == l.head && t.tailR.headR == l.tail.head &&
      t.tailR.tailR.headR == l.tail.tail.head
    }
  }






  //  Our super dupa tests



  def doStuff [A] (l: DoubleLinkedList[A], content: Gen[A], c: Int, n: Int): DoubleLinkedList[A] = 
    if (c < n) {
      // println(c)
      // println(l.toList)
      val rand = Gen.choose(0,3)
      rand.sample.get match {
        case 0 => doStuff(content.sample.get +: l, content, c+1, n) // addL
        case 1 => doStuff(l :+ content.sample.get, content, c+1, n) // addR
        case 2 => doStuff(l.tail, content, c+1, n) // popL
        case 3 => doStuff(l.init, content, c+1, n) // popR
      }
    }
    else l

  def doStuff [A] (t: FingerTree[A], content: Gen[A], c: Int, n: Int): FingerTree[A] = 
    if (c < n) {
      // println(c)
      // println(t.toList)
      val rand = Gen.choose(0,3)
      rand.sample.get match {
        case 0 => doStuff(t.addL(content.sample.get), content, c+1, n) // addL
        case 1 => doStuff(t.addR(content.sample.get), content, c+1, n) // addR
        case 2 => doStuff(t.tailL, content, c+1, n) // popL
        case 3 => doStuff(t.tailR, content, c+1, n) // popR
      }
    }
    else t


  def getTime (f: => Unit): Long = {
    val start = System.nanoTime()
    f
    System.nanoTime() - start
  }

  def testListSize(g: Gen[Int]) = {
    // val linkedA = DoubleLinkedList.range(0,10)
    // val linkedB = DoubleLinkedList.range(0,100)
    // val linkedC = DoubleLinkedList.range(0,200)
    val linkedD = DoubleLinkedList.range(0,500)
    // val treeA: FingerTree[Int] = fingerTreeOfN(10, g).sample.get
    // val treeB: FingerTree[Int] = fingerTreeOfN(100, g).sample.get
    // val treeC: FingerTree[Int] = fingerTreeOfN(200, g).sample.get
    // val treeD: FingerTree[Int] = fingerTreeOfN(500, g).sample.get

    // println(getTime(doStuff(linkedA, g, 0, 10)))
    // println(getTime(doStuff(treeA, g, 0, 10)))
    // println(getTime(doStuff(linkedB, g, 0, 100)))
    // println(getTime(doStuff(treeB, g, 0, 100)))
    // println(getTime(doStuff(linkedC, g, 0, 200)))
    // println(getTime(doStuff(treeC, g, 0, 200)))
    println(getTime(doStuff(linkedD, g, 0, 500)))
    // println(getTime(doStuff(treeD, g, 0, 500)))
  }

  testListSize(Gen.choose(0,1000))

 // val g: Gen[String] = Random.nextString(100)

 def testListContent(g: Gen[String]) = {
    // val linkedA: DoubleLinkedList[String] = DoubleLinkedList.fill(10)(g.sample.get)
    val linkedB: DoubleLinkedList[String] = DoubleLinkedList.fill(100)(g.sample.get)
    val linkedC: DoubleLinkedList[String] = DoubleLinkedList.fill(200)(g.sample.get)
    val linkedD: DoubleLinkedList[String] = DoubleLinkedList.fill(500)(g.sample.get)

    // val treeA: FingerTree[String] = fingerTreeOfN(10, g).sample.get
    val treeB: FingerTree[String] = fingerTreeOfN(100, g).sample.get
    val treeC: FingerTree[String] = fingerTreeOfN(200, g).sample.get
    val treeD: FingerTree[String] = fingerTreeOfN(500, g).sample.get

    // println(getTime(doStuff(linkedA, g, 0, 10)))
    // println(getTime(doStuff(treeA, g, 0, 10)))
    println(getTime(doStuff(linkedB, g, 0, 100)))
    println(getTime(doStuff(treeB, g, 0, 100)))
    println(getTime(doStuff(linkedC, g, 0, 200)))
    println(getTime(doStuff(treeC, g, 0, 200)))
    println(getTime(doStuff(linkedD, g, 0, 500)))
    println(getTime(doStuff(treeD, g, 0, 500)))

    // println(getTime(doStuff(linkedA, g, 0, 10)))
    // println(getTime(doStuff(treeA, g, 0, 10)))
    println(getTime(doStuff(linkedB, g, 0, 100)))
    println(getTime(doStuff(treeB, g, 0, 100)))
    println(getTime(doStuff(linkedC, g, 0, 200)))
    println(getTime(doStuff(treeC, g, 0, 200)))
    println(getTime(doStuff(linkedD, g, 0, 500)))
    println(getTime(doStuff(treeD, g, 0, 500)))

    // println(getTime(doStuff(linkedA, g, 0, 10)))
    // println(getTime(doStuff(treeA, g, 0, 10)))
    println(getTime(doStuff(linkedB, g, 0, 100)))
    println(getTime(doStuff(treeB, g, 0, 100)))
    println(getTime(doStuff(linkedC, g, 0, 200)))
    println(getTime(doStuff(treeC, g, 0, 200)))
    println(getTime(doStuff(linkedD, g, 0, 500)))
    println(getTime(doStuff(treeD, g, 0, 500)))
 }

 // testListContent(Random.nextString(100))



}
