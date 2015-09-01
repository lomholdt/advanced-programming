// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object Exercises {

  // Exercise 3

  def exp (x :Double) (n :Int) :Double =

    n match {
      case n if n>0 && n%2==0 => { val e = exp (x) (n/2); e*e }
      case n if n>0           => x * exp (x) (n-1)
      case 0                  => 1
      case _                  => 1.0 / (exp (x) (-n))
    }

    /* All recursive calls are NOT in tail position.  This function is
     * generally efficient and it uses a stack which is O(log n), so linear
     * space (since n is represented in O(log n) space.  To stress the stack we
     * need a large n, which is a power of 2, and give the function n-1 as the
     * input parameter (then it will recurse roughly n/2 times, before reaching
     * the logarithmic behavior).
     */



  // Exercise 4

  def map[A,B] (a :List[A]) (f :A => B) :List[B] = {

    def go (a :List[A]) (r :List[B]) :List[B] =

      a match {
        case Nil => r.reverse
        case head::tail => go (tail) (f (head)::r)
      }

    go (a) (Nil)
  }

  // Exercise 6

  def filter[A] (l :List[A]) (p :A => Boolean) :List[A] =
    l.flatMap (a => if (p (a)) List(a) else Nil)


  // Exercise 7

  def add (l :List[Int]) (r :List[Int]) :List[Int] =
    (l,r) match {
      case (hl::tl,hr::tr) => (hl+hr)::add (tl) (tr)
      case (Nil,_) => Nil
      case (_,Nil) => Nil
    }

  // Exercise 8

  def zipWith[A,B,C] (f : (A,B)=>C) (l :List[A],r :List[B]) :List[C] =
    (l,r) match {
      case (hl::tl,hr::tr) => f(hl,hr)::zipWith (f) (tl,tr)
      case (Nil,_) => Nil
      case (_,Nil) => Nil
    }

  // Exercise 9

  def hasSubsequence[A] (sup: List[A], sub :List[A]) :Boolean = {
    (sup,sub) match {
      case (_,Nil) => true
      case (Nil,_) => false
      case (x::xs, y::ys) => if (x==y) hasSubsequence (xs,ys)
                             else hasSubsequence (xs,sub)
    }
  }

  // Exercise 10

  def pascal (n :Int) :List[Int] = {

    def nextRow (row :List[Int]) :List[Int] =
      row match {
        case x::y::xs => x+y::nextRow (y::xs)
        case List(_) => List(1)
      }

    def go (levels :Int) (row :List[Int]) :List[Int] =
      if (levels==0) row
      else go (levels-1) (1::nextRow (row))

    go (n-1) (List(1))
  }


}


// Exercise 11

/* I created OrderedPoint as a trait instead of a class, so I can mix it into
 * Points (this allows me to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, I would have to
 * reimplement them in my subclass.  This is not a problem if I mix in a trait
 * construction time. */

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this :java.awt.Point =>

  override def compare (that :java.awt.Point) :Int =
    if (this.getX < that.getX) -1
    else if (this.getX == that.getX && this.getY < that.getY) -1
    else if (this.getX == that.getX && this.getY == that.getY) 0
    else 1

}

/* A test session for Exercise 11

scala> val p = new java.awt.Point(0,1) with OrderedPoint
scala> val q = new java.awt.Point(0,2) with OrderedPoint
scala> p < q
res5: Boolean = true

Notice how we are using nice infix comparison on java.awt
objects that were implemented way before Scala existed :)

*/

