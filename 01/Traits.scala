// Advanced Programming
// Andrzej Wąsowski, 2015, IT University of Copenhagen

// A class with a final property 'name' and
// a constructor. You can still add
// more members like in Java in braces.
abstract class Animal (val name :String)

// concrete methods
trait HasLegs {
  def run  () :String = "after you!"
  def jump () :String = "hop!"
}

// abstract method
trait Audible { def makeNoise () :String }

// field
trait Registered { var id :Int = 0 }

// multiple traits mixed in
class Frog (name:String) extends
  Animal(name) with HasLegs with Audible {
  def makeNoise () :String = "croak!"
} // Frog concrete, so provide makeNoise




object TraitsMain extends App {

  // Mix directly into an object
  val f = new Frog ("Kaj") with
            Registered
  // f: Frog with Registered =
  //            $anon$1@88f0bea

  f.id = 42
  println (
    s"My name is ${f.name}")
  println (
    "I'm running " + f.run )
  println(
    "I'm saying " + f.makeNoise)

}
