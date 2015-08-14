// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

class Person (val name: String, val age: Int) {

  // this line will execute in the primary constructor
  println ("Just constructed a person")

  def description = s"$name is $age years old"
}
