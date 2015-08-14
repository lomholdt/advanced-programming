// Advanced Programming 2015
// Andrzej Wąsowski, IT University of Copenhagen

object Map {

  def map (a :List[Int]) (f :Int => Int) :List[Int] =
    a match {
      case Nil => Nil
      case h::tail => f(h)::map (tail) (f)
    }
}

object MapPoly {

  def map[A,B] (a :List[A]) (f :A => B) :List[B] =
    a match { case Nil     => Nil
              case h::tail => f(h)::map (tail) (f) }
}


object MapExampleMain extends App {

    import MyModule.abs
    import MapPoly.map
    import TailRecursive.factorial

    val mixed_list = List (-1, 2, -3, 4, -5, 6, -7, 8, -9, 10)
    val positive_list = map (mixed_list) (abs _)
    println (positive_list)
    val factorial_list = map (mixed_list) (
      ((_ :Int).toString) compose (factorial _) compose (abs _))
    println (factorial_list)

    // using a loop
    val mixed_array = Array (-1, 2, -3, 4, -5, 6, -7, 8, -9, 10)
    for (i <- 0 until mixed_array.length)
      mixed_array(i) = abs (mixed_array(i))
    // mixed_array should now be positive_array :)

    // rerunning the same example requires recreating the array
    // as we destroyed the source
    val mixed_array1 = Array (-1, 2, -3, 4, -5, 6, -7, 8, -9, 10)
    for (i <- 0 until mixed_array.length)
      mixed_array(i) = factorial (abs (mixed_array(i)))

}




