package com.rockthejvm.playground

object ContextualAbstractions {
  
  // given/using combo
  
  trait Combiner[T] {
    def combine(x: T, y: T): T
    def empty: T
  }

  given intCombiner: Combiner[Int] with {
    def combine(x: Int, y: Int) = x + y
    def empty = 0
  }
  def combineAll[T](values: List[T])(using combiner: Combiner[T]): T =
    values.foldLeft(combiner.empty){ (acc, b) => combiner.combine(acc, b) }


  given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
    override def empty = Some(combiner.empty)
    override def combine(x: Option[T], y: Option[T]) = for {
      vx <- x
      vy <- y
    } yield combiner.combine(vx, vy)
  }

  // extension
  case class Person(name: String) {
    def greet(): String = s"Hello, my name is $name"
  }
  extension (name: String)
    def greet(): String = Person(name).greet()

  val aliceGreeting = "Alice".greet()

  // generic extension
  extension [T](list: List[T])
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty){ (acc, b) => combiner.combine(acc, b) }

  val sumOfInts = List(1,2,3,4).reduceAll
}
// typeclasses
object Scala3Typeclasses {

  case class Person(name: String, age: Int)

  // defn.
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // instances
  given stringSerializer: JSONSerializer[String] with {
    override def toJson(value: String) = s""""$value""""
  }

  given intSerializer: JSONSerializer[Int] with {
    override def toJson(value: Int) = s"$value"
  }

  given personSerializer: JSONSerializer[Person] with {
    override def toJson(value: Person) =
      s"""{"name": "${value.name}", "age": ${value.age}}"""
  }

  given listSerializer[T](using serializer: JSONSerializer[T]): JSONSerializer[List[T]] with {
    override def toJson(list: List[T]): String = list.map(serializer.toJson).mkString("[", ", ", "]")
  }
  
  // user facing API
  def convert2Json[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def convertList2Json[T](list: List[T])(using serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ", ", "]")

  // extension methods just for the types we support
  extension [T](value: T)
    def toJson(using serializer: JSONSerializer[T]): String = serializer.toJson(value)

  def main(args: Array[String]): Unit = {
    val people = List(
        Person("Alice", 10),
        Person("Bob", 12)
      )
    println(convertList2Json(people))

    println(people.toJson)
  }
}
