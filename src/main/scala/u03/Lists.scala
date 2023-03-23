package u03


import scala.annotation.tailrec

object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n - 1)

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => f(h) match
        case Cons(head, tail) => append(Cons(head, tail), flatMap(t)(f))
        case Nil() => flatMap(t)(f)
      case Nil() => Nil()


    def mapWithFM[A, B](l: List[A])(f: A => B): List[B] = l match
      case Cons(h, t) => flatMap(Cons(h, t))(x => Cons(f(x), Nil()))
      case Nil() => Nil()

    def filterWithFM[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(x => pred(x) match
        case true => Cons(x, Nil())
        case false => Nil()
      )

    import u02.Optionals.*
    import u02.Optionals.Option.*

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) => max(t) match
        case Some(x: Int) if x > h => Some(x)
        case _ => Some(h)
      case Nil() => None()

    import u02.Modules.Person
    import u02.Modules.Person.*

    def getCourses(l: List[Person]): List[String] =
      flatMap(l)(p => p match
        case Teacher(_, course) => Cons(course, Nil())
        case Student(_, _) => Nil()
      )

    @tailrec
    def foldLeft[A, B](l: List[A])(default: B)(op: (B, A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(op(default, h))(op)
      case Nil() => default


    def foldRight[A, B](l: List[A])(default: B)(op: (A, B) => B): B = l match
      case Cons(h, Nil()) => foldLeft(Cons(h, Nil()))(default)((a, b) => op(b, a))
      case Cons(h, t) => foldRight(Cons(h, Nil()))(foldRight(t)(default)(op))(op)
      case Nil() => default

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
