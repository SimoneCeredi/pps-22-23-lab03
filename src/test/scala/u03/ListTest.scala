package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

import u02.Optionals.Option.*

class ListTest:

  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))
    assertEquals(Nil(), drop(l, -5))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))
    assertEquals(tail, append(Nil(), tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFM(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFM(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFM(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFM(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(30), max(Cons(10, Cons(30, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  import u02.Modules.Person.*

  @Test def testPersonList() =
    assertEquals(Cons("pps", Cons("pps", Cons("pcd", Nil()))), getCourses(Cons(Student("A", 2022), Cons(Teacher("B", "pps"), Cons(Teacher("C", "pps"), Cons(Teacher("D", "pcd"), Nil()))))))
