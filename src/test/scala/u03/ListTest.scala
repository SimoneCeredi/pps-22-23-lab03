package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:

  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  /**
   * val lst = Cons (10 , Cons (20 , Cons (30 , Nil () )) )
   * drop ( lst , 1) // Cons (20 , Cons (30 , Nil ()))
   * drop ( lst , 2) // Cons (30 , Nil ())
   * drop ( lst , 5) // Nil ()
   *
   * @return
   */
  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))
    assertEquals(Nil(), drop(l, -5))
