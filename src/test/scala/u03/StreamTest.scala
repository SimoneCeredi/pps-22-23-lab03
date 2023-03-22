package u03

import org.junit.Assert.assertEquals
import org.junit.Test

class StreamTest:

  import Streams.*
  import Lists.List.*

  val s = Stream.take(Stream.iterate(0)(_ + 1))(10)

  @Test def testTake() =
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil())))))))))),
      Stream.toList(s))

  @Test def testDrop() =
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))
    assertEquals(Stream.empty(), Stream.drop(Stream.take(Stream.iterate(0)(_ + 1))(1))(2))

  @Test def testConstant() =
    assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFib() =
    val fibs: Stream[Int] = Stream.map(Stream.iterate((0, 1))((a, b) => (b, a + b)))((a, _) => a)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8)))