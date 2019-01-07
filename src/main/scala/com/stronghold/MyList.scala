package redbook

import scala.annotation.tailrec
import scala.reflect.internal.util.Origins.MultiLine

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

/**
  * Companion object to our singly-linked MyList construct
  */
object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(head: Int, tail: MyList[Int]) => head + sum(tail)
  }

  def prod(doubles: MyList[Double]): Double = doubles match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head: Double, tail: MyList[Double]) => head * prod(tail)
  }

  /**
    * A more convenient construction for lists
    * @param as
    * @tparam A
    * @return
    */
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](xs: MyList[A]): MyList[A] = xs match {
    case Nil => Nil
    case Cons(head: A, Nil) => Nil
    case Cons(head: A, tail: MyList[A]) => tail
  }

  def setHead[A](xs: MyList[A], newHead: A): MyList[A] = xs match {
    case Nil => Nil
    case Cons(head: A, tail: MyList[A]) => Cons[A](newHead, tail)
  }

  def setHead2[A](xs: MyList[A], newHead: A): MyList[A] = Cons(newHead, tail(xs))

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n == 0) return l
    drop(tail(l), n - 1)
  }

  def dropWhile[A](l: MyList[A], p: A => Boolean): MyList[A] = {
    val stopDropping: Boolean = l match {
      case Nil => true
      case Cons(head, tail) => !p(head)
    }
    if (stopDropping) l else dropWhile(tail(l), p)
  }

  def append[A](thisMyList: MyList[A], thatMyList: MyList[A]): MyList[A] = thisMyList match {
    case Nil => thatMyList
    case Cons(head, tail) => Cons(head, append(tail, thatMyList))
  }

  def init[A](l: MyList[A]): MyList[A] = {
    def loop(inMyList: MyList[A], outMyList: MyList[A] = Nil): MyList[A] = {
      if (tail(inMyList) == Nil) return outMyList
      val newOutMyList: MyList[A] = inMyList match {
        case Nil => Nil
        case Cons(head, tail) => append(outMyList, Cons(head, Nil))
      }
      loop(tail(inMyList), newOutMyList)
    }
    loop(l)
  }

  /**
    * Applies a binary operator to each member of a collection, usually with the goal
    * of capturing the aggregate result of applying the operator to the collection.
    * For example, we can find the sum of a collection by capturing the sum of each
    * element and the aggregate sum until that point.
    *
    * `foldRight()` does this by recursively drilling "up" the call stack until it
    * reaches right most member of the collection, and then applying the given binary
    * operator as we navigate back "down" the call stack. Since we recurse up to the
    * rightmost value (with the `tail()` function), when we work our way back down the
    * stack, we are moving from right to left.
    *
    * Note that the maintenance of stack frames for each call means that the call
    * stack can grow quickly. (This implementation of `foldRight()`) is *not* tail
    * recursive. For large collections, stack overflows are a threat.
    *
    * @param xs
    * @param id
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A, B](as: MyList[A], id: B)(f: (A, B) => B): B = as match {
    case Nil => id
    case Cons(head, tail) => f(head, foldRight(tail, id)(f))
  }

  def length[A](as: MyList[A]): Int = {
    foldRight[A, Int](as = as, id = 0)((x: A, b: Int) => b + 1)
  }

  /**
    * Applies a binary operator to each member of a collection, usually with the goal
    * of capturing the aggregate result of applying the operator to the collection.
    * For example, we can find the sum of a collection by capturing the sum of each
    * element and the aggregate sum until that point.
    *
    * `foldLeft()` does this by applying the function to the first (i.e. "leftmost")
    * members of the collection and capturing the result immediately. This result is
    * then carried rightward via the next tail recursive call on the tail of the initial
    * collection, entering the associated application of the binary operator.
    * In the `foldLeft()` case, carrying the result along to each successive call means
    * that all of the relevant information about the previous call is represented. As a
    * consequence, we need not maintain state in the stack frame of the previous call
    * and we can "re-use" the memory allocated to that stack frame. In other words,
    * we have no need to travel up and down the stack. Tail recursive optimization
    * from the Scala compiler (which effectively instructs the JVM to use GOTOs) allows
    * us to recurse "in place" from the perspective of the call stack.
    *
    * @param xs
    * @param id
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldLeft[A,B](as: MyList[A], id: B)(f: (B, A) => B): B = {
    @tailrec def loop(as: MyList[A], agg: B = id): B = {
      if (as == Nil) return agg
      val newAgg: B = as match {
        case Nil => id
        case Cons(head, tail) => f(agg, head)
      }
      loop(as = tail(as), agg = newAgg)
    }
    loop(as)
  }

  def length2[A](as: MyList[A]): Int = {
    foldLeft[A, Int](as = as, id = 0)((a: Int, b: A) => a + 1)
  }

  def sum2(as: MyList[Int]): Int = {
    foldLeft[Int, Int](as = as, id = 0)(_ + _)
  }

  def prod2(as: MyList[Int]): Int = {
    foldLeft[Int, Int](as = as, id = 1)(_ * _)
  }

  /**
    * Reverses the order of a collection by starting with an empty list and
    * then prepending each member of the list to this new list while traversing
    * the old list from left to right.
    *
    * @param as
    * @tparam A
    * @return
    */
  def reverse[A](as: MyList[A]): MyList[A] = {
    foldLeft[A, MyList[A]](as = as, id = Nil: MyList[A])( (tail: MyList[A], head: A) => Cons(head, tail) )
  }

  def rightReverse[A](as: MyList[A]): MyList[A] = {
    foldRight[A, MyList[A]](as = as, id = Nil: MyList[A]){
      case (head: A, tail: MyList[A]) =>
        val newTail: MyList[A] = Cons(head, Nil: MyList[A])
        val newHead: MyList[A] = tail
        append(newHead, newTail)
    }
  }

  def rLeftFold[A, B](as: MyList[A], id: B)(f: (A, B) => B): B = {
    val revMyList: MyList[A] = rightReverse[A](as)
    foldRight[A,B](as = as, id = id)(f)
  }

  def lRightFold[A,B](as: MyList[A], id: B)(f: (B, A) => B): B = {
    val revMyList: MyList[A] = reverse[A](as)
    foldLeft[A,B](as = as, id = id)(f)
  }

  def append2[A](thisMyList: MyList[A], thatMyList: MyList[A]): MyList[A] = {
    val revThisMyList: MyList[A] = reverse[A](thisMyList)
    foldLeft[A, MyList[A]](as = revThisMyList, id = thatMyList)( (tail: MyList[A], head: A) => Cons(head,tail) )
  }

  // Not yet complete, come back to this
  def concatMyLists[A](ls: MyList[MyList[A]]): MyList[A] = {
    foldLeft[MyList[A], MyList[A]](as = ls, id = Nil: MyList[A]) {
      case (tail: MyList[MyList[A]], head: MyList[A]) =>
        val tailSplit: (MyList[A], MyList[MyList[A]]) = tail match {
          case Nil => (Nil, Nil)
          case Cons(head2, tail2) => (head2, tail2)
        }
        val newHead: MyList[A] = append2(head, tailSplit._1)
        val newTail: MyList[A] = concatMyLists(tailSplit._2)
        append2(newHead, newTail)
    }
  }

  def splitAtIdx[A](l: MyList[A], idx: Int): (MyList[A], MyList[A]) = ???


  def insert[A](l: MyList[A], idx: Int, value: A): MyList[A] = ???

  def map[A, B](l: MyList[A], f: A => B): MyList[B] = {
    def loop(inList: MyList[A], outList: MyList[B] = Nil): MyList[B] = inList match {
      case Nil => outList
      case Cons(head, tail) => loop(tail, append2(outList, Cons(f(head), Nil)))

    }
    loop(l)
  }

  def filter[A](l: MyList[A], p: A => Boolean): MyList[A] = {
    def loop(inList: MyList[A], outList: MyList[A] = Nil): MyList[A] = inList match {
      case Nil => outList
      case Cons(head, tail) if p(head) => Cons(head, filter(tail, p))
      case Cons(head, tail) if !p(head) => filter(tail, p)
    }
    loop(l)
  }

  def flatMap[A, B](l: MyList[A])(f: A => MyList[B]): MyList[B] = {
    def loop(inList: MyList[A], outList: MyList[B] = Nil): MyList[B] = inList match {
      case Nil => outList
      case Cons(head, tail) => loop(tail, append(outList, f(head)))
    }
    loop(l)
  }

  def flatFilter[A](l: MyList[A])(p: A => Boolean): MyList[A] = ???
}
