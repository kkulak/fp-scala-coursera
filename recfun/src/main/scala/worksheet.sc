
sum(x => x * x, 1, 4)

sum_tail(x => x * x)(1, 4)

product_rec(x => x * x)(1, 4)
product_rec_shorter(x => x * x)(1, 4)

product_rec_shorter(factorial)(1, 4)

general(x => x * x)((a, b) => a + b)(0)(1, 4)
general(x => x * x)((a, b) => a * b)(1)(1, 4)

def factorial(n: Int): Int = {
  if(n == 0) 1 else n * factorial(n - 1)
}

def sum(f: Int => Int, a: Int, b: Int): Int = {
  if(a > b) 0 else f(a) + sum(f, a + 1, b)
}

def sum_tail(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def product_rec(f: Int => Int): (Int, Int) => Int = {
  def inner(a: Int, b: Int):Int = {
    if(a > b) 1 else f(a) * inner(a + 1, b)
  }
  inner
}

def product_rec_shorter(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1 else f(a) * product_rec_shorter(f)(a + 1, b)
}

def general(f: Int => Int)(op: (Int, Int) => Int)(acc: Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc else loop(a + 1, op(f(a), acc))
  }
  loop(a, acc)
}

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def union(other: IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def contains(x: Int): Boolean = {
    if(x < elem) left contains x
    else if(x > elem) right contains x
    else true
  }

  override def incl(x: Int): IntSet = {
    if(x < elem) new NonEmpty(elem, left incl x, right)
    else if(x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  override def union(other: IntSet) = ((left union right) union other) incl elem

  override def toString = "{" + left + elem + right + "}"
}

val root: NonEmpty = new NonEmpty(10, new Empty, new Empty)
val root2 = root.incl(3)
val root3 = root2.incl(2)
root3.contains(3)
root3.contains(0)

trait List[T] {
  def empty: Boolean
  def head: T
  def tail: List[T]
}

class Nil[T] extends List[T] {
  override def empty: Boolean = true
  override def tail: Nothing = throw new NoSuchElementException("Nil.head")
  override def head: Nothing = throw new NoSuchElementException("Nil.tail")
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def empty: Boolean = false
}

def nth[T](n: Int, list: List[T]): T = {
  def find[T](n: Int, list: List[T], index: Int): T = {
    if(index == n) list.head
    else find(n, list.tail, index + 1)
  }

  find(n, list, 0)
}