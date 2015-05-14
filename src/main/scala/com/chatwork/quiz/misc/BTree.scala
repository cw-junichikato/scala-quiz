package com.chatwork.quiz.misc

import scala.collection.immutable.NumericRange

sealed trait Node[+A] {

  val value: A

  val size: Int
  val sum: A
  val avg: A
  val max: A
  val min: A

  def find[B >: A](value: => B): Option[Node[B]]

  def map[B >: A](f: A => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B]

}

object Node {

  def leaf[A](value: => A) = new Leaf(() => value)

  def branch[A](left: => Node[A], value: => A, right: => Node[A])(implicit O: Ordering[A], F: Fractional[A]) =
    new Branch(() => left, () => value, () => right)

}

class Leaf[A](v: () => A) extends Node[A] {

  lazy val value = v()

  val size = 1
  lazy val sum = value
  lazy val avg = value
  lazy val max = value
  lazy val min = value

  override def hashCode(): Int = 31 * value.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Leaf[_] => this.value == that.value
    case _ => false
  }

  override def toString: String = s"Leaf($value)"

  def find[B >: A](value: => B): Option[Node[B]] =
    if (this.value == value) Some(this) else None

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] = Node.leaf(f(value))

}

class Branch[A](l: () => Node[A], v: () => A, r: () => Node[A])
               (implicit O: Ordering[A], F: Fractional[A]) extends Node[A] {

  lazy val value = v()
  lazy val left = l()
  lazy val right = r()

  lazy val max: A = right.max

  lazy val min: A = left.min

  lazy val size: Int = left.size + 1 + right.size

  lazy val sum: A = List(left.sum, value, right.sum).sum

  lazy val avg: A = F.div(sum, F.fromInt(size))

  def find[B >: A](value: => B): Option[Node[B]] = this match {
    case Branch(_, v, _) if v == value => Some(this)
    case Branch(l, v, _) if O.lt(value.asInstanceOf[A], v) => l.find(value)
    case Branch(_, v, r) if O.lt(v, value.asInstanceOf[A]) => r.find(value)
  }

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] =
    Node.branch(left.map(f), f(value), right.map(f))

  override def hashCode(): Int = 31 * (right.hashCode() + value.hashCode() + left.hashCode())

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Branch[_] =>
      this.right == that.right &&
        this.value == that.value &&
        this.left == that.left
    case _ => false
  }

  override def toString: String = s"Branch($left, $value, $right)"

}

object Branch {

  def unapply[A](self: Branch[A]): Option[(Node[A], A, Node[A])] =
    Some((self.left, self.value, self.right))

}

class BTree[A](n: () => Node[A])(implicit F: Fractional[A]) {

  lazy val node = n()

  lazy val size: Int = node.size

  lazy val max: A = node.max

  lazy val min: A = node.min

  lazy val sum: A = node.sum

  lazy val avg: A = F.div(sum, F.fromInt(size))

  def find[B >: A](number: => B): Option[Node[B]] = node.find(number)

  def map[B >: A](f: (A) => B)(implicit O: Ordering[B], F: Fractional[B]): Node[B] = node.map(f)

  override def hashCode(): Int = 31 * node.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: BTree[_] => node == that.node
    case _ => false
  }

  override def toString: String = s"BTree($node)"

}

object BTree {

  def apply[A](node: => Node[A])(implicit F: Fractional[A]): BTree[A] = new BTree[A](() => node)

  def apply[A](values: Seq[A])(implicit O: Ordering[A], F: Fractional[A]): Node[A] = {
    if (values.size == 1) {
      Node.leaf(values.head)
    } else {
      require(values.size % 2 == 1)
      val v = values.size / 2
      val (left, mid :: right) = values.splitAt(v)
      Node.branch(apply(left), mid, apply(right))
    }
  }

}

object Main extends App {

  implicit val integral = scala.math.Numeric.BigDecimalAsIfIntegral

  val numbers = NumericRange(BigDecimal(1), BigDecimal(32768), BigDecimal(1)).toList
  val node = BTree(numbers)
  println("max = " + node.max)
  println("min = " + node.min)
  println("sum = " + node.sum)
  println("avg = " + node.avg)
  println("find(3) = " + node.find(BigDecimal(3)))
  println(node)

}

