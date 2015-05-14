package com.chatwork.quiz.misc

import org.scalatest.{FunSpec, Matchers}

class BTreeSpec extends FunSpec with Matchers {


  describe("BTree#size") {
    it("should return the number of elements in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).size shouldBe 3
      BTree(Node.leaf(1)).size shouldBe 1
    }
  }

  describe("BTree#max") {
    it("should return the max value in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).max shouldBe 3
    }
  }

  describe("BTree#min") {
    it("should return the min value in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).min shouldBe 1
    }
  }

  describe("BTree#sum") {
    it("should return the sum of values in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).sum shouldBe 6
    }
  }

  describe("BTree#avg") {
    it("should return the average of values in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).avg shouldBe 2.0d
    }
  }

  describe("BTree#find") {
    it("should return a node has the value in the BTree") {
      BTree(Node.branch(Node.leaf(1), 2, Node.leaf(3))).find(1) shouldBe Some(Node.leaf(1))
    }
  }

  describe("BTree#apply") {
    it("should return a new BTree from List[Int]") {
      BTree(List(1, 2, 3)) shouldEqual Node.branch(Node.leaf(1), 2, Node.leaf(3))
    }
  }

  implicit val intFractional = new Fractional[Int] with Ordering[Int] {
    override def div(x: Int, y: Int): Int = x / y

    override def toDouble(x: Int): Double = x.toDouble

    override def plus(x: Int, y: Int): Int = x + y

    override def toFloat(x: Int): Float = x.toFloat

    override def toInt(x: Int): Int = x

    override def negate(x: Int): Int = -x

    override def fromInt(x: Int): Int = x

    override def toLong(x: Int): Long = x.toLong

    override def times(x: Int, y: Int): Int = x * y

    override def minus(x: Int, y: Int): Int = x - y

    override def compare(x: Int, y: Int): Int = x compare y
  }

}
