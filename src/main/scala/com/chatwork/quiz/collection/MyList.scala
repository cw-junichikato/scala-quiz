package com.chatwork.quiz.collection

import com.chatwork.quiz.{MyNone, MyOption, MySome}

import scala.annotation.tailrec


sealed trait MyList[+A] {

  // Easy
  def length: Int = foldRight(0)((_, acc) => acc + 1)

  // Normal
  @tailrec
  final def foldLeft[B](z: B)(f: (B, A) => B): B = this match {
    case MyNil => z
    case MyCons(h, t) => t.foldLeft(f(z, h))(f)
    case MyFiltered(l, p) =>
      l.foldLeft(z) { (b, a) =>
        if (p(a)) f(b, a)
        else b
      }
  }

  // 難易度選択制
  // Normal: 条件 - 特にありません、気の向くままに実装してください。
  // Hard:   条件 - foldLeftを使って実装してください。
  def foldRight[B](z: B)(f: (A, B) => B): B =
    foldLeft((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  // Normal
  // scalastyle:off
  def ::[B >: A](b: B): MyList[B] = this match {
    case MyNil => MyCons(b, MyNil)
    case list@MyCons(_, _) => MyCons(b, list)
    case MyFiltered(l, p) => MyFiltered[B](b :: l, p.asInstanceOf[B => Boolean])
  }

  // scalastyle:on

  // Normal
  def reverse: MyList[A] = foldLeft(MyList[A]())((acc, h) => MyCons(h, acc))

  // Normal
  // scalastyle:off
  def ++[B >: A](b: MyList[B]): MyList[B] = this match {
    case MyNil => b
    case MyCons(h, t) => MyCons(h, t ++ b)
    case MyFiltered(l, p) => MyFiltered[B](l ++ b, p.asInstanceOf[B => Boolean])
  }

  // scalastyle:on

  // Normal
  def map[B](f: A => B): MyList[B] = this match {
    case MyNil => MyNil
    case MyCons(h, t) => MyCons(f(h), t.map(f))
    case MyFiltered(l, p) =>
      l.foldRight[MyList[B]](MyNil) { (x, r) =>
        if (p(x)) f(x) :: r
        else r
      }
  }

  // Normal
  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case MyNil => MyNil
    case MyCons(h, t) => f(h) ++ t.flatMap(f)
    case MyFiltered(l, p) =>
      l.foldRight[MyList[B]](MyNil) { (x, r) =>
        if (p(x)) f(x) ++ r
        else r
      }
  }

  // Normal
  def filter(f: A => Boolean): MyList[A] =
    foldRight(MyNil: MyList[A])((h, t) => if (f(h)) MyCons(h, t) else t)

  // Normal: 条件 - filterと同様の実装でも構いません。
  // Hard:   条件 - 中間リストを生成しないように実装してください。
  def withFilter(f: A => Boolean): MyList[A] = this match {
    case MyNil => MyNil
    case MyCons(h, t) => MyFiltered(this, f)
    case MyFiltered(l, p) => MyFiltered[A](l, { x => p(x) && f(x) })
  }

  // Normal
  @tailrec
  final def find(f: A => Boolean): MyOption[A] = this match {
    case MyNil => MyNone
    case MyCons(h, t) => if (f(h)) MySome(h) else t.find(f)
    case MyFiltered(l, p) => l.find{ e => p(e) && f(e) }
  }

  // Normal
  def startsWith[B >: A](prefix: MyList[B]): Boolean = {
    @tailrec
    def startsWith0(l: MyList[A], prefix: MyList[B]): Boolean = (l, prefix) match {
      case (_, MyNil) => true
      case (MyCons(h, t), MyCons(h2, t2)) if h == h2 => startsWith0(t, t2)
      case (l@MyFiltered(_, _),r@MyFiltered(_, _)) => startsWith0(l.toList, r.toList)
      case _ => false
    }
    startsWith0(this, prefix)
  }

  def toList[B >: A]: MyList[B] = this match {
    case MyNil => MyNil
    case MyFiltered(l, p) => l.filter(p)
    case _ => this
  }

}

case object MyNil extends MyList[Nothing]

case class MyCons[A](head: A, tail: MyList[A]) extends MyList[A]

case class MyFiltered[A](l: MyList[A], p: A => Boolean) extends MyList[A]

object MyList {

  // Easy
  def empty[A]: MyList[A] = MyNil

  // Normal
  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty)
      MyNil
    else
      MyCons(as.head, apply(as.tail: _*))
  }

}
