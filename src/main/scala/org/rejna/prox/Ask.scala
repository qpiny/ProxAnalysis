package org.rejna.prox

import scala.collection.mutable.ListBuffer

class RawIntData(val values: Byte*) extends IndexedSeq[Byte] {
  val bit0 = false
  val bit1 = true
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val ask = {
    val s = (Seq(head > 0) /: this)((ret, curr) => {
      val prev = ret.last
      if (curr == max && prev == bit0) ret :+ bit1
      else if (curr == min && prev == bit1) ret :+ bit0
      else ret :+ prev
    })
    new RawBoolData(s: _*)
  }
}

class RawBoolData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val edges = {
    val builder = ListBuffer.empty[Int]
    var prev = false
    for ((x, i) <- values.zipWithIndex)
      if (x ^ prev) {
        builder += i
        prev = x
      }
    builder
  }

  lazy val clock = if (length == 0)
    (0, 0, 0)
  else
    (edges.min, edges.max, edges.sum / edges.length)

  lazy val normalise = {
    val builder = ListBuffer.empty[Boolean]
    var prev = false
    builder += prev
    for (edge <- edges) {
      val ncycles = clock._1 / edge // {ncycles,edge} could be 0 ?
      for (i <- 1 to ncycles)
        builder += prev
      prev = !prev
    }
    new BinData(builder: _*)
  }
}

class BinData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  def manchester(lohi: Boolean) = {
    val s = for (t <- values.grouped(2))
      yield (t.head, t.last) match {
      case (false, false) | (true, true) => sys.error("PB !")
      case (true, false) => !lohi
      case (false, true) => lohi
    }
    new BinData(s.toSeq: _*)
  }

  def biphase(flat: Boolean) = {
    val s = for (t <- values.grouped(2))
      yield t.head ^ t.last ^ flat
    new BinData(s.toSeq: _*)
  }
}