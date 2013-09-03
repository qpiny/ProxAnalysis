package org.rejna.prox

import scala.collection.mutable.ListBuffer
import scala.io.Source

object RawByteData {
  def apply(input: Source) = {
    new RawByteData(input.mkString.split("\\s+").map(_.toByte): _*)
  }
}

class RawByteData(val values: Byte*) extends IndexedSeq[Byte] {
  val bit0 = false
  val bit1 = true
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val ask = {
    val s = (Seq.empty[Boolean] /: this)((ret, curr) => {
      val prev = ret.lastOption.getOrElse(bit0)
      if (curr == max && prev == bit0) ret :+ bit1
      else if (curr == min && prev == bit1) ret :+ bit0
      else ret :+ prev
    })
    new RawBoolData(s: _*)
  }

  override def toString = values.map(b => f"$b%4d").mkString(",")
}

class RawBoolData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val edges = {
    val builder = ListBuffer.empty[Int]
    var prev = false
    var prevIdx = 0
    for ((x, i) <- values.zipWithIndex)
      if (x ^ prev) {
        builder += i - prevIdx
        prevIdx = i
        prev = x
      }
    builder.tail
  }

  lazy val clock = if (length == 0)
    (0, 0, 0)
  else
    (edges.min, edges.max, edges.sum.toFloat / edges.length)

  lazy val normalise = {
    val builder = ListBuffer.empty[Boolean]
    var prev = true
    //builder += prev
    for (edge <- edges) {
      val ncycles = (edge.toFloat / clock._1).round // {ncycles,edge} could be 0 ?
      if (ncycles == 0) {
        println(s"${clock._1} / ${edge} = 0")
        builder += prev
      }
      for (i <- 1 to ncycles)
        builder += prev
      prev = !prev
    }
    new BinData(builder: _*)
  }

  override def toString = values.map(b => if (b) "   1" else "   0").mkString(",")
}

object BinData {
  def apply(data: Seq[Boolean]) = new BinData(data: _*)
}

class BinData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  def manchester(lohi: Boolean) = {
    val errors = values.grouped(2).count(t => !(t.head ^ t.last))
    val v = values.grouped(2).flatMap {
      case Seq(t, l, dummy @ _*) if (t ^ l) => Some(t ^ lohi)
      case _ => None
    } toSeq

    (errors, new BinData(v: _*))
  }

  def biphase(flat: Boolean) = {
    // check biphase validity
    val errors = values.tail.grouped(2).count(t => !(t.head ^ t.last))

    val s = for (t <- values.grouped(2))
      yield t.head ^ t.last ^ flat
    (errors, new BinData(s.toSeq: _*))
  }

  def checkPeriodicityString: String = {
    checkPeriodicity().map(b => f"$b%4d").mkString(",")
  }

  def checkPeriodicity(other: Seq[Boolean] = values.tail): List[Int] = {
    if (other.length < 10)
      Nil
    else {
      val s = other.zip(values).count {
        case (a, b) => a == b
      }
      (s * 100 / other.length) :: checkPeriodicity(other.tail)
    }
  }

  lazy val maxConsecutive = {
    var max0Cnt = 0
    var max1Cnt = 0
    var max0Idx = -1
    var max1Idx = -1
    var prev = !values.head
    var prevCnt = -1
    var prevIdx = -1
    for ((x, i) <- values.zipWithIndex) {
      if (x == prev) {
        prevCnt += 1
      }
      else {
        if (prev) {
          if (prevCnt > max0Cnt) {
            max1Cnt = prevCnt
            max1Idx = prevIdx
          }
        }
        else {
          if (prevCnt > max1Cnt) {
            max0Cnt = prevCnt
            max0Idx = prevIdx
          }
        }
        prevIdx = i
        prev = x
        prevCnt = 1
      }
    }
    (max0Cnt, max0Idx, max1Cnt, max1Idx)
  }

  override def toString = values.map(b => if (b) "1" else "0").mkString
}