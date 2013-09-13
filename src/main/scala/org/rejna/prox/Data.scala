package org.rejna.prox

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{ Try, Success, Failure }

class DemodulationException(message: String, val errors: Int, cause: Exception = null) extends Exception(message, cause)

object DisabledData {
  implicit def seq2rawInt(values: Seq[Int]) = new RawIntData(values: _*)
  implicit def source2rawInt(input: Source) = new RawIntData(input.mkString.split("\\s+").map(_.toInt): _*)

  implicit def seq2rawBool(values: Seq[Boolean]) = new RawBoolData(values: _*)
  implicit def seq2bin(values: Seq[Boolean]) = new BinData(values: _*)
}

object RawIntData {
  def apply(values: Seq[Int]) = new RawIntData(values: _*)
  def apply(input: Source) = new RawIntData(input.mkString.split("\\s+").map(_.toInt): _*)
}

class RawIntData(val values: Int*) extends IndexedSeq[Int] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val ask = {
    var prev = values.head > 0
    val ma = max
    val mi = min
    val s = for (x <- values)
      yield x match {
      case `ma` =>
        prev = true
        prev
      case `mi` =>
        prev = false
        prev
      case _ =>
        prev
    }
    RawBoolData(s)
  }
  override def toString = values.map(b => f"$b%4d").mkString(",")
}

object RawBoolData {
  def apply(values: Seq[Boolean]) = new RawBoolData(values: _*)
}

class RawBoolData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val edges = {
    val builder = ListBuffer.empty[Int]
    var prev = false
    var prevIdx = 0
    for ((x, i) <- values.zipWithIndex)
      if (x != prev) {
        builder += i - prevIdx
        prevIdx = i
        prev = x
      }
    builder.drop(2)
  }

  // works only if there are 2 frequencies, one half of the other
  lazy val clock = if (length == 0)
    1
  else {
    val median = (edges.max + edges.min) / 2
    val ee = edges.map(e => if (e < median) e * 2 else e)
    ee.sum / (2 * ee.length)
  }

  lazy val normalise = {
    val builder = ListBuffer.empty[Boolean]
    var prev = true
    //builder += prev
    for (edge <- edges) {
      val ncycles = (edge.toFloat / clock).round // {ncycles,edge} could be 0 ?
      if (ncycles == 0) {
        println(s"${clock} / ${edge} = 0")
        builder += prev
      }
      for (i <- 1 to ncycles)
        builder += prev
      prev = !prev
    }
    BinData(builder)
  }

  override def toString = values.map(b => if (b) "   1" else "   0").mkString(",")
}

object BinData {
  def apply(values: Seq[Boolean]) = new BinData(values: _*)
  def apply(input: Source) = new BinData(input.mkString.map(_ == '1'): _*)
}

class BinData(val values: Boolean*) extends IndexedSeq[Boolean] {
  def apply(idx: Int) = values(idx)
  def length = values.length

  lazy val shift = BinData(values.tail)

  def rot(n: Int) = BinData(values.drop(n) ++ values.take(n))

  def corr2(that: Seq[(Boolean, Boolean)]) = that.zip(this).map {
    case ((a, b), c) if a && b == c => (true, b)
    case ((a, b), c) => (false, b)
  }

  def manchester(lohi: Boolean, shifted: Boolean = false): Try[BinData] = {
    //    println(s"trying Manchester with : ${this}")
    //    val errorIndex = values.zipWithIndex.grouped(2).collect {
    //      case Seq((tv, ti), (lv, li), dummy @ _*) if tv == lv => (ti, li) 
    //    }
    //    println(s"Error are at : " + ("" /: errorIndex) {
    //      case (s, (t, l)) => s"${s} [${t}:${l}]"
    //    })

    val errors = values.grouped(2).count(t => !(t.head ^ t.last))

    if (errors > values.length / 10) {
      if (shifted)
        Failure(new DemodulationException(s"Manchester demodulation failed (${errors}/${values.length})", errors))
      else
        shift.manchester(lohi, true)
    } else {
      val v = values.grouped(2).flatMap { // use collect instead of flatmap
        case Seq(t, l, dummy @ _*) if (t ^ l) => Some(t ^ lohi)
        case _ => None
      }

      Success(BinData(v.toSeq))
    }
  }

  def biphase(flat: Boolean, shifted: Boolean = false): Try[BinData] = {
    // check biphase validity
    val errors = values.tail.grouped(2).count(t => !(t.head ^ t.last))

    if (errors > values.length / 10) {
      if (shifted)
        Failure(new DemodulationException(s"Biphase demodulation failed (${errors}/${values.length})", errors))
      else
        shift.biphase(flat, true)
    } else {
      val v = for (t <- values.grouped(2))
        yield t.head ^ t.last ^ flat
      Success(BinData(v.toSeq))
    }
  }

  def checkPeriodicity: RawIntData = {
    RawIntData(checkPeriodicity(values.tail))
  }

  def checkPeriodicity(other: Seq[Boolean]): List[Int] = {
    if (other.length < 10)
      Nil
    else {
      val s = other.zip(values).count(x => x._1 == x._2)
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
      } else {
        if (prev) {
          if (prevCnt > max1Cnt) {
            max1Cnt = prevCnt
            max1Idx = prevIdx
          }
        } else {
          if (prevCnt > max0Cnt) {
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

  def extract(size: Int, offset: Int) = {
    BinData(values.drop(offset).take(size) ++ values.take(offset + size - values.length))
  }

  def extractFromHeader(period: Int) = {
    extract(period, maxConsecutive._2) ::
      extract(period, maxConsecutive._4) ::
      Nil
  }

  def findParity(size: Int) = {
    RawIntData((0 until size).map {
      case offset =>
        val group = values.drop(offset).toList.grouped(size + 1).toList
        val l = group.length
        group.count(b => {
          if (b.length > 1)
            b.init.reduce(_ ^ _) == b.last
          else
            true
        }) * 100 / l
    })
  }

  override def toString = values.map(b => if (b) "1" else "0").mkString
}

class Correlation(p: List[Int], v: Seq[Int]) extends Ordered[Correlation] {
  def this(v: BinData) = this(List(0), v.map(if (_) 1 else 0))

  lazy val size = v.length

  def score = v.count(x => x == 0 || x == size)
  
  def compare(that: Correlation) = this.score - that.score
  
  def makeWith(d: BinData, top: Int) = {
    // d.length == v.length
    
    var ss = List[Correlation]()
    var min = Int.MaxValue
    var toplen = 0
    
    for (i <- 0 until size) {
      val c = new Correlation(i :: p, d.rot(i) zip v map {
        case (true, x) => x + 1
        case (false, x) => x
      })
      if (toplen < top || c.score > min) {
        ss = (c :: ss).sorted
        min = ss.head.score
        toplen += 1
      }
      if (toplen > top) {
        ss = ss.tail
        min = ss.head.score
        toplen -= 1
      }
    }
    ss
  }
  
  override def toString = {
    p.mkString("[", ",", "]") + "(" + score + ")"
  }
}