package org.rejna.prox

import scala.io.{ Source, Codec }
import scala.collection.mutable.MapBuilder
import scala.collection.immutable.Map
import scala.util.{ Failure, Success }
import Data._

class ProcessedData(val name: String, raw: RawIntData) {
  val ask = raw.ask
  val norm = ask.normalise
  val output = Map(
    "1" -> norm,
    "2" -> norm.shift) flatMap {
      case (n, d) =>
        (s"${n}/manchester" -> d.manchester(false)) ::
          (s"${n}/biphase" -> d.biphase(true)) ::
          Nil
    } flatMap {
      case (n, Success(d)) =>
        val periodicity = d.checkPeriodicity
        val periodicityScore = periodicity.max
        if (periodicityScore > 80) // min 80% of similarity between 2 periods
          List(n -> Success((periodicity.indexOf(periodicityScore) + 1, d)))
        else
          List(n -> Failure(new DemodulationException(s"Periodicity not found : ${periodicity}", periodicityScore)))
      case (n, Failure(e)) =>
        List(n -> Failure(e))
    }

  val outputWithName = output.map {
    case (t, d) => s"${name}/${t}" -> d
  }

  val header = output flatMap {
    case (n, Success((p, d))) =>
      d.extractFromHeader(p).zipWithIndex.map {
        case (d, i) => s"${n}/header${i}" -> d
      }
    case (n, Failure(e)) => Nil
  }

  val headerWithName = header.map {
    case (t, d) => s"${name}/${t}" -> d
  }

  val parity = output flatMap {
    case (n, Success((p, d))) =>
      s"${n}/parity4" -> d.findParity(4) ::
        s"${n}/parity8" -> d.findParity(8) ::
        Nil
    case (n, Failure(e)) => Nil
  }

  val parityWithName = parity.map {
    case (t, d) => s"${name}/${t}" -> d
  }
}

object Main extends App {

  def findSameBit(data: BinData*) = {
    if (data.isEmpty)
      new RawIntData()
    else {
      val b = (Array.fill(data.head.length)(0) /: data)((s, d) => (s zip d) map {
        case (a, b) => if (b) a + 1 else a
      })
      new RawIntData(b: _*)
    }
  }
  override def main(args: Array[String]): Unit = {
    val data = new java.io.File("pm3").listFiles.map(f => new ProcessedData(f.getName, Source.fromFile(f)))
    val headers = data.flatMap(prd => prd.header).groupBy(_._1.tail).map {
      case (t, d) => s"${t}/same" -> findSameBit(d.map(_._2): _*)
    }

    val output = headers ++
      (data.flatMap(_.outputWithName)) ++
      (data.flatMap(_.parityWithName)) ++
      (data.flatMap(_.headerWithName))

    for ((n, d) <- output.toList)
      println(s"${n}|${d}")
  }
}