package org.rejna.prox

import scala.io.{ Source, Codec }
import scala.collection.mutable.MapBuilder
import scala.collection.immutable.Map
import scala.util.{ Failure, Success }

class ProcessedData(val name: String, raw: RawIntData) {
  val ask = raw.ask
  val norm = ask.normalise
  val output = Map(
    "manchester" -> norm.manchester(false),
    "biphase" -> norm.biphase(true)) flatMap {
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

  def correlate5(size: Int, data1: BinData, data2: BinData, data3: BinData, data4: BinData, data5: BinData) = {

    val matrix = for {
      i2 <- 0 until size;
      i3 <- 0 until size;
      i4 <- 0 until size;
      i5 <- 0 until size;
      d2 = data2.rot(i2);
      d3 = data3.rot(i3)
      d4 = data2.rot(i4);
      d5 = data3.rot(i5)
    } yield (data1
      .corr2(d2
        .corr2(d3
          .corr2(d4
            .corr2(d5
              .map((true, _))))))
      .count(_._1), i2, i3, i4, i5)
    //    for (b1 <- data1;
    //    b2 <- d2;
    //    b3 <- d3;
    //    b4 <- d4;
    //    b5 <- d5) yield (b1 == b2) && (b2 == b3) && (b3 == b4) && (b4 == b5)
    //    (
    //        
    //      data1 zip d2 zip d3 count { x => (x._1._1 == x._1._2) && (x._1._2 == x._2) },
    //      i2, i3)
    matrix.maxBy(_._1)
  }

  def findSameBit(data: BinData*) = {
    if (data.isEmpty)
      new RawIntData()
    else {
      val b = (Array.fill(data.head.length)(0) /: data)((s, d) => (s zip d) map {
        case (a, b) => if (b) a + 1 else a
      })
      RawIntData(b)
    }
  }
  override def main(args: Array[String]): Unit = {
    val bb = BinData(Source.fromFile("data/bertrand_B316_FQHD_59525_biphase.data"))
    val bm = BinData(Source.fromFile("data/bertrand_B316_FQHD_59525_manchester.data"))
    val cb = BinData(Source.fromFile("data/cyril_A028_FQHD_12915_biphase.data"))
    val cm = BinData(Source.fromFile("data/cyril_A028_FQHD_12915_manchester.data"))
    val fb = BinData(Source.fromFile("data/fabienne_D313_FQHD_46142_biphase.data"))
    val fm = BinData(Source.fromFile("data/fabienne_D313_FQHD_46142_manchester.data"))
    val jb = BinData(Source.fromFile("data/jerome_A02Q_FQHD_44186_biphase.data"))
    val jm = BinData(Source.fromFile("data/jerome_A02Q_FQHD_44186_manchester.data"))
    val mb = BinData(Source.fromFile("data/mathieu_D426_FQ_21246_biphase.data"))
    val mm = BinData(Source.fromFile("data/mathieu_D426_FQ_21246_manchester.data"))
    val tb = BinData(Source.fromFile("data/thomas_BN07_FQHD_49033_biphase.data"))
    val tm = BinData(Source.fromFile("data/thomas_BN07_FQHD_49033_manchester.data"))

    val top = 128
    println("Computing correlation for biphase : ")
    val a = new Correlation(bb).makeWith(cb, top)
    println(s"1) ${a.mkString("\n")})")
    val b = a.flatMap(_.makeWith(fb, top)).sorted.takeRight(top)
    println(s"2) ${b.mkString("\n")}")
    val c = b.flatMap(_.makeWith(jb, top)).sorted.takeRight(top)
    println(s"3) ${c.mkString("\n")}")
    val d = c.flatMap(_.makeWith(mb, top)).sorted.takeRight(top)
    println(s"4) ${d.mkString("\n")}")
    val e = d.flatMap(_.makeWith(tb, top)).sorted.takeRight(top)
    println(s"5) ${e.mkString("\n")}")

    //    val data = new java.io.File("pm3").listFiles
    //      .filter(_.getName.startsWith("thomas"))
    //      .map(f => new ProcessedData(f.getName, RawIntData(Source.fromFile(f))))
    //
    //    println("manchester:")
    //    for (d <- data) {
    //      d.output("manchester") match {
    //        case Success((p, a)) => println(a)
    //        case _ =>
    //      }
    //    }
    //    println("biphase:")
    //    for (d <- data) {
    //      d.output("biphase") match {
    //        case Success((p, a)) => println(a)
    //        case _ =>
    //      }
    //    }
    //       println(s"edges=${d.ask.edges}")
    //       println(s"clock=${d.ask.clock}")
    //       println(s"ask=${d.ask}")
    //       println(s"norm=${d.norm}")
    //       println(s"output=${d.output}")
    //     }
    //    val headers = data.flatMap(prd => prd.header).groupBy(_._1.tail).map {
    //      case (t, d) => s"${t}/same" -> findSameBit(d.map(_._2): _*)
    //    }
    //
    //    val output = headers ++
    //      (data.flatMap(_.outputWithName)) ++
    //      (data.flatMap(_.parityWithName)) ++
    //      (data.flatMap(_.headerWithName))
    //
    //    for ((n, d) <- output.toList)
    //      println(s"${n}|${d}")
  }
}