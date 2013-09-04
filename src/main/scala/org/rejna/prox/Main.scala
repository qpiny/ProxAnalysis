package org.rejna.prox

import scala.io.{ Source, Codec }
import scala.collection.mutable.MapBuilder
import scala.collection.immutable.Map


class ProcessedData(name: String, raw: RawIntData) {
  val ask = raw.ask
  Map(s"${name}/norm" -> ask.normalise) flatMap {
    case (n, d) =>
      (s"${n}1" -> d) ::
      (s"${n}2" -> d.shift) ::
      Nil
  } flatMap {
    case (n, d) =>
      (s"${n}1/manchester" -> d.manchester(false)) ::
      (s"${n}1/biphase" -> d.biphase(true)) ::
      Nil
  } flatMap {
    case (n, (e, d)) =>
      if (e > d.length / 10) // error max 10%
        Nil
      else
        List(n -> d)
  }
//  flatMap {
//    case (n, d) =>
//      val periodicity = d.checkPeriodicity
//      if (periodicity.max > 80) // min 80% of similarity between 2 periods
//        
//  }
//  val norm2 = BinData(norm1.tail)
//  
//    (analyse(s"${filename}/biphase1")_).tupled(norm.biphase(true)) ++
//      (analyse(s"${filename}/biphase2")_).tupled(norm2.biphase(true)) ++
//      (analyse(s"${filename}/manchester1")_).tupled(norm.manchester(false)) ++
//      (analyse(s"${filename}/manchester2")_).tupled(norm2.manchester(false))
}
object Main extends App {

  def analyse(name: String)(errors: Int, data: BinData) = {
    val ret = new MapBuilder[String, (BinData, RawIntData, RawIntData), Map[String, (BinData, RawIntData, RawIntData)]](Map.empty[String, (BinData, RawIntData, RawIntData)])
    if (errors > data.length / 10) {
      println(s"${name} : Too many errors occur (${errors}), aborting")
    } else {
      println(s"${name} : ${data}")
      val periodicity = data.checkPeriodicity
      val periodRate = periodicity.max
      if (periodRate > 80) {
        val period = periodicity.indexOf(periodRate) + 1
        val (m0, i0, m1, i1) = data.maxConsecutive
        val data0 = data.extract(period, i0)
        val data0p4 = data0.findParity(4)
        val data0p8 = data0.findParity(8)
        val data1 = data.extract(period, i1)
        val data1p4 = data1.findParity(4)
        val data1p8 = data1.findParity(8)
        ret += s"${name}/0" -> (data0, data0p4, data0p8)
        ret += s"${name}/1" -> (data1, data1p4, data1p8)
        println(s"${name} : found periodicity (${periodRate}%) at ${period} (${errors} errors)")
        println(s"${name}/0 : Found ${m0} zeros at ${i0}, it could be an header")
        println(s"${name}/0 : ${data0}")
        println(s"${name}/0/p4 : ${data0p4}")
        println(s"${name}/0/p8 : ${data0p8}")
        println(s"${name}/1 : Found ${m1} ones at ${i1}, it could be an header")
        println(s"${name}/1 : ${data1}")
        println(s"${name}/1/p4 : ${data1p4}")
        println(s"${name}/1/p8 : ${data1p8}")
      } else {
        println(s"${name} : no periodicity found (max ${periodRate}%)  (${errors} errors)")
      }
    }
    ret.result
  }

  def getData(filename: String) = {
    val raw = RawIntData(Source.fromFile(s"traces/${filename}")(Codec.UTF8))
    val ask = raw.ask
    val norm = ask.normalise
    val norm2 = BinData(norm.tail)
    (analyse(s"${filename}/biphase1")_).tupled(norm.biphase(true)) ++
      (analyse(s"${filename}/biphase2")_).tupled(norm2.biphase(true)) ++
      (analyse(s"${filename}/manchester1")_).tupled(norm.manchester(false)) ++
      (analyse(s"${filename}/manchester2")_).tupled(norm2.manchester(false))
  }

  
  override def main(args: Array[String]): Unit = {
    val data = new java.io.File("traces").listFiles.par.map(f => getData(f.getName)).reduce(_ ++ _)
    for ((k, v) <- data)
      println(s"${k},${v}")
  }

  def debug = {
    val raw = RawIntData(Source.fromFile("traces/a.data")(Codec.UTF8))
    val index = (0 to raw.length).map(b => f"$b%4d").mkString(",")
    println(s"idx =${index}")
    println(s"raw (${raw.length}) =${raw}")

    val ask = raw.ask
    println(s"ask (${ask.length}) =${ask}")

    val clock = ask.clock
    println(s"clock=${clock}")

    val edges = ask.edges.map(b => f"$b%4d").mkString(",")
    println(s"edge (${ask.edges.length}) =${edges}")

    val norm = ask.normalise
    val norm2 = BinData(norm.tail)
    println(s"norm (${norm.length}) =${norm}")
    println(s"norm2 (${norm2.length}) =${norm2}")
    (analyse("biphase1")_).tupled(norm.biphase(true))
    (analyse("biphase2")_).tupled(norm2.biphase(true))
    (analyse("manchester1")_).tupled(norm.manchester(false))
    (analyse("manchester2")_).tupled(norm2.manchester(false))
  }

}