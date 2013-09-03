package org.rejna.prox

import scala.io.{ Source, Codec }
import scala.collection.mutable.MapBuilder
import scala.collection.immutable.Map

object Main extends App {

  def analyse(name: String)(errors: Int, data: BinData) = {
    val ret = new MapBuilder[String, BinData, Map[String, BinData]](Map.empty[String, BinData])
    if (errors > data.length / 10) {
      println(s"${name} : Too many errors occur (${errors}), aborting")
    } else {
      println(s"${name} : ${data}")
      val period = data.checkPeriodicity()
      println(s"${name}/periodicity : ${period.map(b => f"$b%4d").mkString(",")}")
      val max = period.max
      if (max > 80) {
        val index = period.indexOf(max) + 1
        println(s"${name} : found periodicity (${max}%) at ${index} (${errors} errors)")
        val (m0, i0, m1, i1) = data.maxConsecutive
        ret += s"${name}/0" -> BinData(data.drop(i0).take(max))
        ret += s"${name}/1" -> BinData(data.drop(i1).take(max))
        //        println(s"${name}/0 : Found ${m0} zeros at ${i0}, it could be an header")
        //        println(s"${name}/0 : ${BinData(data.drop(i0).take(max))}")
        //        println(s"${name}/1 : Found ${m1} ones at ${i1}, it could be an header")
        //        println(s"${name}/1 : ${BinData(data.drop(i1).take(max))}")
      } else {
        println(s"${name} : no periodicity found (max ${max}%)  (${errors} errors)")
      }
    }
    ret.result
  }

  def getData(filename: String) = {
    val raw = RawByteData(Source.fromFile(s"traces/${filename}")(Codec.UTF8))
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
      println("${k},${v}")
  }

  def debug = {
    val raw = RawByteData(Source.fromFile("traces/a.data")(Codec.UTF8))
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