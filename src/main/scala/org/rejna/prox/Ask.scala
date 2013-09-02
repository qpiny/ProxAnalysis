package org.rejna.prox

class ByteBuffer extends Seq[Byte]
class BitBuffer extends Seq[Boolean]

class Ask extends Function1[Seq[Byte], Seq[Boolean]] {
  val bit0 = false
  val bit1 = true
  def apply(buff: ByteBuffer) = {
    val max = buff.max
    val min = buff.min
    (Seq(buff.head > 0) /: buff)((ret, curr) => {
      val prev = ret.last
      if (curr == max && prev == bit0) ret :+ bit1
      else if (curr == min && prev == bit1) ret :+ bit0
      else ret :+ prev
    })
  }
}