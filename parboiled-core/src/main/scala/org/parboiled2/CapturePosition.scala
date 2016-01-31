package org.parboiled2

case class CapturePosition(val range: Long) extends AnyVal {
  def start = (range & 0x00000000FFFFFFFFL).toInt
  def end = (range >>> 32).toInt
  def isEmpty = range == ((-1L << 32) + (-1L))
  def isDefined = !isEmpty
  override def toString: String =
    s"CapturePosition($start, $end)"
}

case class CapturePositionString(pos: CapturePosition, string: String) {
  override def toString: String =
    s"CapturePositionString(${pos.start}, ${pos.end}, $string)"
}

object CapturePosition {
  def apply(start: Int, end: Int): CapturePosition =
    CapturePosition((end.toLong << 32) + start)

  def apply(start: Int, end: Int, str: String): CapturePositionString =
    CapturePositionString(apply(start, end), str)

  val empty = apply(-1, -1)
}
