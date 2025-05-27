package dev.capslock.lexorank4s

opaque type LexoRank = String
object LexoRank {
  def apply(value: String): LexoRank = value
  def apply(bucket: Short, rank: String): LexoRank = s"$bucket|$rank"
  def fromBigInt(bucket: Short, rank: BigInt): LexoRank =
    require(rank >= 0, "Rank must be non-negative") // ensure rank is non-negative
    s"$bucket|${rank.toString(RADIX)}"

  val DELIMITER: Char = '|'
  val RADIX: Int = 36
  val HALF_RADIX_VALUE: String = BigInt(RADIX / 2).toString(LexoRank.RADIX)
  val initial: LexoRank = "0|i"

  def fromString(value: String): LexoRank = value

  def toString(rank: LexoRank): String = rank

  given Ordering[LexoRank] = new Ordering[LexoRank] {
    override def compare(x: LexoRank, y: LexoRank): Int = x.compareTo(y)
  }

  extension (lexorank: LexoRank) {
    def bucket: Short = {
      s"${lexorank.head}".toShort
    }
    def rank: String = {
      lexorank.drop(2)
    }
    infix def between(other: LexoRank): LexoRank = {
      val myBucket = lexorank.bucket
      val otherBucket = other.bucket
      if (myBucket != otherBucket) {
        throw new IllegalArgumentException(
          s"Cannot compute between ranks in different buckets: $lexorank and $other"
        )
      }
      val myRank = lexorank.rank
      val otherRank = other.rank
      val (l, r) = if (myRank < otherRank) (myRank, otherRank) else (otherRank, myRank)
      val maxLength = math.max(l.length, r.length)
      // to keep corresponding BigInt sane, pad rank.
      val lp = l.padTo(maxLength, '0')
      val rp = r.padTo(maxLength, '0')

      val lb = BigInt(lp, LexoRank.RADIX)
      val rb = BigInt(rp, LexoRank.RADIX)

      val diff = rb - lb

      diff match {
        case d if d == BigInt(0) => r // no difference, return self
        case d if d == BigInt(1) => // we should grow length
          val newRank = l + "i"
          LexoRank(myBucket, newRank)
        case _ =>
          val newRankBI = lb + diff / 2

          val result = LexoRank.fromBigInt(myBucket, newRankBI)

          // especially when the left rank is very short, we need to pad the result
          if (result.rank.length < lexorank.rank.length) {
            // left pad.
            val paddedRank = result.rank.view.reverse.padTo(lexorank.rank.length, '0').mkString.reverse
            LexoRank(myBucket, paddedRank)
          } else result
      }
    }
  }
}
