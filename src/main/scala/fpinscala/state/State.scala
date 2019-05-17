package fpinscala.state

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, r) = rng.nextInt
    if (n < 0) {
      (math.abs(n), r)
    } else {
      (n, r)
    }
  }

  /**
    * Generate Double between 0 and 1, not including 1
    */
  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    if (n == 0) {
      (n, r)
    } else {
      (1 / n.toDouble, r)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, r) = rng.nextInt
    val (d, rd) = double(r)
    ((n, d), rd)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((n, d), r) = intDouble(rng)
    ((d, n), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    ((d, d1, d2), r2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def recursiveInts(l: List[Int], count: Int, rng: RNG): (List[Int], Int, RNG) = {
      if (count <= 0) {
        (l, count, rng)
      } else {
        val (n, r) = rng.nextInt
        recursiveInts(n :: l, count - 1, r)
      }
    }
    require(count > 0, "List length should be more then 0")
    val (l, _, r) = recursiveInts(List.empty[Int], count, rng)
    (l, r)
  }
}
