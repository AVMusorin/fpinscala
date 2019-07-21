package fpinscala.parallelism

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

import fpinscala.parallelism.Par.Par
import org.scalatest.FunSpec
import org.scalatest.Matchers

class ParSpec extends FunSpec with Matchers {
  describe("Par") {
    it("should work with map2") {
      val es: ExecutorService = Executors.newFixedThreadPool(2)
      val par1 = Par.unit(List(1, 2, 3, 4, 5))
      val par2 = Par.unit(6)

      val res: Par[List[Int]] = Par.map2(par2, par1)(_ :: _)
      res(es).get() should be(List(6, 1, 2, 3, 4, 5))
    }

    it("should work with with mapPar") {
      val es: ExecutorService = Executors.newFixedThreadPool(4)
      val in = List(1, 2, 3, 4)
      val func = (i: Int) => {
        println("Current thread: " + Thread.currentThread().getId)
        i * i
      }
      val res: Par[List[Int]] = Par.map(Par.parMap(in)(func))(100 :: _)
      res(es).get() should be (List(100, 1, 4, 9, 16))
    }

    it("should work with filterPar") {
      val es: ExecutorService = Executors.newFixedThreadPool(2)
      val in = List(1, 4, 3, 6, 8)
      val res = Par.parFilter(in)(_ % 2 == 0)
      res(es).get() should be (List(4, 6, 8))
    }

//    it("should make deadlock") {
//      val a = Par.lazyUnit(42 + 1)
//      val S = Executors.newFixedThreadPool(1)
//      println(Par.equal(S)(a, Par.fork(a)))
//    }
  }
}
