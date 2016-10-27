package com.pourtois.symbolette.expr

import Expr._

import org.scalatest._

/**
  * Created by manu on 24.10.16.
  */
class TestUnary extends FunSuite {

  test("Soon your pocahontas compilable test name here") {

    val z = symbol("z")
    val r = symbol("r")
    val x = symbol("x", r :: z :: Nil)

    val diff = -(-x)
    val opti = diff.optimized

    val best = diff.best
    println("Diff " + diff)
    printOpti(opti)
    println("Best " + best)

    assert(best == x)
    assert((-(-(-(-x)))).best == x)
    assert((-(-(-(-(-x))))).best == (-x))

  }
  test("binary") {

    val a = symbol("a")
    val b = symbol("b")
    val c = symbol("c")

    val sum = a + a

    // printOpti(sum.optimized)
    assert((a + a).best == Integer(2) * a)

    val sum4 = a + a + a + a
    //printOpti(sum4.optimized)
    check(a + a + a + a, Integer(4) * a)
    check(a + a + a - a, Integer(2) * a)
    check(a * b + a * c, a * (b + c))

    check(a * b + c * a, a * (b + c))
    check((a + 7) * b + c * (a + 7), (a + 7) * (b + c))
    check(a * 0, ZERO)
    check(a + a - a - a, ZERO)
    check(a - a, ZERO)
    check(a / a, ONE)
    check(a / a, ONE)
    assert((a * (b + c)) == (a * (b + c)))
    check((a * (b + c)) / (a * (b + c)), ONE)
    check((a * (b + c)) / ((b + c)*a), ONE)
    check((a * (c + b)) / ((b + c)*a), ONE)
    check(exp(a+c)*exp(b), exp((a+c)+b))

  }

  def check(e: Expr, expected: Expr): Unit = {
    val best = e.best
    assert(best == expected)
  }


  def printOpti(o: Seq[Expr]) = {
    println("Optimized")
    o.foreach(println(_))
  }

}