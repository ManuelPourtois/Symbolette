package com.pourtois.symbolette.expr.test

import com.pourtois.symbolette.expr.Expr._
import com.pourtois.symbolette.expr.{Expr, Integer}
import org.scalatest._
import com.pourtois.symbolette.expr.Symbol

/**
  * Created by manu on 24.10.16.
  */
class TestUnary extends FunSuite {

  val y = symbol("y")
  val z = symbol("z")
  val r = symbol("r")
  val x = symbol("x")

  test("Soon your pocahontas compilable test name here") {


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

  test("another non-refactorable name") {
    val l1 = ONE::ZERO::(ONE/TWO)::PI::Nil
    val l2 = ONE::(ONE/Integer(2))::ZERO::PI::Nil
    val l3 = ONE::ZERO::TWO::Nil
    val l4 = ONE::ZERO::TWO::TWO::Nil


    val t = Expr.equals(l1,l2)
    assert (Expr.equals(l1,l2))
    assert (!Expr.equals(l3,l4))
  }

  test("") {
    assert( ((x+y)+z) == (x+(y+z)))
    assert( ((x+y)+z) == (x+(z+y)))
    assert( ((x+y)+(z+z)) == ((x+z)+(y+z)))

    val e = (x+y+x+y+x+y+x+y)

    //printOpti(e.optimized)
//    printOpti(e.optimized.flatMap(_.optimized).distinct)
    assert((x+x+x+x+x+x+x+x).best == Integer(8)*x)
  }
  test("derivate") {

    assert(x.derivative(x) == Expr.ONE)
    assert(x.derivative(y) == Expr.ZERO)

    assert(exp(x).derivative(x).best == exp(x))
    assert(exp(x*x).derivative(x).best == Expr.TWO*x*exp(x*x))

    val phi = symbol("phi",x::y::Nil)
    val phid: Symbol  = (phi.derivative(x)).asInstanceOf[Symbol]

    System.err.println("Phi ' "+phid)
    assert(exp(phi).derivative(x).best == phid*exp(phi))


  }

}