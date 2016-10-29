package com.pourtois.symbolette.expr

/**
  * Created by manpo on 17/10/2016.
  * a kitchen table symbolic calculator
  */
trait Expr extends Ordered[Expr] {


  def +(b: Expr) = Plus(this, b)

  def -(b: Expr) = Minus(this, b)

  def *(b: Expr) = Prod(this, b)

  def /(b: Expr) = Divide(this, b)

  def unary_-() = Negate(this)

  def optimize: Expr = this

  def derivative(d: Expr): Expr

  val optimized: Seq[Expr]
  val cost: Int
  val valueHash :Long

  lazy val best = stableBest(optimized.head, 10)


  def stableBest(e: Expr, maxHop: Int): Expr = {
    if (maxHop == 0) {
      println("Warning unstable optimum " + e)
      e
    }
    else {
      val next = e.optimized.head
      if (e == next) e else stableBest(next, maxHop - 1)
    }
  }

  import scala.math.Ordered.orderingToOrdered

  def compare(that: Expr): Int = {
    val c = this.cost compare that.cost
    if (c == 0) {
      if (this == that) 0 else (this.hashCode - that.hashCode)
    } else c


  }


}

trait Constant extends Expr {
  def derivative(d: Expr) = Expr.ZERO

  lazy val optimized = List(this)
}

trait Transcendant extends Constant {
  val cost = 1
}


object Expr {
  def symbol(name: String, depend: List[Symbol] = Nil, derivate: List[Symbol] = Nil) =  Symbol(name, depend, derivate)

  val ONE = Integer(1)
  val TWO = Integer(2)
  val ZERO = Integer(0)

  def exp(a: Expr) = Exp(a)

  val PI = new Transcendant {}

  def bestOrder(l: Seq[Expr]): Seq[Expr] = l.sortWith((e1, e2) => e1.cost < e2.cost)

  implicit def intToExpr(i: Int) = Integer(i)


  def equals(l1: List[Expr], l2: List[Expr]): Boolean = {
    val l1s = l1.sorted
    val l2s = l2.sorted
    l1s.equals(l2s)
  }

  implicit def tupleToIndiceDef(tuple: (Indice,Indice)) = IndiceDef(tuple._1,Some(tuple._2))
  implicit def indiceToIndiceDef(indice:Indice) = IndiceDef(indice,None)

  implicit def indiceToExpr(indice:Indice) = indice.symbol


}
trait Operator extends Expr {
}
case class Derivative(of:Expr, by : Expr) extends Operator {

  //val optimOf = of.optimized

  lazy val optimized = of.derivative(by).optimized

  override def derivative(d: Expr): Expr = of.derivative(by).derivative(d)

  override val cost: Int = 6
}


case class Integer(i: Int) extends Constant {
  val cost = if (i == 0) 0 else 1
}


/*
case class Exp(a :Expr) extends Func(a) {
  override def derivative(d: Expr): Expr = a.derivative(d)*Exp(a)

}

abstract class Func(val a: Expr)extends  Expr {
  override def derivative(d: Expr): Expr = ???

}*/