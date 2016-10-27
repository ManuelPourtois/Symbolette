package com.pourtois.symbolette.expr
import Expr._

import scala.collection.mutable.ListBuffer

/**
  * Created by manu on 24.10.16.
  */
trait UnaryOperator extends Expr {

  val op:Expr

  def build(op: Expr): UnaryOperator

  lazy val optimized = {
      val optimizedUnaries: Seq[UnaryOperator] = op.optimized.map(this.build(_))

       UnaryOperator.rules.flatMap(optimizedUnaries.collect(_)).sorted
    }
}
object UnaryOperator {
  type Rule = PartialFunction[UnaryOperator, Expr]

  val rules: List[Rule] = {
    val buffer = new ListBuffer[Rule]()
    def rule(r: Rule): Unit = {
      buffer += r
    }

    rule({ case oper : UnaryOperator => oper }) // Maybe this is the best optimization
    rule({ case Negate(Integer(a)) => Integer(-a) })
    rule({ case Negate(Negate(a)) => a })
    rule({ case Exp( Plus(a,b)) => exp(a)*exp(b) })

    buffer.toList
  }


}

case class Negate(  op:Expr) extends UnaryOperator {
  def derivative(d:Expr) = -op.derivative(d)

  def build(op: Expr): UnaryOperator = Negate(op)

  lazy val cost = 2 + op.cost
}

case class Exp(op :Expr) extends UnaryOperator {
  def build(op: Expr): UnaryOperator = Exp(op)

  def derivative(d: Expr): Expr = op.derivative(d)*Exp(op)

  lazy val cost = 4 + op.cost
}
