package com.pourtois.symbolette.expr

import Expr._

import scala.collection.mutable.ListBuffer

/**
  * Created by manu on 23.10.16.
  */
trait BinaryOperator extends Expr {
  val op1: Expr
  val op2: Expr

  def build(op1: Expr, op2: Expr): BinaryOperator

  lazy val optimized: List[Expr] = {
    // quiet : Dragons are living here
    val optOp2 = op2.optimized

    val optimizedBinaries : Seq[BinaryOperator] = op1.optimized.flatMap(o1 => optOp2.map(o2 => this.build(o1,o2)))
    BinaryOperator.rules.flatMap(optimizedBinaries.collect(_)).sorted.distinct
 }

}

object BinaryOperator {

  type Rule = PartialFunction[BinaryOperator, Expr]

  val rules: List[Rule] = {
    val buffer = new ListBuffer[Rule]()
    def rule(r: Rule): Unit = {
      buffer += r
    }

    rule({ case oper : BinaryOperator => oper }) // Maybe this is the best optimization


    rule({ case Plus(a, b) => b + a })
    rule({ case Plus(a, Negate(b)) => a - b })
    rule({ case Plus(a, b) => b + a })
    rule({ case Plus(Prod(Integer(c),a), b) if a == b => Integer(c+1)*a })
    rule({ case Plus(a, Negate(b)) => a - b })
    rule({ case Plus(Negate(a), Negate(b)) => -(a + b) })

    rule({ case Plus(Prod(a,b), Prod(c,d) ) if a == c => a*(b+d) })
    rule({ case Plus(Prod(a,b), Prod(c,d) ) if a == d => a*(b+c) })

    rule({ case Prod(a, b) => b * a })
    rule({ case Prod(Negate(a), b) => -(a * b) })
    rule({ case Prod(a, Negate(b)) => -(a * b) })
    rule({ case Prod(Negate(a), Negate(b)) => a * b })
    rule({ case Plus(a, ZERO) => a })
    rule({ case Minus(a, ZERO) => a })
    rule({ case Minus(a, b) => -(b - a) })
    rule({ case Minus(a, b) if a == b => ZERO })
    rule({ case Minus(Prod(Integer(c),a), b) if a == b => Integer(c-1)*a })

    rule({ case Divide(a, b) if a == b => ONE })

    rule({ case Plus(a, b) if a == b => TWO * a })
    rule({ case Plus(Integer(a), Integer(b)) => Integer(a + b) })
    rule({ case Minus(Integer(a), Integer(b)) => Integer(a - b) })
    rule({ case Prod(Integer(a), Integer(b)) => Integer(a * b) })
    rule({ case Prod(a, ONE) => a })
    rule({ case Prod(ONE, a) => a })
    rule({ case Divide(a, ONE) => a })
    rule({ case Prod(a, ZERO) => ZERO })
    rule({ case Prod(ZERO, a) => ZERO })
    rule({ case Plus(Plus(a: Expr, b: Expr), c: Expr) => a + (b + c) })
    rule({ case Plus(Plus(a: Expr, b: Expr), c: Expr) => b + (a + c) })
    rule({ case Plus(a: Expr, Plus(b: Expr, c: Expr)) => (a + b) + c })
    rule({ case Plus(a: Expr, Plus(b: Expr, c: Expr)) => (a + c) + b })
    rule({ case Prod(Exp(a),Exp(b)) => exp(a+b) })


    buffer.toList
  }



}

case class Plus(op1: Expr, op2: Expr) extends BinaryOperator {
  def build(op1: Expr, op2: Expr) = Plus(op1, op2)

  def derivative(d: Expr) = op1.derivative(d) + op2.derivative(d)

  override def equals(that: Any): Boolean = that match {
      case a: Plus => Plus.equals(this,a)
      case _ => false
    }
  val cost = 4 + op1.cost +op2.cost
}

object Plus {

  def equals(a: Plus , b:Plus ) :Boolean = Expr.equals(allPlus(a,Nil),allPlus(b,Nil))

  def allPlus(p : Plus, l:List[Expr] ): List[Expr] = {
    p match {
      case Plus(a:Plus,b:Plus) => allPlus(a,allPlus(b,l))
      case Plus(a:Plus,b) => b::allPlus(a,l)
      case Plus(a,b:Plus) => a::allPlus(b,l)
      case Plus(a,b) => a::b::l
      case _ => ???

    }

  }
}

case class Minus(op1: Expr, op2: Expr) extends BinaryOperator {
  def build(op1: Expr, op2: Expr) = Minus(op1, op2)

  def derivative(d: Expr) = op1.derivative(d) - op2.derivative(d)
  val cost = 4 + op1.cost +op2.cost

}

case class Prod(op1: Expr, op2: Expr) extends BinaryOperator {
  def build(op1: Expr, op2: Expr) = Prod(op1, op2)

  def derivative(d: Expr) = (op1.derivative(d)) * op2 + op1 * (op2.derivative(d))
  val cost = 5 + op1.cost +op2.cost

  override def equals(that: Any): Boolean = that match {
    case Prod(a,b) => {
      val left = op1 == a && op2 == b
      val right = op1 == b && op2 == a
      left || right
    }
    case _ => false
  }
}

case class Divide(op1: Expr, op2: Expr) extends BinaryOperator {
  def build(op1: Expr, op2: Expr) = Divide(op1, op2)

  def derivative(d: Expr) = (op1.derivative(d) * op2 - op1 * op2.derivative(d)) / (op2 * op2)
  val cost = 6 + op1.cost +op2.cost

}
