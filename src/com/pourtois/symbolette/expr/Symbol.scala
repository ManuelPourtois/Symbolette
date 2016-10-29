package com.pourtois.symbolette.expr


/**
  * Created by manu on 28.10.16.
  */


trait Symbol extends Expr {

  val name: String
  val depend: List[Symbol]
  val derivate: List[Symbol]

  def dependOn(s: Expr) = depend.contains(s)

  override def derivative(d: Expr): Expr = d match {
    case sd: Symbol => if (d == this) Expr.ONE else Symbol.derivate(this, sd)
    case _ => Expr.ZERO
  }

  lazy val optimized: List[Expr] = this :: Nil

  override def toString: String = {

    derivate.foldRight(name)((s, b) => b + "," + s.name) + (depend match {
      case Nil => ""
      case _ => '(' + depend.map(_.name).mkString(",") + ')'
    })
  }

  val cost = 3
}


object Symbol {
  def apply(name: String, depend: List[Symbol] = Nil, derivate: List[Symbol] = Nil): Symbol = SymbolImpl(name, depend, derivate)


  def derivate(symbol: Symbol, d: Symbol): Expr = if (symbol.dependOn(d)) SymbolImpl(symbol.name, symbol
                                                                                                  .depend, d :: symbol
                                                                                                                .derivate)
  else Expr.ZERO

  case class SymbolImpl(name: String, depend: List[Symbol] = Nil, derivate: List[Symbol] = Nil) extends Symbol

}


case class Coordinates(val symbols: List[Symbol]) {

  def indice(name: String) = Indice(name, this)

  def sum(f: (Symbol) => Expr) = symbols.tail.foldLeft(f(symbols.head))((e,s)=> Plus(e,f(s)))

}
//case class Tensor2Elem(s1:Symbol,s2:Symbol, value:Expr)

case class Tensor2(values : Map[(Symbol,Symbol), Expr]) {
  def apply(s1:Symbol,s2:Symbol) = values.getOrElse((s1,s2), Expr.ZERO)
}

case class IndiceDefinition(map: Map[Indice, Symbol]) {
  def symbol(indice: Indice): Symbol = map(indice)
}


case class Indice(name: String, coordinates: Coordinates) {

  val symbol = IndicedSymbol(this)
}
case class IndicedSymbol(indice:Indice) extends Symbol {
  override val name: String = "x"
  override val depend: List[Symbol] = Nil
  override val derivate: List[Symbol] = Nil
}

//val defaultDefinition = IndiceDefinition(new Map())
trait UnoptimizableExpr extends Expr {
  override lazy val optimized: List[Expr] = ???

  override def derivative(d: Expr): Expr = ???

  override val cost: Int = ???
}

case class IndicedExpr( value: Expr, indices: Map[Indice,Indice]) extends UnoptimizableExpr

object IndicedExpr {
  def apply( value: Expr,indices : IndiceDef*) = {
    ???
  }



}

case class IndiceDef( from :Indice, to :Option[Indice]) {

}

