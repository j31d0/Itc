package itc

import scala.util.parsing.combinator.*

enum Expr:
  case Type extends Expr
  case Prop extends Expr
  case Id(name: String) extends Expr
  case App(f: Expr, a: Expr) extends Expr
  case Fun(x: String, t: Expr, b: Expr) extends Expr
  case Univ(x: String, t: Expr, b: Expr) extends Expr
  case Int(n: BigInt) extends Expr
  case Bool(b: Boolean) extends Expr
  case IntT extends Expr
  case BooleanT extends Expr

  override def toString: String = this match {
    case Type      => "Type"
    case Prop      => "Prop"
    case Id(name)  => name
    case App(f, a) => s"${f.atomic} ${a.atomic}"
    case Fun(x, t, b) =>
      b.nestFun match {
        case (nts, b) =>
          s"fun ${((x, t) :: nts).map((v) => s"(${v._1}: ${v._2})").mkString(" ")} => $b"
      }
    case Univ(x, t, b) =>
      if (b.frees contains x)
        b.nestUniv match {
          case (nts, b) =>
            s"forall ${((x, t) :: nts).map((v) => s"(${v._1}: ${v._2})").mkString(" ")}, $b"
        }
      else s"${t.atomic} -> $b"
    case Int(n)   => n.toString
    case Bool(b)  => b.toString
    case IntT     => "Int"
    case BooleanT => "Boolean"
  }

  def nestFun: (List[(String, Expr)], Expr) = this match {
    case Fun(x, t, b) =>
      val (l, b2) = b.nestFun
      ((x, t) :: l, b2)
    case _ => (Nil, this)
  }
  def nestUniv: (List[(String, Expr)], Expr) = this match {
    case Univ(x, t, b) =>
      if (b.frees contains x)
        val (l, b2) = b.nestUniv
        ((x, t) :: l, b2)
      else (Nil, this)
    case _ => (Nil, this)
  }

  def atomic: String = this match {
    case App(_, _) | Fun(_, _, _) | Univ(_, _, _) => s"(${this.toString})"
    case _                                        => this.toString
  }

  def alpha(x: String, y: String): Expr = this match {
    case App(t1, t2) => App(t1.alpha(x, y), t2.alpha(x, y))
    case Fun(z, k, b) =>
      if (z == x) then Fun(z, k.alpha(x, y), b)
      else Fun(z, k.alpha(x, y), b.alpha(x, y))
    case Univ(z, k, b) =>
      if (z == x) then Univ(z, k.alpha(x, y), b)
      else Univ(z, k.alpha(x, y), b.alpha(x, y))
    case Id(name) => if (name == x) then Id(y) else this
    case IntT | BooleanT | Int(_) | Bool(_) | Prop | Type => this
  }

  def frees: Set[String] = this match {
    case App(t1, t2)   => t1.frees ++ t2.frees
    case Fun(x, k, b)  => (b.frees - x) ++ k.frees
    case Univ(x, k, b) => (b.frees - x) ++ k.frees
    case Id(name)      => Set(name)
    case IntT | BooleanT | Int(_) | Bool(_) | Prop | Type => Set()
  }

  def bindings: Set[String] = this match {
    case App(t1, t2)   => t1.bindings ++ t2.bindings
    case Fun(x, k, b)  => k.bindings ++ b.bindings + x
    case Univ(x, k, b) => k.bindings ++ b.bindings + x
    case Id(name)      => Set()
    case IntT | BooleanT | Int(_) | Bool(_) | Prop | Type => Set()
  }

  def toHole: HoleExpr = this match
    case Id(name)      => HoleExpr.Id(name)
    case Int(n)        => HoleExpr.Int(n)
    case Bool(b)       => HoleExpr.Bool(b)
    case App(f, a)     => HoleExpr.App(f.toHole, a.toHole)
    case Fun(x, t, b)  => HoleExpr.Fun(x, t, b.toHole)
    case Univ(x, t, b) => HoleExpr.Univ(x, t, b.toHole)
    case IntT          => HoleExpr.IntT
    case BooleanT      => HoleExpr.BooleanT
    case Prop          => HoleExpr.Prop
    case Type          => HoleExpr.Type

end Expr
object Expr:
  def apply(str: String): Expr = Parser
    .parseAll(Parser.eparser, str)
    .getOrElse(Parser.error("Expr parse fail"))
end Expr

enum HoleExpr:
  case Type extends HoleExpr
  case Prop extends HoleExpr
  case Id(name: String) extends HoleExpr
  case App(f: HoleExpr, a: HoleExpr) extends HoleExpr
  case Fun(x: String, t: Expr, b: HoleExpr) extends HoleExpr
  case Univ(x: String, t: Expr, b: HoleExpr) extends HoleExpr
  case Int(n: BigInt) extends HoleExpr
  case Bool(b: Boolean) extends HoleExpr
  case IntT extends HoleExpr
  case BooleanT extends HoleExpr
  case Hole(target: scala.Int, t: Expr) extends HoleExpr

  override def toString: String = this match {
    case Type          => "Type"
    case Prop          => "Prop"
    case Id(name)      => name
    case App(f, a)     => s"($f) ($a)"
    case Fun(x, t, b)  => s"fun ($x: $t) => $b"
    case Univ(x, t, b) => s"forall ($x: $t), $b"
    case Int(n)        => n.toString
    case Bool(b)       => b.toString
    case IntT          => "Int"
    case BooleanT      => "Boolean"
    case Hole(t, ty)   => s"[[goal $t: $ty]]"
  }

  def replace(n: scala.Int, e: HoleExpr): HoleExpr = this match
    case Hole(t, ty) if t == n => e
    case App(f, a)             => App(f.replace(n, e), a.replace(n, e))
    case Fun(x, t, b)          => Fun(x, t, b.replace(n, e))
    case Univ(x, t, b)         => Univ(x, t, b.replace(n, e))
    case _                     => this

  def freeTargets: Set[scala.Int] = this match
    case Hole(t, ty)   => Set(t)
    case App(f, a)     => f.freeTargets ++ a.freeTargets
    case Fun(_, _, b)  => b.freeTargets
    case Univ(_, _, b) => b.freeTargets
    case _             => Set()

  def freshTargets(n: scala.Int): List[scala.Int] =
    (freeTargets.max + 1 to freeTargets.max + n).toList

  def toExpr: Option[Expr] = this match
    case Id(name) => Some(Expr.Id(name))
    case Int(n)   => Some(Expr.Int(n))
    case Bool(b)  => Some(Expr.Bool(b))
    case App(f, a) =>
      for { fv <- f.toExpr; av <- a.toExpr } yield Expr.App(fv, av)
    case Fun(x, t, b)  => for { bv <- b.toExpr } yield Expr.Fun(x, t, bv)
    case Univ(x, t, b) => for { bv <- b.toExpr } yield Expr.Univ(x, t, bv)
    case Type          => Some(Expr.Type)
    case Prop          => Some(Expr.Prop)
    case IntT          => Some(Expr.IntT)
    case BooleanT      => Some(Expr.BooleanT)
    case Hole(t, ty)   => None

end HoleExpr
