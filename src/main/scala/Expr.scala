package itc

import scala.util.parsing.combinator.*

enum Kind:
    case ProperK extends Kind
    case ArrowK(p: Kind, r: Kind) extends Kind

    override def toString: String = this match {
        case ProperK => "Type"
        case ArrowK(p, r) => s"($p -> $r)"
    }
end Kind

enum Type:
    case FunT(x: String, k: Kind, b: Type) extends Type
    case AppT(t1: Type, t2: Type) extends Type
    case UnivT(x: String, k: Kind, b: Type) extends Type
    case IdT(name: String) extends Type
    case ArrowT(p: Type, r: Type) extends Type
    case IntT extends Type
    case BooleanT extends Type

    override def toString: String = this match {
        case AppT(t1, t2) => s"($t1) ($t2)"
        case FunT(x, k, b) => s"fun ($x: $k) => $b"
        case UnivT(x, k, b) => s"forall ($x: $k), $b"
        case IdT(name) => s"$name"
        case ArrowT(p, r) => s"($p -> $r)"
        case IntT => "Int"
        case BooleanT => "Boolean"
    }

    def alpha(x: String, y: String): Type = this match {
        case AppT(t1, t2) => AppT(t1.alpha(x, y), t2.alpha(x, y))
        case FunT(z, k, b) => if (z == x) then this else FunT(z, k, b.alpha(x, y))
        case UnivT(z, k, b) => if (z == x) then this else UnivT(z, k, b.alpha(x, y))
        case IdT(name) => if (name == x) then IdT(y) else this
        case ArrowT(p, r) => ArrowT(p.alpha(x, y), r.alpha(x, y))
        case IntT => this
        case BooleanT => this
    }

    def frees: Set[String] = this match {
        case AppT(t1, t2) => t1.frees ++ t2.frees
        case FunT(x, _, b) => b.frees - x
        case UnivT(x, _, b) => b.frees - x
        case IdT(name) => Set(name)
        case ArrowT(p, r) => p.frees ++ r.frees
        case IntT => Set()
        case BooleanT => Set()
    }

end Type
object Type:
    def apply(str: String): Type = Parser.parseAll(Parser.tparser, str).getOrElse(Parser.error("Type parse fail"))
end Type

enum Expr:
    case Id(name: String) extends Expr
    case Int(n: BigInt) extends Expr
    case Bool(b: Boolean) extends Expr
    case App(f: Expr, a: Expr) extends Expr
    case Fun(x: String, t: Type, b: Expr) extends Expr
    case TApp(f: Expr, a: Type) extends Expr
    case TFun(x: String, k: Kind, b: Expr) extends Expr

    override def toString: String = this match {
        case Id(name) => name
        case Int(n) => n.toString
        case Bool(b) => b.toString
        case App(f, a) => s"($f) ($a)"
        case Fun(x, t, b) => s"fun ($x: $t) => $b"
        case TApp(f, a) => s"($f) [$a]"
        case TFun(x, k, b) => s"fun ($x: $k) => $b"
    }

    def toHole: HoleExpr = this match
        case Id(name) => HoleExpr.Id(name)
        case Int(n) => HoleExpr.Int(n)
        case Bool(b) => HoleExpr.Bool(b)
        case App(f, a) => HoleExpr.App(f.toHole, a.toHole)
        case Fun(x, t, b) => HoleExpr.Fun(x, t, b.toHole)
        case TApp(f, a) => HoleExpr.TApp(f.toHole, a)
        case TFun(x, k, b) => HoleExpr.TFun(x, k, b.toHole)

end Expr
object Expr:
    def apply(str: String): Expr = Parser.parseAll(Parser.eparser, str).getOrElse(Parser.error("Expr parse fail"))
end Expr

enum HoleExpr:
    case Id(name: String) extends HoleExpr
    case Int(n: BigInt) extends HoleExpr
    case Bool(b: Boolean) extends HoleExpr
    case App(f: HoleExpr, a: HoleExpr) extends HoleExpr
    case Fun(x: String, t: Type, b: HoleExpr) extends HoleExpr
    case TApp(f: HoleExpr, t: Type) extends HoleExpr
    case TFun(x: String, k: Kind, b: HoleExpr) extends HoleExpr
    case Hole(target: scala.Int, t: Type) extends HoleExpr

    override def toString: String = this match {
        case Id(name) => name
        case Int(n) => n.toString
        case Bool(b) => b.toString
        case App(f, a) => s"($f) ($a)"
        case Fun(x, t, b) => s"fun ($x: $t) => $b"
        case TApp(f, a) => s"($f) [$a]"
        case TFun(x, k, b) => s"fun ($x: $k) => $b"
        case Hole(t, ty) => s"[[goal $t: $ty]]"
    }

    def replace(n: scala.Int, e: HoleExpr): HoleExpr = this match
        case Hole(t, ty) if t == n => e
        case App(f, a) => App(f.replace(n, e), a.replace(n, e))
        case Fun(x, t, b) => Fun(x, t, b.replace(n, e))
        case TApp(f, a) => TApp(f.replace(n, e), a)
        case TFun(x, k, b) => TFun(x, k, b.replace(n, e))
        case _ => this
    
    def frees: Set[scala.Int] = this match
        case Hole(t, _) => Set(t)
        case App(f, a) => f.frees ++ a.frees
        case Fun(_, _, b) => b.frees
        case TApp(f, _) => f.frees
        case TFun(_, _, b) => b.frees
        case _ => Set()
    
    def freshes(n: scala.Int): List[scala.Int] = (frees.max + 1 to frees.max + n).toList

    def toExpr: Option[Expr] = this match
        case Id(name) => Some(Expr.Id(name))
        case Int(n) => Some(Expr.Int(n))
        case Bool(b) => Some(Expr.Bool(b))
        case App(f, a) => for { fv <- f.toExpr; av <- a.toExpr } yield Expr.App(fv, av)
        case Fun(x, t, b) => for { bv <- b.toExpr } yield Expr.Fun(x, t, bv)
        case TApp(f, a) => for { fv <- f.toExpr } yield Expr.TApp(fv, a)
        case TFun(x, k, b) => for { bv <- b.toExpr } yield Expr.TFun(x, k, bv)
        case Hole(t, ty) => None

end HoleExpr
