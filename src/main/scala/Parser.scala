package itc

import Expr.*
import Type.*
import Kind.*
import Command.*
import scala.util.parsing.combinator.RegexParsers

case class ParsingError(msg: String) extends Exception

object Parser extends RegexParsers:
  def error(msg: String): Nothing = throw ParsingError(msg)

  private def wrapR[T](e: => Parser[T]): Parser[T] = "(" ~> e <~ ")"
  private def wrapS[T](e: => Parser[T]): Parser[T] = "[" ~> e <~ "]"

  lazy val keywords = Set(
    "true",
    "false",
    "fun"
  )
  lazy val tKeywords = Set("Int", "Boolean", "Type", "forall", "fun")

  private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply
  private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false
  private lazy val x: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))
  private lazy val tx: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!tKeywords(_))

  private lazy val eid: Parser[Id] = x ^^ Id.apply
  private lazy val eint: Parser[Int] = n ^^ Int.apply
  private lazy val ebool: Parser[Bool] = b ^^ Bool.apply
  private lazy val efun: Parser[Fun] =
    (("fun" ~> wrapR((x <~ ":") ~ tparser) <~ "=>") ~ eparser) ^^ {
      case (name ~ ty) ~ b => Fun(name, ty, b)
    }
  private lazy val etfun: Parser[TFun] =
    (("fun" ~> wrapR((tx <~ ":") ~ kparser) <~ "=>") ~ eparser) ^^ {
      case (name ~ kind) ~ b => TFun(name, kind, b)
    }

  private lazy val e0: Parser[Expr] =
    eid |
      eint |
      ebool |
      efun |
      etfun |
      wrapR(eparser)

  private lazy val aa: Parser[AppArg] =
    wrapS(tparser) ^^ AppArg.T.apply |
    e0 ^^ AppArg.E.apply

  enum AppArg:
    case T(t: Type) extends AppArg
    case E(e: Expr) extends AppArg

  private lazy val e2: Parser[Expr] =
    (e0 ~ rep(aa)) ^^ { case (eb ~ as) =>
      as.foldLeft(eb) {
        case (e, AppArg.T(t)) => TApp(e, t)
        case (e, AppArg.E(e2)) => App(e, e2)
      }
    }
  lazy val eparser: Parser[Expr] = e2

  private lazy val tint: Parser[IntT.type] = "Int" ^^^ IntT
  private lazy val tboolean: Parser[BooleanT.type] = "Boolean" ^^^ BooleanT
  private lazy val tid: Parser[IdT] = tx ^^ IdT.apply
  private lazy val tuniv: Parser[UnivT] =
    ("forall" ~> wrapR((tx <~ ":") ~ kparser) <~ ",") ~ tparser ^^ { case (x ~ k) ~ t =>
      UnivT(x, k, t)
    }
  private lazy val tfun: Parser[FunT] =
    (("fun" ~> wrapR((tx <~ ":") ~ kparser) <~ "=>") ~ tparser) ^^ {
      case (name ~ kind) ~ b => FunT(name, kind, b)
    }

  private lazy val t0: Parser[Type] =
      tid |
      tint |
      tboolean |
      tuniv |
      tfun |
      wrapR(tparser)

  private lazy val t1: Parser[Type] =
    rep1(t0) ^^ { case ts => ts.reduceLeft(Type.AppT.apply) }

  private lazy val t2: Parser[Type] =
    rep1sep(t1, "->") ^^ {
      case t0 :: Nil => t0
      case ts        => ts.reduceRight(Type.ArrowT.apply)
    }
  lazy val tparser: Parser[Type] = t2


  private lazy val kprop: Parser[Kind.ProperK.type] = "Type" ^^^ ProperK

  private lazy val k0: Parser[Kind] =
    kprop |
    wrapR(kparser)
  private lazy val k1: Parser[Kind] =
    rep1sep(k0, "->") ^^ {
      case k0 :: Nil => k0
      case ks => ks.reduceRight(Kind.ArrowK.apply)
    }

  lazy val kparser: Parser[Kind] = k1

  private lazy val clet: Parser[Let] =
    ("Definition" ~> x <~ ":=") ~ (eparser <~ ".") ^^ { case (s ~ e) =>
      Let(s, e)
    }
  private lazy val clettype: Parser[LetType] =
    ("Definition" ~> x <~ ":") ~ (kparser <~ ":=") ~ (tparser <~ ".") ^^ { case s ~ k ~ t =>
      LetType(s, k, t)
    }
  private lazy val cdefine: Parser[Definition] =
    ("Definition" ~> x <~ ":") ~ (tparser <~ ".") ^^ { case (s ~ ty) =>
      Definition(s, ty)
    }
  private lazy val cprint: Parser[Print] = ("Print" ~> x <~ ".") ^^ Print.apply

  lazy val cparser: Parser[Command] = clet | clettype | cdefine | cprint

  private lazy val tacintro: Parser[Tactic.Intro] =
    ("intro" ~> x <~ ".") ^^ Tactic.Intro.apply
  private lazy val tacapply: Parser[Tactic.Apply] =
    ("apply" ~> eparser <~ ".") ^^ Tactic.Apply.apply
  private lazy val tacunfold: Parser[Tactic.Unfold] =
    ("unfold" ~> tx) ~ opt("in" ~> tx) <~ "." ^^ { case x ~ h => Tactic.Unfold(x, h)}
  private lazy val taccbv: Parser[Tactic.Cbv] =
    "cbv" ~> opt("in" ~> tx) <~ "." ^^ Tactic.Cbv.apply
  private lazy val tacdefined: Parser[Tactic.Defined.type] =
    "Defined" <~ "." ^^^ Tactic.Defined

  lazy val tacparser: Parser[Tactic] = tacintro | tacapply | tacunfold | taccbv | tacdefined
end Parser
