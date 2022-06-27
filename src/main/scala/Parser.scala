package itc

import Expr.*
import Type.*
import Command.*
import scala.util.parsing.combinator.RegexParsers

case class ParsingError(msg: String) extends Exception

object Parser extends RegexParsers:
    def error(msg: String): Nothing = throw ParsingError(msg)

    private def wrapR[T](e: => Parser[T]): Parser[T] = "(" ~> e <~ ")"


    lazy val keywords = Set(
      "true", "false", "fun"
    )
    lazy val tKeywords = Set("Int", "Boolean")

    private lazy val n: Parser[BigInt] = "-?[0-9]+".r ^^ BigInt.apply
    private lazy val b: Parser[Boolean] = "true" ^^^ true | "false" ^^^ false
    private lazy val x: Parser[String] =
      "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!keywords(_))
    private lazy val tx: Parser[String] =
      "[a-zA-Z_][a-zA-Z0-9_]*".r.withFilter(!tKeywords(_))

    private lazy val eid: Parser[Id] = x ^^ Id.apply
    private lazy val eint: Parser[Int] = n ^^ Int.apply
    private lazy val ebool: Parser[Bool] = b ^^ Bool.apply
    private lazy val efun: Parser[Fun] = (("fun" ~> wrapR((x <~ ":") ~ tparser) <~ "=>") ~ eparser) ^^ { case (name ~ ty) ~ b => Fun(name, ty, b)}

    private lazy val e0: Parser[Expr] =
        eid |
        eint |
        ebool |
        efun |
        wrapR(eparser)

    private lazy val e1: Parser[Expr] =
        rep1(e0) ^^ {
            case es => es.reduceLeft(App.apply)
        }
    lazy val eparser: Parser[Expr] = e1


    private lazy val tint: Parser[Type.IntT.type] = "Int" ^^^ Type.IntT
    private lazy val tboolean: Parser[Type.BooleanT.type] = "Boolean" ^^^ Type.BooleanT

    private lazy val t0: Parser[Type] =
        tint |
        tboolean |
        wrapR(tparser)

    private lazy val t1: Parser[Type] =
        rep1sep(t0, "->") ^^ {
            case t0 :: Nil => t0
            case ts => ts.reduceRight(Type.ArrowT.apply)
        }
    lazy val tparser: Parser[Type] = t1


    private lazy val clet: Parser[Let] = ("Definition" ~> x <~ ":=") ~ (eparser <~ ".") ^^ { case (s ~ e) => Let(s, e) }
    private lazy val cdefine: Parser[Definition] = ("Definition" ~> x <~ ":") ~ (tparser <~ ".") ^^ { case (s ~ ty) => Definition(s, ty) }
    private lazy val cprint: Parser[Print] = ("Print" ~> x <~ ".") ^^ Print.apply

    lazy val cparser: Parser[Command] = clet | cdefine | cprint

    private lazy val tacintro: Parser[Tactic.Intro] = ("intro" ~> x <~ ".") ^^ Tactic.Intro.apply
    private lazy val tacapply: Parser[Tactic.Apply] = ("apply" ~> eparser <~ ".") ^^ Tactic.Apply.apply
    private lazy val tacdefined : Parser[Tactic.Defined.type] = "Defined" <~ "." ^^^ Tactic.Defined

    lazy val tacparser: Parser[Tactic] = tacintro | tacapply | tacdefined
end Parser
