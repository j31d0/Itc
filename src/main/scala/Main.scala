package itc
import org.jline.reader
import org.jline.terminal.TerminalBuilder
import org.jline.utils.AttributedString
import reader.{
  LineReader,
  LineReaderBuilder,
  EndOfFileException,
  EOFError,
  UserInterruptException
}

import scala.Console.{MAGENTA => M, CYAN => C, YELLOW => Y, RED => R, RESET}

object Main:
  val typeSystem = "Fw"

  val name = s"Itc_${typeSystem}"
  def prompt(pname: String) = s"\n$M$pname>$RESET "
  val newLinePrompt = " " * (name.length + 2)

  def main(args: Array[String]): Unit =
    val terminal = TerminalBuilder.builder.dumb(false).build()
    val reader = LineReaderBuilder.builder
      .terminal(terminal)
      // .highlighter(Highlighter)
      // .parser(Parser)
      .variable(LineReader.SECONDARY_PROMPT_PATTERN, "%M")
      .variable(LineReader.HISTORY_FILE, s".${name.toLowerCase}_history")
      .option(LineReader.Option.DISABLE_EVENT_EXPANSION, true)
      .build()
    def strs(pname: String): LazyList[String] = (
      try {
        reader.readLine(prompt(pname))
      } catch {
        case _: EndOfFileException | _: UserInterruptException => ":q"
      }
    ) #:: strs(pname)

    println(s"Welcome to the $M$name$RESET REPL.")
    println(s"Type in :q, :quit, or the EOF character to terminate the REPL.")

    var globalEnv: Map[String, Expr] = Map()
    var globalTenv: Map[String, Type] = Map()

    var globalTTenv: Map[String, Type] = Map()
    var globalTKenv: Map[String, Kind] = Map()
    for (str <- strs(name).takeWhile(s => !eof(s)) if str.trim.nonEmpty) {
      val opt = lift {
        val cmd = Command(str)
        // println(s"  ${C}Parsed:$RESET $cmd")
        cmd
      }

      opt.foreach(cmd =>
        cmd match {
          case Command.Let(s, e) =>
            if (globalTenv contains s) || (globalTKenv contains s) then
              println(s" $s already exists.")
            else
              TypeCheck(globalTKenv, globalTenv, e) match {
                case None => println("TypeCheck fail")
                case Some(t) => {
                  globalEnv += (s -> e)
                  globalTenv += (s -> t)
                  println(s" $s: $t defined.")
                }
              }

          case Command.LetType(s, k, t) =>
            if (globalTenv contains s) || (globalTKenv contains s) then
              println(s" $s already exists.")
            else {
              KindCheck(globalTKenv, t) match {
                case None => println("KindCheck fail")
                case Some(k2) => {
                  if (k == k2) then
                    globalTTenv += (s -> t)
                    globalTKenv += (s -> k)
                    println(s" $s = $t : $k defined.")
                  else println(s"kind $k is different to ${k2}.")
                }
              }
            }

          case Command.Definition(s, ty) =>
            if (globalTenv contains s) || (globalTKenv contains s) then
              println(s" $s already exists.")
            else
              KindCheck(globalTKenv, ty) match {
                case Some(Kind.ProperK) => {
                  val ictx = Context(Map(), Map(), (0, ty))
                  val ihe = HoleExpr.Hole(0, ty)
                  proof(
                    reader,
                    s,
                    List(ictx),
                    ihe,
                    globalTKenv,
                    globalTTenv,
                    globalTenv
                  ) match {
                    case Some(e) => {
                      globalEnv += (s -> e)
                      globalTenv += (s -> ty)
                      println(s" $s: $ty defined.")
                    }
                    case None => println(s" $s removed.")
                  }
                }
                case _ => println("KindCheck fail")
              }
          case Command.Print(s) =>
            (globalTenv.lift(s), globalEnv.lift(s)) match {
              case (Some(t), Some(e)) => println(s" $s = $e : $t")
              case _ =>
                (globalTKenv.lift(s), globalTTenv.lift(s)) match {
                  case (Some(k), Some(t)) => println(s" $s = $t : $k")
                  case _                  => println(s" $s is not exists.")
                }
            }
        }
      )
    }

  def proof(
      reader: LineReader,
      name: String,
      ctxs: List[Context],
      he: HoleExpr,
      globalTKenv: Map[String, Kind],
      globalTalias: Map[String, Type],
      globalTenv: Map[String, Type]
  ): Option[Expr] = {
    ctxs match {
      case ctx :: ts =>
        println(ctx.toDetail); ts.foreach(ctx => println(ctx.toShort))
      case Nil => println(" No more subgoals.")
    }; println(s" Building Expression: ${he.toString}")
    val input =
      try {
        reader.readLine(prompt(name))
      } catch {
        case _: EndOfFileException | _: UserInterruptException => ":q"
      }
    if (eof(input) || input.trim.isEmpty) None
    else
      val opt = lift {
        val tac = Tactic(input)
        // println(s"  ${C}Parsed:$RESET $tac")
        tac
      }
      opt match {
        case Some(Tactic.Defined) =>
          if (ctxs.length == 0) he.toExpr
          else {
            println(" There are unresolved subgoals.");
            proof(reader, name, ctxs, he, globalTKenv, globalTalias, globalTenv)
          }
        case Some(t) =>
          ctxs match {
            case ctx :: ts =>
              Tactic.manipulate(
                t,
                globalTKenv,
                globalTalias,
                globalTenv,
                ctx,
                he
              ) match {
                case None =>
                  proof(
                    reader,
                    name,
                    ctxs,
                    he,
                    globalTKenv,
                    globalTalias,
                    globalTenv
                  )
                case Some(nctxs, nhe) =>
                  proof(
                    reader,
                    name,
                    nctxs ++ ts,
                    nhe,
                    globalTKenv,
                    globalTalias,
                    globalTenv
                  )
              }
            case Nil =>
              proof(
                reader,
                name,
                ctxs,
                he,
                globalTKenv,
                globalTalias,
                globalTenv
              )
          }
        case None =>
          proof(reader, name, ctxs, he, globalTKenv, globalTalias, globalTenv)
      }
  }

  def eof(str: String): Boolean =
    str == ":quit" || str == ":q" || str == "Quit."
  def defined(str: String): Boolean = str == "Defined."

  def lift[T](res: => T): Option[T] = try {
    Some(res)
  } catch {
    case ParsingError(msg) =>
      println(s"  Parsing failed. $msg")
      None
    case e: Throwable =>
      e.printStackTrace()
      None
  }

end Main
