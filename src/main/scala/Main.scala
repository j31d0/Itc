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
  val typeSystem = "CoC"

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
    var globalTenv: Map[String, Expr] = Map()

    for (str <- strs(name).takeWhile(s => !eof(s)) if str.trim.nonEmpty) {
      val opt = lift {
        val cmd = Command(str)
        // println(s"  ${C}Parsed:$RESET $cmd")
        cmd
      }

      opt.foreach(cmd =>
        cmd match {
          case Command.Let(s, e) =>
            if (globalTenv contains s) then
              println(s" $s already exists.")
            else
              TypeCheck(globalTenv, globalEnv, e) match {
                case None => println("TypeCheck fail")
                case Some(t) => {
                  globalEnv += (s -> e)
                  globalTenv += (s -> t)
                  println(s" $s: $t defined.")
                }
              }

          case Command.Definition(s, ty) =>
            if (globalTenv contains s) then
              println(s" $s already exists.")
            else
              TypeCheck(globalTenv, globalEnv, ty) match {
                case Some(_) => {
                  val ictx = Context(Map(), (0, ty))
                  val ihe = HoleExpr.Hole(0, ty)
                  proof(
                    reader,
                    s,
                    List(ictx),
                    ihe,
                    globalTenv,
                    globalEnv,
                  ) match {
                    case Some(e) => {
                      if (TypeCheck(globalTenv, globalEnv, e).isEmpty) println(" warning: TypeCheck fail on whole expression")
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
              case _ => println(s" $s is not exists.")
            }
        }
      )
    }

  def proof(
      reader: LineReader,
      name: String,
      ctxs: List[Context],
      he: HoleExpr,
      globalTenv: Map[String, Expr],
      globalEnv: Map[String, Expr]
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
            proof(reader, name, ctxs, he, globalTenv, globalEnv)
          }
        case Some(t) =>
          ctxs match {
            case ctx :: ts =>
              Tactic.manipulate(
                t,
                globalTenv,
                globalEnv,
                ctx,
                he
              ) match {
                case None =>
                  proof(
                    reader,
                    name,
                    ctxs,
                    he,
                    globalTenv,
                    globalEnv
                  )
                case Some(nctxs, nhe) =>
                  proof(
                    reader,
                    name,
                    nctxs ++ ts,
                    nhe,
                    globalTenv,
                    globalEnv
                  )
              }
            case Nil =>
              proof(
                reader,
                name,
                ctxs,
                he,
                globalTenv,
                globalEnv
              )
          }
        case None =>
          proof(reader, name, ctxs, he, globalTenv, globalEnv)
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
