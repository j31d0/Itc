package itc

enum Command:
    case Let(x: String, e: Expr) extends Command
    case LetType(x: String, k: Kind, t: Type) extends Command
    case Definition(x: String, t: Type) extends Command
    case Print(x: String) extends Command
end Command
object Command:
    def apply(str: String): Command = Parser.parseAll(Parser.cparser, str).getOrElse(Parser.error("Command parse fail"))
end Command