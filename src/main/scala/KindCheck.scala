package itc

import Type.*

object KindCheck:
  def apply(kenv: Map[String, Kind], t: Type): Option[Kind] = t match
    case FunT(x, b) => apply(kenv + (x -> Kind.DotKind), b)
    case IdT(name)  => kenv.lift(name)
    case ArrowT(p, r) =>
      apply(kenv, p) match {
        case Some(Kind.DotKind) =>
          apply(kenv, r) match {
            case Some(Kind.DotKind) => Some(Kind.DotKind)
            case _                  => None
          }
        case _ => None
      }
    case IntT     => Some(Kind.DotKind)
    case BooleanT => Some(Kind.DotKind)
end KindCheck
