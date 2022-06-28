package itc

import Type.*

object KindCheck:
  def apply(kenv: Map[String, Kind], t: Type): Option[Kind] = t match
    case FunT(x, k, b) => apply(kenv + (x -> k), b) match
      case Some(r) => Some(Kind.ArrowK(k, r))
      case None => None
    case AppT(t1, t2) => apply(kenv, t1) match
      case Some(Kind.ArrowK(p, r)) => apply(kenv, t2) match
        case Some(k) => if (p == k) then Some(r) else None
        case None => None
      case _ => None
    case UnivT(x, k, b) => apply(kenv + (x -> k), b)
    case IdT(name)  => kenv.lift(name)
    case ArrowT(p, r) =>
      apply(kenv, p) match {
        case Some(Kind.ProperK) =>
          apply(kenv, r) match {
            case Some(Kind.ProperK) => Some(Kind.ProperK)
            case _                  => None
          }
        case _ => None
      }
    case IntT     => Some(Kind.ProperK)
    case BooleanT => Some(Kind.ProperK)
end KindCheck
