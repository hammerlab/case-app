package caseapp.core

import caseapp.util.Implicit
import caseapp.{ @@, HelpMessage, Hidden, Name, ValueDescription }
import shapeless.labelled.{ FieldType, field }
import shapeless.{ ::, HList, HNil, Strict, Witness, the }

trait ContextHListParser[Context, L <: HList, D <: HList, -N <: HList, -V <: HList, -M <: HList, -H <: HList, R <: HList] {
  type P <: HList
  def apply(default: D, names: N, valueDescriptions: V, helpMessages: M, noHelp: H): ContextParser.Aux[Context, L, P]
}

object ContextHListParser {
  def apply[Context, L <: HList, D <: HList, N <: HList, V <: HList, M <: HList, H <: HList, R <: HList](implicit args: ContextHListParser[Context, L, D, N, V, M, H, R]): Aux[Context, L, D, N, V, M, H, R, args.P] = args

  type Aux[Context, L <: HList, D <: HList, N <: HList, V <: HList, M <: HList, H <: HList, R <: HList, P0 <: HList] =
    ContextHListParser[Context, L, D, N, V, M, H, R] { type P = P0 }

  def instance[Context, L <: HList, D <: HList, N <: HList, V <: HList, M <: HList, H <: HList, R <: HList, P0 <: HList](p: (D, N, V, M, H) => ContextParser.Aux[Context, L, P0]): Aux[Context, L, D, N, V, M, H, R, P0] =
    new ContextHListParser[Context, L, D, N, V, M, H, R] {
      type P = P0
      def apply(default: D, names: N, valueDescriptions: V, helpMessages: M, noHelp: H) = p(default, names, valueDescriptions, helpMessages, noHelp)
    }

  implicit def hnil[Context]: Aux[Context, HNil, HNil, HNil, HNil, HNil, HNil, HNil, HNil] =
    instance { (_, _, _, _, _) =>
      new ContextParser[Context, HNil] {
        type D = HNil
        override def apply(implicit context: Context): Parser.Aux[HNil, HNil] =
          the[Parser.Aux[HNil, HNil]]
      }
    }

  implicit def hconsTaggedDefault[Context, K <: Symbol, Tag, H, T <: HList, PT <: HList, DT <: HList, NT <: HList, VT <: HList, MT <: HList, HT <: HList, RT <: HList]
  (implicit
   name: Witness.Aux[K],
   argParser: Strict[ContextArgParser[Context, H @@ Tag]],
   headDefault: Implicit[Option[Default[H @@ Tag]]],
   tail: Strict[Aux[Context, T, DT, NT, VT, MT, HT, RT, PT]]
  ): Aux[Context, FieldType[K, H @@ Tag] :: T, Option[H @@ Tag] :: DT, List[Name] :: NT, Option[ValueDescription] :: VT, Option[HelpMessage] :: MT, Option[Hidden] :: HT, None.type :: RT, Option[H @@ Tag] :: PT] =
    hconsDefault[Context, K, H @@ Tag, T, PT, DT, NT, VT, MT, HT, RT]

  implicit def hconsDefault[Context, K <: Symbol, H, T <: HList, PT <: HList, DT <: HList, NT <: HList, VT <: HList, MT <: HList, HT <: HList, RT <: HList]
  (implicit
   name: Witness.Aux[K],
   contextArgParser: Strict[ContextArgParser[Context, H]],
   headDefault: Implicit[Option[Default[H]]],
   tail: Strict[Aux[Context, T, DT, NT, VT, MT, HT, RT, PT]]
  ): Aux[Context, FieldType[K, H] :: T, Option[H] :: DT, List[Name] :: NT, Option[ValueDescription] :: VT, Option[HelpMessage] :: MT, Option[Hidden] :: HT, None.type :: RT, Option[H] :: PT] =
    instance { (default0, names, valueDescriptions, helpMessages, noHelp) =>

      val tailContextParser = tail.value(default0.tail, names.tail, valueDescriptions.tail, helpMessages.tail, noHelp.tail)

      val headNames = Name(name.value.name) :: names.head
      val headDescriptions = valueDescriptions.head
      val headDefault0 = default0.head
      val defaultValuePreset = headDefault0.orElse(headDefault.value.map(_()))

      new ContextParser[Context, FieldType[K, H] :: T] {
        type D = Option[H] :: PT
        override def apply(implicit context: Context): Parser.Aux[FieldType[K, H] :: T, D] = {

          implicit val argParser: Strict[ArgParser[H]] = contextArgParser.map(_.apply)

          implicit val tailParser: Parser.Aux[T, PT] = tailContextParser.apply

          new Parser[FieldType[K, H] :: T] {
            val args = Arg(
              name.value.name,
              headNames,
              headDescriptions,
              helpMessages.head,
              noHelp.head.nonEmpty,
              argParser.value.isFlag,
              argParser.value.description,
              for (dv <- headDefault.value; dvp <- defaultValuePreset) yield dv.describe(dvp)
            ) +: tailParser.args

            type D = Option[H] :: PT

            def init = None :: tailParser.init

            def step(args: Seq[String], d: Option[H] :: PT) = {
              if (args.isEmpty)
                Right(None)
              else {
                val matchedOpt = headNames.iterator.map(_.apply(args.head)).collectFirst {
                  case Right(valueOpt) => valueOpt
                }

                matchedOpt match {
                  case Some(valueOpt) =>
                    if (valueOpt.isEmpty && args.tail.isEmpty)
                      argParser.value(d.head).right.map(h => Some((Some(h) :: d.tail, args.tail)))
                    else
                      argParser.value(d.head, valueOpt.getOrElse(args.tail.head), valueOpt.nonEmpty).right.flatMap {
                        case (usedArg, h) =>
                          if (valueOpt.nonEmpty && !usedArg)
                            Left(s"Unrecognized value: ${valueOpt.get}")
                          else
                            Right(Some((Some(h) :: d.tail, if (valueOpt.nonEmpty) args.tail else if (usedArg) args.tail.tail else args.tail)))
                      }

                  case None =>
                    tailParser.step(args, d.tail).right.map(_.map {
                      case (t, args) => (d.head :: t, args)
                    })
                }
              }
            }

            def get(d: Option[H] :: PT) = {
              val maybeHead = d.head
                              .orElse(defaultValuePreset)
                              .toRight(s"Required option ${name.value.name} / $headNames not specified")
              for {
                h <- maybeHead.right
                t <- tailParser.get(d.tail).right
              } yield field[K](h) :: t
            }
          }
        }
      }
    }

//  implicit def hconsRecursive[K <: Symbol, H, HD, T <: HList, PT <: HList, DT <: HList, NT <: HList, VT <: HList, MT <: HList, HT <: HList, RT <: HList]
//  (implicit
//   headParser: Strict[ContextParser.Aux[H, HD]],
//   tail: Aux[T, DT, NT, VT, MT, HT, RT, PT]
//  ): Aux[FieldType[K, H] :: T, Option[H] :: DT, Nil.type :: NT, None.type :: VT, None.type :: MT, None.type :: HT, Some[Recurse] :: RT, HD :: PT] =
//    instance { (default0, names, valueDescriptions, helpMessages, noHelp) =>
//      val tailParser = tail(default0.tail, names.tail, valueDescriptions.tail, helpMessages.tail, noHelp.tail)
//
//      new ContextParser[FieldType[K, H] :: T] {
//        type D = HD :: PT
//        override def apply(implicit context: Context): Parser[FieldType[K, H] :: T] = ???
//      }
//    }
}
