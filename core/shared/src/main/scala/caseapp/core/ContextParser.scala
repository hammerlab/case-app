package caseapp.core

import caseapp.util.AnnotationList
import caseapp.{ HelpMessage, Hidden, Name, Recurse, ValueDescription }
import shapeless.{ Annotations, HList, LabelledGeneric, Strict }

trait ContextParser[-Context, T] { self =>
  type D
  def apply(implicit context: Context): Parser.Aux[T, D]

  def map[U](f: T => U): ContextParser.Aux[Context, U, D] =
    new ContextParser[Context, U] {
      type D = self.D

      override def apply(implicit context: Context): Parser.Aux[U, D] = self.apply.map(f)
    }
}

object ContextParser {

  def apply[Context, T](implicit parser: ContextParser[Context, T]): Aux[Context, T, parser.D] = parser

  type Aux[-Context, T, D0] = ContextParser[Context, T] { type D = D0 }

  implicit def fromParser[Context, T, D](implicit parser: Parser.Aux[T, D]): ContextParser.Aux[Context, T, D] =
    new ContextParser[Context, T] {
      type D = parser.D
      override def apply(implicit context: Context): Parser.Aux[T, D] = parser
    }

  implicit def generic[Context, CC, L <: HList, D <: HList, N <: HList, V <: HList, M <: HList, H <: HList, R <: HList, P <: HList]
  (implicit
   gen: LabelledGeneric.Aux[CC, L],
   defaults: shapeless.Default.AsOptions.Aux[CC, D],
   names: AnnotationList.Aux[Name, CC, N],
   valuesDesc: Annotations.Aux[ValueDescription, CC, V],
   helpMessages: Annotations.Aux[HelpMessage, CC, M],
   noHelp: Annotations.Aux[Hidden, CC, H],
   recurse: Annotations.Aux[Recurse, CC, R],
   parser: Strict[ContextHListParser.Aux[Context, L, D, N, V, M, H, R, P]]
  ): Aux[Context, CC, P] =
    parser.value(defaults(), names(), valuesDesc(), helpMessages(), noHelp()).map(gen.from)
}
