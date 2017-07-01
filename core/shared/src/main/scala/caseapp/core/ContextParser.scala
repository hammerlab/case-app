package caseapp.core

import caseapp.util.AnnotationList
import caseapp.{ HelpMessage, Hidden, Name, Recurse, ValueDescription }
import shapeless.{ Annotations, HList, LabelledGeneric, Strict }

trait ContextParser[T] { self =>
  type D
  def apply(implicit context: Context): Parser.Aux[T, D]

  def map[U](f: T => U): ContextParser.Aux[U, D] =
    new ContextParser[U] {
      type D = self.D

      override def apply(implicit context: Context): Parser.Aux[U, D] = self.apply.map(f)
    }
}

object ContextParser {

  def apply[T](implicit parser: ContextParser[T]): Aux[T, parser.D] = parser

  type Aux[T, D0] = ContextParser[T] { type D = D0 }

  implicit def fromParser[T, D](implicit parser: Parser.Aux[T, D]): ContextParser.Aux[T, D] =
    new ContextParser[T] {
      type D = parser.D
      override def apply(implicit context: Context): Parser.Aux[T, D] = parser
    }

  implicit def generic[CC, L <: HList, D <: HList, N <: HList, V <: HList, M <: HList, H <: HList, R <: HList, P <: HList]
  (implicit
   gen: LabelledGeneric.Aux[CC, L],
   defaults: shapeless.Default.AsOptions.Aux[CC, D],
   names: AnnotationList.Aux[Name, CC, N],
   valuesDesc: Annotations.Aux[ValueDescription, CC, V],
   helpMessages: Annotations.Aux[HelpMessage, CC, M],
   noHelp: Annotations.Aux[Hidden, CC, H],
   recurse: Annotations.Aux[Recurse, CC, R],
   parser: Strict[ContextHListParser.Aux[L, D, N, V, M, H, R, P]]
  ): Aux[CC, P] =
    parser.value(defaults(), names(), valuesDesc(), helpMessages(), noHelp()).map(gen.from)
}
