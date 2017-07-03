package caseapp.core

trait ContextArgParser[-Context, T] {
  def apply(implicit context: Context): ArgParser[T]
}

object ContextArgParser {

  def apply[Context, T](implicit parser: ContextArgParser[Context, T]): ContextArgParser[Context, T] = parser

  implicit def fromArgParser[Context, T](implicit argParser: ArgParser[T]): ContextArgParser[Context, T] =
    new ContextArgParser[Context, T] {
      override def apply(implicit context: Context): ArgParser[T] = argParser
    }

  def instance[Context, T](hintDescription: String)(f: String => Either[String, T]): ContextArgParser[Context, T] =
    new ContextArgParser[Context, T] {
      override def apply(implicit context: Context): ArgParser[T] =
        ArgParser.instance(hintDescription)(f)
    }

  implicit def conextOptionArgParser[Context, T](implicit
                                                 contextArgParser: ContextArgParser[Context, T]
                                                ): ContextArgParser[Context, Option[T]] =
    new ContextArgParser[Context, Option[T]] {
      override def apply(implicit context: Context): ArgParser[Option[T]] =
        ArgParser.option(contextArgParser.apply)
    }

}
