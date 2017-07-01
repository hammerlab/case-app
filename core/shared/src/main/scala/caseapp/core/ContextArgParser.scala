package caseapp.core

import java.nio.file.{ Path, Paths }

import caseapp.core.ArgParser.instance

trait Context {
  def str: String
}

trait ContextArgParser[T] {
  def apply(implicit context: Context): ArgParser[T]
}

object ContextArgParser {

  def apply[T](implicit parser: ContextArgParser[T]): ContextArgParser[T] = parser

  implicit def fromArgParser[T](implicit argParser: ArgParser[T]): ContextArgParser[T] =
    new ContextArgParser[T] {
      override def apply(implicit context: Context): ArgParser[T] = argParser
    }
}

case class Foo(n: Int, s: String)

object Foo {
  val parser = implicitly[ContextParser[Foo]]
}

case class Pat(path: Path)

object Pat {
  implicit val parser: ContextArgParser[Pat] =
    new ContextArgParser[Pat] {
      override def apply(implicit context: Context): ArgParser[Pat] =
        instance("pat") {
          str ⇒
            Right(
              Pat(
                Paths.get(
                  context.str + "/" + str
                )
              )
            )
        }
    }
}

case class Bar(n: Int, pat: Pat)

object Bar {
  val parser = implicitly[ContextParser[Bar]]
}
