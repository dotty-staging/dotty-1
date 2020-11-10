package scala.quoted

trait QuoteContext { self: scala.internal.quoted.QuoteContextInternal =>

  val reflect: scala.tasty.Reflection

  type Nested = QuoteContext {
    val reflect: self.reflect.type
  }

}
