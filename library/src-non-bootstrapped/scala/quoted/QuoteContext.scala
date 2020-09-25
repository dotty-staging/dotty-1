package scala.quoted

import scala.tasty.QuoteContextReflection
import scala.internal.tasty.CompilerInterface


trait QuoteContext extends QuoteContextReflection, CompilerInterface { self =>

  type Nested = QuoteContext {
    val tasty: self.tasty.type
  }

}
