object ConsoleTest {
  import scala.annotation.internal.local

  class IO
  class Console(io: IO)

  def foo(@local io: IO) = {
    val console = new Console(io)
    bar(console)
  }

  def bar(@local console: Console) = {
    ()
  }
}
