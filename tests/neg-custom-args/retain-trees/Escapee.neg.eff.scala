object Escapee {
  import scala.annotation.internal.local

  def escapade(
    @local notLocal: Any // error
  ): Any =
    notLocal

  def greatEscape(
    @local notLocal: Any // error
  ): Any = {
    {
      notLocal
    }
  }
}
