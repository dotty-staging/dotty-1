object Escapee {
  import scala.annotation.internal.local
  def escapade(@local notLocal: Any): Any =
    notLocal // error
}
