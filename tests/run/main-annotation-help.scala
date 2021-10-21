// Sample main method
object myProgram:

  /** Adds two numbers */
  @main def add(num: Int, inc: Int = 1): Unit =
    println(s"$num + $inc = ${num + inc}")

end myProgram

object Test:
  def callMain(args: Array[String]): Unit =
    val clazz = Class.forName("add")
    val method = clazz.getMethod("main", classOf[Array[String]])
    method.invoke(null, args)

  def main(args: Array[String]): Unit =
    callMain(Array("--help"))
    callMain(Array("Some", "garbage", "before", "--help"))
    callMain(Array("--help", "and", "some", "stuff", "after"))
end Test
