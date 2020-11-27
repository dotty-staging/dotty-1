object Messages {
  private var logs: Int = 0
  def logMessage(a: String): Unit =
    logs += 1
    println(a)
  def count = logs
}

@main def Test: Unit =
  assert(Messages.count == 0)
  visitExportsExprMap(new Logger { export Messages.{logMessage => log} })(
    _.log("visited exports with ExprMap")
  )
  assert(Messages.count == 1)
  visitExportsTreeMap(new Logger { export Messages.{logMessage => log} })(
    _.log("visited exports with TreeMap")
  )
  assert(Messages.count == 2)
  visitExportsTreeAccumulator(new Logger { export Messages.{logMessage => log}; export Messages.count })(
    exportStrings => println(s"extracted with TreeAccumulator: $exportStrings")
  )
  println("reflection show:")
  visitExportsShow({ object Observer { export Messages.count } })
  println("reflection show extractors:")
  visitExportsShowExtract({ object Observer { export Messages.count } })
  val localLogger = new Logger { def log(a: String): Unit = println(a) }
  visitExportsSplice(localLogger).log("visited exports with splice")
  visitExportsSpliceInverse(logger => new Logger {
    private val delegate = logger
    export delegate._
  }).log("visited exports with splice inverted")
  visitExportsShowMembers({
    object NatModule {

      class Nat(val toInt: Int)

      final object Zero extends Nat(0)

      def Succ(pred: Nat): Nat = Nat(pred.toInt + 1)

    }
    object Nats {
      export NatModule._
    }
  })
  visitExportsShowMembers({
    new Logger {
      export Messages.{logMessage => log, _}
    }
  })
  visitExportsShowMembers({
    object EnumModule {
      enum Color {
        case Red, Green, Blue
      }
    }
    object Colors {
      export EnumModule._
    }
  })
  visitExportsShowMembers({
    object BoolsModule {
      opaque type Bool = Boolean
      final val True: Bool = true
      final val False: Bool = false
    }
    object Bools {
      export BoolsModule._
    }
  })
  visitExportsShowMembers({
    object CompanionsModule {
      final class Box[+T] private (t: T)
      object Box {
        def create[T](t: T): Box[T] = Box(t)
      }
    }
    object Boxes {
      export CompanionsModule._
    }
  })
