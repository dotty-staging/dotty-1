
@main def Test: Unit =
  println(labeled.block { label =>
    "Expression result"
  })

  println(labeled.block { label =>
    label.break("Break result")
  })

  var count = 0
  println(labeled.block { label =>
    if count < 5 then
      count += 1
      label.continue()
    s"Continued $count times"
  })

  println(labeled.block { label =>
    (() => "(non-local) Expression result")()
  })

  println(labeled.block { label =>
    (() => label.break("(non-local) Break result"))()
  })

  var count2 = 0
  println(labeled.block { label =>
    (() => {
      if count2 < 5 then
        count2 += 1
        label.continue()
      s"(non-local) Continued $count times"
    }
    )()
  })

   println(labeled.block { label =>
    identity(label) // could break or continue
    "(leak) Expression result"
  })