package userspace

import pseudolib._

import annotation.internal.local

object obj {
  val llist = 1 :: 2 :: 3 :: NNil[Int]()

  def foo(@local c: Console): Any = {
    val l = LList.map(llist) { i => c.println(i) }
    l
  }

  def bar(
    @local c: Console // error
  ): Any = c
}
