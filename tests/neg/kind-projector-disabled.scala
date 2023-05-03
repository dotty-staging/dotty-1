
def xd1[A, M <: Map[A, _]](ma: M[Int]): Unit = ???

def xd2[A, M <: Map[A, *]](ma: M[Int]): Unit = ??? // error // error
