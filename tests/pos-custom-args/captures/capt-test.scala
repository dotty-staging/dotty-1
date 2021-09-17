class C
type Cap = {*} C
class A
class B
def m[A, B] =
  ??? : ((f: {*} A => B) => {f} (xs: List[A]) => List[B])
  //: (f: {*} A => B) => {f} (List[A] => List[B])

def mc: (f: {*} String => Int) => {f} List[String] => List[Int] = m[String, Int]