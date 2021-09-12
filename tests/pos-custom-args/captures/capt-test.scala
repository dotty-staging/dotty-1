class C
type Cap = {*} C
class A
class B
def m[A, B]//: (f: {*} A => B) => {f} (List[A] => List[B])
  =
  //[A, B] =>
    (f: {*} A => B) => (xs: List[A]) => f.asInstanceOf[List[B]]

