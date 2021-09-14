class C
type Cap = {*} C
def m[A, B]//: (f: {*} A => B) => {f} (List[A] => List[B])
  =
  //[A, B] =>
    (f: {*} A => B) => (xs: List[A]) => f.asInstanceOf[List[B]]

def mc: (f: {*} String => Int) => {f} List[String] => List[Int] = m[String, Int]