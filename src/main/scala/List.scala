sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](h:A,t:List[A]) extends List[A]

object List {

  def sum(a:List[Int]):Int= a match {
    case Nil => 0
    case Cons(h,t)=> h+sum(t)
  }
  def product(a:List[Double]):Double= a match {
    case Nil=>1.0
    case Cons(0.0, _)=>0.0
    case Cons(h, t) => h*product(t)
  }
  def apply[A](a:A*):List[A]=if(a.isEmpty)Nil else Cons(a.head,apply(a.tail:_*))
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _)))=>x
    case Nil => 42
    case Cons(x,Cons(y,Cons(3,Cons(4, _)))) =>x+y
    case Cons(h, t)=>h+sum(t)
    case _ =>102
  }
  def append[A](a1:List[A],a2:List[A]):List[A]= a1 match {
    case Nil=>a2
    case Cons(h, t) => Cons(h,append(t,a2))
  }
  def foldRight[A,B](a:List[A],b:B)(f:(A,B)=>B):B=a match {
    case Nil => b
    case Cons(h, t)=>f(h, foldRight(t,b)(f))
  }
  def sum2(a:List[Int]):Int=foldRight(a,0)(_+_)
  def product2(a:List[Double]):Double=foldRight(a,1.0)(_*_)
  def tail[A](a:List[A]):List[A]=a match {
    case Nil=>sys.error("error")
    case Cons(_, t) => t
  }
  def setHead[A](a:List[A], h:A):List[A]=a match {
    case Nil=>sys.error("error")
    case Cons(_, t) =>Cons(h,t)
  }
  def drop[A](a:List[A],n:Int):List[A]=a match {
    case Nil=>Nil
    case Cons(h, t) => drop(t,n-1)
  }
  def dropWhile[A](a:List[A], f: A=>Boolean):List[A]=a match {
    case Cons(h,t) if f(h)=>dropWhile(t,f)
    case _=>a
  }

  def init[A](a:List[A]):List[A]= a match {
    case Nil => sys.error("init of empty list")
      // 匹配到最后一次Nil为止
    case Cons(_,Nil)=>Nil
      // 不是最后一个就一直往下匹配，不停初始化，每次都把一个元素依次加入
    case Cons(h, t)=>Cons(h,init(t))
  }
  // 把List(a,b,c,d,..)变为 Cons(a,Cons(b,Cons(c,Cons(d,...))))
//  def init2[A](a:List[A]):List[A]= {
//    import collection.mutable.ListBuffer
//    val b = new ListBuffer[A]
//    @annotation.tailrec
//    def go[A](a:List[A]):List[A]= a match {
//      case Cons(_,Nil)=>List(b.toList:_*)
//      case Cons(h, t)=>b+=h;go(t)
//    }
//    go(a)
//  }
}
