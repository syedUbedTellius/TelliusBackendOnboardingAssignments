package projects

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}


abstract class node() {
  def insert(d:Int):node
  def leafs():Int
  def height():Int
  def preorder():Unit
}

case class emptynode() extends node{
  override def insert(d:Int): node = {
    fillednode(d,emptynode(),emptynode())
  }

  override def preorder(): Unit = {}

  override def leafs(): Int = -1

  override def height(): Int = 0
}

 case class fillednode(value:Int, left:node, right:node) extends node{
   override def insert(d:Int): node = {
     if(d<value){
       fillednode(value,left.insert(d),right)
     }else{
       fillednode(value,left,right.insert(d))
     }
   }

   override def preorder(): Unit = {
     println(value)
     left.preorder()
     right.preorder()
   }

   override def leafs(): Int = {
     val a=left.leafs()
     val b=right.leafs()
     if(a==(-1) && (b==(-1)) ){
       1
     }else{
       (if(a!=(-1)) a else 0) + (if(b!=(-1)) b else 0)
     }
   }

   override def height(): Int = {
     (if(left.height()>right.height()) left.height() else right.height())+1
   }
 }
object BT extends App{
var n:node = emptynode()
  n= n.insert(2)
  n=n.insert(1)
  n= n.insert(3)
  println(n.leafs())
  println(n.height())
  n.preorder()

  val f1 = Future(n.height())
  val f2 = Future(n.leafs())

  val combined = for {
    h <- f1.map(Success(_)).recover { case e => Failure(e) }
    l <- f2.map(Success(_)).recover { case e => Failure(e) }
  } yield (h, l)

  combined.onComplete {
    case Success((Success(h),Success(l))) =>
      println(s"$h $l")

    case Success((Success(h),Failure(ex)))=>
      println(s"height is ${h} but cant find leafs because ${ex}")

    case Success((Failure(ex),Success(l)))=>
      println(s"leafs is ${l} but cant find height because ${ex}")

    case Failure(ex) =>
      println("system failure: " + ex)
  }

  Thread.sleep(4000)


}
