package projects

import scala.annotation.tailrec

object Retry extends App{
  @tailrec
  def retry[T](times: Int, delay: Int = 0)(block: => T): T = {
    try {
      block
    } catch {
      case ex: Throwable =>
        if (times > 1) {
          if (delay > 0) Thread.sleep(delay)
          retry(times - 1, delay)(block)
        } else throw ex
    }
  }
  retry(3)(
    {
      println("Trying...")
      if (math.random() < 0.6) throw new RuntimeException("Fail")
      println("Success")
    }
  )


}
