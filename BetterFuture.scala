import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**@author Sachin verma
 * Experimental library for people like me who don't want to go with pure FP IO's but want Futures
 * to behave better. */
class BetterFuture[+A](val pieceOfCode: ()=>A) { self=>

  def execute(implicit ec:ExecutionContext) = {
    Future(Try(pieceOfCode.apply).toEither)
  }

  def executeInSameThread = Try(pieceOfCode.apply).toEither

  def inParallelWith[B](f: =>B)(implicit ec:ExecutionContext) = {
    val a = self.execute
    val b = BetterFuture(f).execute
    a flatMap(_=>b) // check if exception from a is passed to b
  }

  def <||> [B](f: =>B)(implicit ec:ExecutionContext) = inParallelWith(f)

  def map[B](f: A => B)(implicit ec: ExecutionContext): BetterFuture[B] = {
    for {
      x <- self
    } yield f.apply(x)
  }

  def flatMap[B](f: A => BetterFuture[B])(implicit executor: ExecutionContext): BetterFuture[B] = {
    for {
      x <- self
      res <- f(x)
    } yield res
  }



}

object BetterFuture{
  def apply[A](a: =>A) = new BetterFuture[A](()=>a)
}

