package future

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FutureFactory {

  def apply[T](block: () => T): Future[T] = Future(block())

}
