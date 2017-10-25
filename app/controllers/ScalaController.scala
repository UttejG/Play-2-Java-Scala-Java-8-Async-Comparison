package controllers

import javax.inject.Inject

import future.FutureFactory
import play.Logger
import play.api.http.Writeable
import play.api.libs.json.{Format, Json, Writes}
import play.api.libs.json.Json.format
import play.api.libs.ws.WSClient
import play.api.mvc.{Action, Codec, Controller}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ScalaController @Inject() (wsClient: WSClient) extends Controller {

  val names: List[String] = List("Jim", "Dean", "Kunal")

  def lower(str: String): String = str.toLowerCase()

  def strlen(str: String): Int = str.length

  def explode(str: String): List[Char] = str.toCharArray().toList

  def index = Action {
    val loweredNames: List[String] = names.map(lower)
    println(s"$names.map(lower) yields $loweredNames")

    val lengthOfNames: List[Int] = names.map(strlen)
    println(s"$names.map(strlen) yields $lengthOfNames")

    val explodedNames: List[List[Char]] = names.map(explode)
    println(s"$names.map(explode) yields $explodedNames")

    val flattenedExplodedNames: List[Char] = names.flatMap(explode)
    println(s"$names.map(explode) yields $flattenedExplodedNames")

    Ok(lower("Hello World"))
  }

  def proxy = Action.async {
    val responseFuture = wsClient.url("http://example.com").get()

    Logger.info("Before map")
    val resultFuture = responseFuture.map { resp =>
        Logger.info("Within map")
        Status(resp.status)(resp.body).as(resp.header("Content-Type").get)
    }

    Logger.info("After map")
    resultFuture
  }

  def parallel = Action.async {
    val start = System.currentTimeMillis()
    def getLatency(r: Any): Long = System.currentTimeMillis() - start

    val google = wsClient.url("http://google.com").get().map(getLatency)
    val yahoo = wsClient.url("http://yahoo.com").get().map(getLatency)

    google.flatMap { googleResponseTime: Long =>
        yahoo.map { yahooResponseTime: Long =>
            Ok(s"Google response time:  $googleResponseTime; " +
               s"Yahoo response time: $yahooResponseTime")
        }
    }
  }

  def paramsFromFoo(x: Any): String = "?bar=baz"

  def sequential = Action.async {
    val foo = wsClient.url("http://www.yahoo.com").get()

    foo.flatMap { fooResponse =>
        // Use data in fooResponse to build the second request
        val bar = wsClient.url("http://www.google.com").get()

        bar.map { barResponse =>
            // Now you can use barResponse and fooResponse to build a Result
            Ok(s"response from yahoo.com is ${fooResponse.status} & " +
               s"from google.com is ${barResponse.status}")
        }
    }
  }

  // Handle Exceptions in Futures by logging them and returning a fallback value
  def withErrorHandling[T](f: Future[T], fallback: T): Future[T] = {
    f.recover {
      case t: Throwable =>
        Logger.error("Something went wrong!", t)
        fallback
    }
  }

  def checkHostName(hostName: String) = Action.async {
    // try using "thisdomaindoesnotexist"
    val myFuture = wsClient.url(s"http://www.$hostName.com").get()
      .map { resp => resp.statusText }

    val myFutureWithFallback = withErrorHandling(myFuture, "fallback value")

    // str either contains the result of myFuture's async I/O or
    // "fallback value" if any Exception was thrown
    myFutureWithFallback.map { str => Ok(str) }
  }

  case class PostNComments(postId: Int, title: String, body: String, commentId: Int)
  case class SequentialGetPostFutureFutureRequest(postAndComments: Seq[PostNComments])
  case class Post(id: Int, title: String)
  case class Comment(id: Int, body: String, postId: Int)

  implicit val utf8Codec: Codec = Codec.utf_8

  implicit val postWrites: Writes[Post] = Json.writes[Post]
  implicit val commentWrites: Writes[Comment] = Json.writes[Comment]
  implicit val postNCommentsFormat: Format[PostNComments] = Json.format[PostNComments]

  def writable[A](implicit codec: Codec, writes: Writes[A]): Writeable[A] =
    new Writeable(
      content => codec.encode(Json.stringify(Json.toJson(content)(writes))),
      Option("application/json")
    )

  implicit def postWritable(implicit codec: Codec, writes: Writes[Post]): Writeable[Post] = writable[Post](codec, writes)

  implicit def commentWritable(implicit codec: Codec, writes: Writes[Comment]): Writeable[Comment] = writable[Comment](codec, writes)

  def sequentialGetGetFutureFuture =
    Action.async {
      FutureFactory { () =>
        Thread.sleep(20)
        FutureFactory { () =>
          val foo = wsClient.url("http://www.yahoo.com").get()

          foo.flatMap { fooResponse =>
            // Use data in fooResponse to build the second request
            val bar = wsClient.url("http://www.google.com").get()

            bar.map { barResponse =>
              // Now you can use barResponse and fooResponse to build a Result
              Ok(s"response from yahoo.com is ${fooResponse.status} & from google.com is ${barResponse.status}")
            }
          }
        } flatMap identity
      } flatMap identity
    }

  def sequentialGetPostFutureFuture(postId: Int, title: String, body: String, commentId: Int) =
    Action.async {
      FutureFactory { () =>
        Thread.sleep(20)
        FutureFactory { () =>

          val post = Post(postId, title)

          val foo = wsClient.url("https://my-json-server.typicode.com/typicode/demo/posts").post(post)

          foo.flatMap { fooResponse =>
            val comment = Comment(commentId, body, postId)
            // Use data in fooResponse to build the second request
            val bar = wsClient.url("https://my-json-server.typicode.com/typicode/demo/comments").post(comment)

            bar.map { barResponse =>
              // Now you can use barResponse and fooResponse to build a Result
              Ok(s"response for posts is ${fooResponse.status} & for comment is ${barResponse.status}")
            }
          }
        } flatMap identity
      } flatMap identity
    }

  case class SequentialPostFutureFutureRequest(message: String)

  // This is not getting instrumented properly
  def sequentialPostGetFutureFuture =
    Action.async {
      request =>

        implicit val fetchFeatureFlagsRequestFormat: Format[SequentialPostFutureFutureRequest] = format[SequentialPostFutureFutureRequest]

        val json = request.body.asJson.get
        val myRequest = json.as[SequentialPostFutureFutureRequest]

        println(s"Request payload: ${myRequest.message}")

        FutureFactory { () =>
          Thread.sleep(20)
          FutureFactory { () =>
            val foo = wsClient.url("http://www.yahoo.com").get()

            foo.flatMap { fooResponse =>
              // Use data in fooResponse to build the second request
              val bar = wsClient.url("http://www.google.com").get()

              bar.map { barResponse =>
                // Now you can use barResponse and fooResponse to build a Result
                Ok(s"response from yahoo.com is ${fooResponse.status} & from google.com is ${barResponse.status}")
              }
            }
          } flatMap identity
        } flatMap identity
    }

  // This is not getting instrumented properly
  def sequentialPostPostFutureFuture =
    Action.async {
      implicit request =>

        val json = request.body.asJson.get
        val myRequest = json.as[PostNComments]

        println(s"Request payload: ${myRequest}")

        FutureFactory { () =>
          Thread.sleep(20)
          FutureFactory { () =>

            val post = Post(myRequest.postId, myRequest.title)

            val foo = wsClient.url("https://my-json-server.typicode.com/typicode/demo/posts").post(post)

            foo.flatMap { fooResponse =>
              val comment = Comment(myRequest.commentId, myRequest.body, myRequest.postId)
              // Use data in fooResponse to build the second request
              val bar = wsClient.url("https://my-json-server.typicode.com/typicode/demo/comments").post(comment)

              bar.map { barResponse =>
                // Now you can use barResponse and fooResponse to build a Result
                Ok(s"response for posts is ${fooResponse.status} & for comment is ${barResponse.status}")
              }
            }
          } flatMap identity
        } flatMap identity
    }
}
