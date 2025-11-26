package projects

import akka.actor.{Actor, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout
import spray.json._
import java.sql.{Connection, DriverManager, ResultSet}
import scala.concurrent.duration._
import akka.stream.ActorMaterializer


object DB {
  Class.forName("org.sqlite.JDBC")
  val conn: Connection = DriverManager.getConnection("jdbc:sqlite:users.db")

  val stmt = conn.createStatement()
  stmt.execute(
    """CREATE TABLE IF NOT EXISTS USERS(
      |id INTEGER PRIMARY KEY AUTOINCREMENT,
      |name TEXT NOT NULL UNIQUE,
      |password TEXT NOT NULL,
      |startTime TEXT NOT NULL,
      |created_at TEXT NOT NULL
      |)""".stripMargin
  )

  case class User(id: Int, name: String, password: String, startTime: String, created_at: String)

  def exists(name: String): Boolean = {
    val ps = conn.prepareStatement("SELECT COUNT(*) AS cnt FROM USERS WHERE name=?")
    ps.setString(1, name)
    val rs = ps.executeQuery()
    rs.next() && rs.getInt("cnt") > 0
  }


  def validatePassword(pass: String): Boolean = {
    pass.length > 8 &&
      pass.exists(_.isDigit) &&
      pass.exists(_.isLetter) &&
      pass.exists(ch => "!@#$%^&*()_+{}[]".contains(ch))
  }

  def insert(user: User): User = {
    val ps = conn.prepareStatement(
      "INSERT INTO USERS(name,password,startTime,created_at) VALUES(?,?,?,?)",
      java.sql.Statement.RETURN_GENERATED_KEYS
    )
    ps.setString(1, user.name)
    ps.setString(2, user.password)
    ps.setString(3, user.startTime)
    ps.setString(4, user.created_at)
    ps.executeUpdate()
    val keys = ps.getGeneratedKeys
    keys.next()
    user.copy(id = keys.getInt(1))
  }

  def update(id: Int, name: String, passwordOpt: Option[String]): Boolean = {
    val ps = passwordOpt match {
      case Some(p) => conn.prepareStatement("UPDATE USERS SET name=?, password=? WHERE id=?")
      case None    => conn.prepareStatement("UPDATE USERS SET name=? WHERE id=?")
    }
    ps.setString(1, name)
    passwordOpt.foreach(p => ps.setString(2, p))
    ps.setInt(passwordOpt.map(_ => 3).getOrElse(2), id)
    ps.executeUpdate() > 0
  }

  def get(id: Int): Option[User] = {
    val rs = stmt.executeQuery(s"SELECT * FROM USERS WHERE id=$id")
    if (rs.next()) Some(User(rs.getInt("id"), rs.getString("name"), rs.getString("password"), rs.getString("startTime"), rs.getString("created_at")))
    else None
  }

  def getAll: List[User] = {
    val rs = stmt.executeQuery("SELECT * FROM USERS")
    Iterator.continually(rs).takeWhile(_.next()).map { r =>
      User(r.getInt("id"), r.getString("name"), r.getString("password"), r.getString("startTime"), r.getString("created_at"))
    }.toList
  }

  def replace(id: Int, user: User): Boolean = {
    val ps = conn.prepareStatement(
      "UPDATE USERS SET name=?, password=?, startTime=?, created_at=? WHERE id=?"
    )
    ps.setString(1, user.name)
    ps.setString(2, user.password)
    ps.setString(3, user.startTime)
    ps.setString(4, user.created_at)
    ps.setInt(5, id)
    ps.executeUpdate() > 0
  }

  def delete(id: Int): Boolean = {
    val ps = conn.prepareStatement("DELETE FROM USERS WHERE id=?")
    ps.setInt(1, id)
    ps.executeUpdate() > 0
  }

}

trait JsonSupport extends DefaultJsonProtocol {
  implicit val userFormat = jsonFormat5(DB.User)
}

object UserActor {
  case class Create(name: String, password: String)
  case class Update(id: Int, name: String, passwordOpt: Option[String])
  case class Get(id: Int)
  case object GetAll
  case class Replace(id: Int, user: DB.User)
  case class Delete(id: Int)

}

class UserActor extends Actor {
  import UserActor._
  import DB._

  def receive: Receive = {
    case Create(name, password) =>
      sender() ! insert(User(0, name, password, java.time.Instant.now.toString, java.time.Instant.now.toString))
    case Update(id, name, passOpt) =>
      sender() ! update(id, name, passOpt)
    case Get(id) =>
      sender() ! get(id)
    case GetAll =>
      sender() ! getAll
    case Replace(id, user) =>
      sender() ! DB.replace(id, user)
    case Delete(id) =>
      sender() ! DB.delete(id)

  }
}

object Api extends App with JsonSupport {
  implicit val system = ActorSystem("system")
  implicit val materializer= ActorMaterializer()
  implicit val ec = system.dispatcher
  implicit val timeout = Timeout(5.seconds)

  val actor = system.actorOf(Props[UserActor], "userActor")


  import akka.http.scaladsl.model._
  import akka.http.scaladsl.server._

  val rejectionHandler = RejectionHandler.newBuilder()
    .handleNotFound {
      complete(
        HttpResponse(
          StatusCodes.NotFound,
          entity = HttpEntity(
            ContentTypes.`text/html(UTF-8)`,
            "<html><body><h2>Sorry bro, we don’t have anything at this URL.</h2></body></html>"
          )
        )
      )
    }
    .result()


  val exceptionHandler = ExceptionHandler {
    case ex: Throwable =>
      extractUri { uri =>
        println(s"ERRORRRR Request to $uri failed: ${ex.getMessage}")
        complete(
          HttpResponse(
            StatusCodes.InternalServerError,
            entity = HttpEntity(
              ContentTypes.`text/html(UTF-8)`,
              "<html><body><h2>Oops! We are fixing things. Please try again later.</h2></body></html>"
            )
          )
        )
      }
  }
  import UserActor._

  val route = handleRejections(rejectionHandler){
    handleExceptions(exceptionHandler) {
      {
        pathPrefix("users") {
          post {
            entity(as[String]) { body =>
              val js = body.parseJson.asJsObject
              val name = js.fields("name").convertTo[String]
              val pass = js.fields("password").convertTo[String]

              if (!DB.validatePassword(pass)) complete(400 -> "Invalid password")

              val result = (actor ? Create(name, pass)).mapTo[DB.User]
              complete(result.map(_.toJson.prettyPrint))
            }
          } ~
            patch {
              parameters('id.as[Int]) { id =>
                entity(as[String]) { body =>
                  val js = body.parseJson.asJsObject
                  val name = js.fields("name").convertTo[String]
                  val passOpt = js.fields.get("password").map(_.convertTo[String])
                  if (passOpt.exists(p => !DB.validatePassword(p))) complete(400 -> "Invalid password")
                  else{
                  val result = (actor ? Update(id, name, passOpt)).mapTo[Boolean]
                  complete(result.map(r => s"Updated = $r"))}
                }
              }
            } ~
            get {
              path(IntNumber) { id =>
                val result = (actor ? Get(id)).mapTo[Option[DB.User]]
                complete(result.map(_.toJson.prettyPrint))
              } ~
                pathEndOrSingleSlash {

                  parameter("username") { username =>
                    if (!DB.exists(username)) complete(401 -> "Unauthorized")
                    else {
                      val result = (actor ? GetAll).mapTo[List[DB.User]]
                      complete(result.map(_.toJson.prettyPrint))
                    }
                  }
                }
            }~put {
            parameters('id.as[Int]) { id =>
              entity(as[String]) { body =>
                val js = body.parseJson.asJsObject
                val name = js.fields("name").convertTo[String]
                val pass = js.fields("password").convertTo[String]
                if (!DB.validatePassword(pass)) complete(400 -> "Invalid password")
                val user = DB.User(id, name, pass, java.time.Instant.now.toString, java.time.Instant.now.toString)
                val result = (actor ? Replace(id, user)).mapTo[Boolean]
                complete(result.map(r => s"Replaced = $r"))
              }
            }
          }~delete {
            parameters('id.as[Int]) { id =>
              val result = (actor ? Delete(id)).mapTo[Boolean]
              complete(result.map(r => s"Deleted = $r"))
            }
          }
        }
      }
    }
  }



  Http().bindAndHandle(route, "localhost", 8000)
  println("Server running at http://localhost:8000")

}
