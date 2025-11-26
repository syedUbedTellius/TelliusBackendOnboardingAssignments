package projects

import akka.actor._
import java.io.{File, FileWriter, PrintWriter}
import java.time.LocalDateTime
case class LogInfo(msg: String)
case class LogWarn(msg: String)
case object Rotate

class logger extends Actor {
  private val infoFile = new File("info.log")
  private val warnFile = new File("warn.log")
  private var infoWriter = new PrintWriter(new FileWriter(infoFile, true))
  private var warnWriter = new PrintWriter(new FileWriter(warnFile, true))

  override def postStop(): Unit = {
    infoWriter.close()
    warnWriter.close()
  }
  def rotateFile(f: File): Unit = {
    val newName = s"${f.getName}_${System.currentTimeMillis()}"
    f.renameTo(new File(newName))
  }

  override def receive: Receive = {
    case LogInfo(msg) =>
      infoWriter.println(s"INFO ${LocalDateTime.now()} - $msg")
      infoWriter.flush()

    case LogWarn(msg) =>
      warnWriter.println(s"WARN ${LocalDateTime.now()} - $msg")
      warnWriter.flush()

    case Rotate =>
      infoWriter.close()
      warnWriter.close()
      rotateFile(infoFile)
      rotateFile(warnFile)
      infoWriter = new PrintWriter(new FileWriter(infoFile, true))
      warnWriter = new PrintWriter(new FileWriter(warnFile, true))

  }
}

class LoggerSupervisor extends Actor {
  override val supervisorStrategy: SupervisorStrategy =
    OneForOneStrategy() {
      case _: Exception => SupervisorStrategy.Restart
    }
  private val logger = context.actorOf(Props[logger], "logger")

  def receive: Receive = {
    case msg => logger forward msg
  }
}

object LoggerMain extends App {
  val system = ActorSystem("LogSystem")
  val supervisor = system.actorOf(Props[LoggerSupervisor], "supervisor")

  supervisor ! LogInfo("Application start.....")
  supervisor ! LogWarn("Warning....")
  supervisor ! LogInfo("Some Info.....")
  supervisor ! Rotate
  supervisor ! LogWarn("Last.....")
}
