package projects

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import projects.com._

class BankActorSpec
  extends TestKit(ActorSystem("TestSystem"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A Bank Actor" should {
    "create an account and return zero balance" in {
      val probe = TestProbe()
      val bank = system.actorOf(Props[bankSupervisor])

      bank ! addacc(1)
      probe.send(bank, tellbal(1))
      probe.expectMsg(0)
    }
    "deposit correctly" in {
      val probe = TestProbe()
      val bank = system.actorOf(Props[bankSupervisor])

      bank ! addacc(1)
      bank ! dep(1, 500)
      probe.send(bank, tellbal(1))
      probe.expectMsg(500)
    }

    "withdraw correctly" in {
      val probe = TestProbe()
      val bank = system.actorOf(Props[bankSupervisor])
      bank ! addacc(2)
      bank ! dep(2, 500)
      bank ! wdr(2, 200)
      probe.send(bank, tellbal(2))
      probe.expectMsg(300)
    }
    "respond with error when account does not exist" in {
      val probe = TestProbe()
      val bank = system.actorOf(Props[bankSupervisor])
      probe.send(bank, tellbal(99))
      probe.expectMsg("Acc dont exists with id 99")
    }
  }
}
