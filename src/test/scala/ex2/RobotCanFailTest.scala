package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailTest extends AnyFlatSpec with Matchers:
  "A RobotCanFail with 100% failure probability" should "never execute actions" in :
    // Probabilità 1.0 = fallisce sempre
    val robot = new RobotCanFail(new SimpleRobot((0, 0), Direction.North), 100)
    robot.act()
    robot.act()
    robot.position shouldBe(0, 0) // Rimanere incollato alla partenza

  "A RobotCanFail with 0% failure probability" should "always execute actions" in :
    // Probabilità 0.0 = non fallisce mai
    val robot = new RobotCanFail(new SimpleRobot((0, 0), Direction.North), 0)
    robot.act()
    robot.act()
    robot.position shouldBe(0, 2)