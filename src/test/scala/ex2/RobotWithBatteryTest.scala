package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatteryTest extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "stop moving when battery is 0" in:
    val robot = new RobotWithBattery(new SimpleRobot((0, 0), Direction.North))
    robot.act()
    robot.act()
    robot.act()
    robot.position shouldBe (0, 2)