package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedTest extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "execute the action N times for each call" in:
    val robot = new RobotRepeated(new SimpleRobot((0, 0), Direction.North), 3)
    robot.act()
    robot.position shouldBe(0, 3)

    robot.act()
    robot.position shouldBe(0, 6)