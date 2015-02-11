package simulations

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  test("demux with 0 control wires and 1 out wire") {
    val in, out = new Wire
    demux(in, List(), List(out))
    in.setSignal(false)
    run

    assert(out.getSignal === false)

    in.setSignal(true)
    run

    assert(out.getSignal === true)
  }

  test("demux must have correct number of c and out wires") {
    val in, c, out = new Wire
    intercept[IllegalArgumentException]{
      demux(in, List(c), List(out))
    }
  }

  test("demux with 1 control wire and 2 out wires") {
    val in, c, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))
    in.setSignal(false)
    c.setSignal(false)
    run

    assert(out1.getSignal === false)
    assert(out2.getSignal === false)

    in.setSignal(true)
    run

    assert(out1.getSignal === true)
    assert(out2.getSignal === false)

    c.setSignal(true)
    run

    assert(out1.getSignal === false)
    assert(out2.getSignal === true)

    in.setSignal(false)
    run

    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
  }

  test("demux with 3 control wires") {
    val in = new Wire
    val cs = List.fill(3)(new Wire)
    val outs = List.fill(8)(new Wire)

    demux(in, cs, outs)
    in.setSignal(true)

    0 to 7 foreach { i =>
      cs(0).setSignal(i % 2 != 0)
      cs(1).setSignal(((i / 2) % 2) != 0)
      cs(2).setSignal((((i / 2) / 2) % 2) != 0)
      run

      assert(outs(i).getSignal === true)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class CircuitSimulatorQuickCheckSuite extends FunSuite with Checkers {

  test("demux works as expected") {
    check(new QuickCheckSimulator with CircuitSimulator {
      override val InverterDelay: Int = 1
      override val AndGateDelay: Int = 3
      override val OrGateDelay: Int = 5
    })
  }
}
