package simulations

import common._
import org.scalacheck.Properties
import org.scalacheck._
import Arbitrary._
import Prop._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

trait CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire): Unit = {
    val a1Inverted, a2Inverted, outputInverted = new Wire
    inverter(a1, a1Inverted)
    inverter(a2, a2Inverted)
    andGate(a1Inverted, a2Inverted, outputInverted)
    inverter(outputInverted, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if (out.length != Math.pow(2, c.length)) throw new IllegalArgumentException

    c match {
      case List() => in addAction (() => out(0).setSignal(in.getSignal))
      case List(c0) => {
        val c0Inverted = new Wire
        inverter(c0, c0Inverted)
        andGate(in, c0Inverted, out(0))
        andGate(in, c0, out(1))
      }
      case cs => {
        val io0, io1 = new Wire
        demux(in, List(cs.last), List(io0, io1))
        val half = out.length / 2
        demux(io0, cs.init, out take half)
        demux(io1, cs.init, out drop half)
      }
    }
  }
}

abstract class QuickCheckSimulator extends Properties("CircuitSimulator") with CircuitSimulator {

  val controls = for (n <- Gen.choose(0, 10)) yield n
  val output = for (n <- Gen.choose(0, 1023)) yield n

  property("demux") = forAll (controls, output){ (c: Int, o: Int) =>
    val in = new Wire
    val cs = List.fill(c)(new Wire)
    val n = Math.pow(2, c).toInt
    val outs = List.fill(n)(new Wire)

    demux(in, cs, outs)
    in.setSignal(true)

    val i = Math.abs(o % n)
    cs zip toBoolList(i) foreach { case (c, s) => c.setSignal(s) }
    run

//    println(f"--- test with ${c}%s controls and ${n}%s outputs and signal set for ${i}%s ---")
//    cs.zipWithIndex foreach {case (w, index) =>println(f"c${index}%s = ${w.getSignal}%s")}
//    outs.zipWithIndex foreach {case (w, index) => println(f"o${index}%s = ${w.getSignal}%s")}

    outs(i).getSignal
  }

  def toBoolList(i: Int): List[Boolean] = {
    val r = (i % 2) != 0
    val c = i / 2
    if (c == 0)
      List(r)
    else
      r :: toBoolList(i / 2)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def demuxExample: Unit = {
    val in = new Wire
    val cs = List.fill(3)(new Wire)
    val outs = List.fill(8)(new Wire)

    demux(in, cs, outs)
    in.setSignal(true)

    probe("c0", cs(0))
    probe("c1", cs(1))
    probe("c2", cs(2))

    0 to 7 foreach { i =>
      probe(f"out$i%d", outs(i))
      cs(0).setSignal (i % 2 != 0)
      cs(1).setSignal (((i / 2) % 2) != 0)
      cs(2).setSignal ((((i / 2) / 2) % 2) != 0)

      run
    }
  }

}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  println("andGateExample")
  Circuit.andGateExample

  println("orGate2Example")
  Circuit.orGate2Example
  Circuit.demuxExample
}
