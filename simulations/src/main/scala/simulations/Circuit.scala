package simulations

import common._

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

abstract class CircuitSimulator extends Simulator {

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

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1Inv , a2Inv, a1a2InvAnd = new Wire
    inverter(a1, a1Inv)
    inverter(a2, a2Inv)
    andGate(a1Inv, a2Inv, a1a2InvAnd)
    inverter(a1a2InvAnd, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => {
        andGate(in, in,  out.head)
      }
      case r::Nil => {
        andGate(in, r, out.tail.head)
        val w = new Wire
        inverter(r, w)
        andGate(in, w, out.head)
      }
      case r::rs => {
        val s = out.size/2
        val ts = List.fill(s)(new Wire)
        demux(in, rs, ts)
        var l = out
        ts.foreach { e =>
          demux(e, r::Nil, l)
          l = l.tail.tail
        }
      }
    }
  }
/*
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    val rc = c.reverse
    rc match {
      case Nil => {
        andGate(in, in,  out.head)
      }
      case r::rs => {
        val s = out.size
        val h1 = out.take(s/2)
        val h2 = out.drop(s/2)
        if (!r.getSignal) {
          h1.foreach(andGate(in, r, _))
          demux(in, rs.reverse, h2)
        } else {
          val rNot = new Wire
          inverter(r, rNot)
          h2.foreach(andGate(in, rNot, _))
          demux(in, rs.reverse, h1)
        }
      }
    }
  }
  */
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

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
