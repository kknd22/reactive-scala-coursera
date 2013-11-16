package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  val DeMuxDelay = 7

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

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux example") {
    val in = new Wire
    val c0, c1 = new Wire
    val cs = c1::c0::List()
    val r0, r1, r2, r3 = new Wire
    val out = r3::r2::r1::r0::List()

    demux(in, cs, out)

    in.setSignal(false)
/*    run

    assert(r3.getSignal === false, "0 and r3")
    assert(r2.getSignal === false, "0 and r2")
    assert(r1.getSignal === false, "0 and r1")
    assert(r0.getSignal === false, "0 and r0")
*/
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run

    assert(r3.getSignal === false, "1 and r3")
    assert(r2.getSignal === false, "1 and r2")
    assert(r1.getSignal === false, "1 and r1")
    assert(r0.getSignal === true, "1 and r0")

    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(r3.getSignal === true, "2 and r3" )
    assert(r2.getSignal === false, "2 and r2")
    assert(r1.getSignal === false, "2 and r1")
    assert(r0.getSignal === false, "2 and r0")


  }

}
