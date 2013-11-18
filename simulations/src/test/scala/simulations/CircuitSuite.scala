package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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
     val cs = List.fill(3){new Wire}
     val out = List.fill(8){ new Wire}

     demux(in, cs, out)
     in.setSignal(true)

     /*
          cs.head.setSignal(false)
          cs.tail.head.setSignal(false)
          cs.tail.tail.head.setSignal(true)


         run

          print(out.map(_.getSignal))
         val aout = out.toArray
         for (i<- 0 to 7) {
           if ( i != 1)
             assert(aout(i).getSignal === false, s"3-8 gate and $i")
           else
             assert(aout(i).getSignal === true, s"3-8 gate and $i")
         }
     */
     cs.head.setSignal(true)
     cs.tail.head.setSignal(false)
     cs.tail.tail.head.setSignal(true)
     run
     print(out map(_.getSignal))

     val aout2 = out.toArray
     for (i<- 0 to 7) {
       if ( i != 5)
         assert(aout2(i).getSignal === false, s"3-8 gate and $i")
       else
         assert(aout2(i).getSignal === true, s"3-8 gate and $i")
     }

  }
}
