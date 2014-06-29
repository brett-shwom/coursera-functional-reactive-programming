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

  test("simple demux example") {
    //simple example is with no control wires
    val in1, out1 = new Wire

    demux(in1,List[Wire](),List[Wire](out1))

    in1.setSignal(true)
    run
    
    assert(out1.getSignal === true, "true")


    val in2, out2 = new Wire

    demux(in2,List[Wire](),List[Wire](out2))

    in2.setSignal(false)
    run
    
    assert(out2.getSignal === false, "false")


  }

  test("more complex demux example") {
    //simple example is with no control wires
    val in1, out11,out12,out13,out14,out15,out16,out17,out18 = new Wire

    val signal11,signal12,signal13 = new Wire
    signal11.setSignal(false)
    signal12.setSignal(true)
    signal13.setSignal(false)

    val out1 = (List[Wire](out11,out12,out13,out14,out15,out16,out17,out18))

    demux(in1,List[Wire](signal11,signal12,signal13),out1)

    in1.setSignal(true)
    run


    assert(
      out1
        .map(x => x.getSignal)
        .map(x => if (x) 1 else 0 )
        .foldLeft(0)(_+_) == 1
     , "adds to 1 (i.e. only 1 bit is set to true)")

  }



}
