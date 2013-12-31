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
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    in1.setSignal(true)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 4")
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

    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
    in1.setSignal(true)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 4")
  }

  test("demux example") {
    val in = new Wire
    val out=List[Wire](new Wire, new Wire, new Wire, new Wire)
    val c=List[Wire](new Wire, new Wire)
    def signals(listwires:List[Wire]):List[Boolean]=
    {
    	def withAcc(l:List[Wire], acc:List[Boolean]):List[Boolean]=
    	{
    	  if(!l.isEmpty)
    	    withAcc(l.tail, acc:+l.head.getSignal)
    	  else
    	     acc
    	}
    	withAcc(listwires, Nil)
    }
    demux(in, c, out)
    in.setSignal(true)
    c.head.setSignal(false)
    c.tail.head.setSignal(false)
    run
    assert(signals(out) === List(false, false, false, true), "demux 4.1")
  }
  test ("demux forum"){
def demux0 {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(false)
    run
    
    assert(out.getSignal == false, "demux0 1")

    in.setSignal(true)
    run
    
    assert(out.getSignal == true, "demux0 2")
  }
  
   def demux1 {
    val in, c, o0, o1 = new Wire
    demux(in, List(c), List(o1,o0))
    in.setSignal(false)
    c.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux1 1")
    assert(o1.getSignal == false, "demux1 1")

    in.setSignal(true)
    run
    
    assert(o0.getSignal == true, "demux1 2")
    assert(o1.getSignal == false, "demux1 2")
    
    
    c.setSignal(true)
    run
    
    assert(o0.getSignal == false, "demux1 3")
    assert(o1.getSignal == true, "demux1 3")
    
    in.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux1 4")
    assert(o1.getSignal == false, "demux1 4")
  }
   
   def demux2 {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1,c0), List(o3,o2,o1,o0))
    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux2 1")
    assert(o1.getSignal == false, "demux2 1")
    assert(o2.getSignal == false, "demux2 1")
    assert(o3.getSignal == false, "demux2 1")

    in.setSignal(true)
    run
    
    assert(o0.getSignal == true, "demux2 2")
    assert(o1.getSignal == false, "demux2 2")
    assert(o2.getSignal == false, "demux2 2")
    assert(o3.getSignal == false, "demux2 2")
    
    
    c1.setSignal(true)
    run
    
    assert(o0.getSignal == false, "demux2 3")
    assert(o1.getSignal == false, "demux2 3")
    assert(o2.getSignal == true, "demux2 3")
    assert(o3.getSignal == false, "demux2 3")
    
    in.setSignal(false)
    run
    
    assert(o0.getSignal == false, "demux2 4")
    assert(o1.getSignal == false, "demux2 4")
    assert(o2.getSignal == false, "demux2 4")
    assert(o3.getSignal == false, "demux2 4")
  }
   demux2
  }
}
