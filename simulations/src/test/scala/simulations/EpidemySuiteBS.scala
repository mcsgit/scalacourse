package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EpidemySuiteBS extends FunSuite {
  test("number of initially infected people"){
	  var sickTimes = 0
	  for(i <- 0 to 100){
			  val es = new EpidemySimulator
	
	      sickTimes = es.persons.filter(p => p.infected ).length
	      assert(sickTimes == es.SimConfig.population * es.SimConfig.initialInfectedRatio, "number of initially infected people:"+sickTimes+" vs "+es.SimConfig.population * es.SimConfig.initialInfectedRatio)
	      
	  }
  }

  test("An immune person should not become sick "){
	  var sickTimes = 0
	  for(i <- 0 to 100){
			  val es = new EpidemySimulator
			 for(p<-es.persons) {
			  //p.immune=true
			  //p.infected=true
			 }
	      while(es.agenda.head.time < 17) es.next
	
	      sickTimes = es.persons.filter(p => p.sick && p.immune ).length
	      assert(sickTimes == 0, "An immune person should not become sick")
	      
	  }
  }
}