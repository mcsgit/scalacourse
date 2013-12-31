package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val initialInfectedRatio: Double=0.01
    val MaxMoveDelay: Int=5
    val SickDelay: Int=6
    val DeathDelay: Int=14
    val ImmuneDelay: Int=16
    val HealthDelay: Int=18
    val InfectionProbability=40
    val DeathProbability=25

  }

  import SimConfig._

  val allpopulation=List.range(1, population)
  val persons: List[Person] = for(i <- allpopulation)  yield (new Person(i))
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var gotInfection=false
    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    if (id<=population*initialInfectedRatio)
      infected=true

    def setInfected(s: Boolean)= {
	    if (s  &&  !immune && !dead && !gotInfection) {
	      gotInfection=true
	      afterDelay(SickDelay){setSick(true)}
          afterDelay(DeathDelay){ if (getProbability(DeathProbability)) setDead(true)}
	      afterDelay(ImmuneDelay){ setImmune(true)}
	      afterDelay(HealthDelay){ setHealthy(true)}
	      infected=s;
	    }else
	    {
	      
	    }
        
	}
	    
	def setSick(s:Boolean)={
	  if(!dead /*&& s && !immune*/)
	  {
	    if(s && !immune)
		  sick=true
		else
		    sick=false
	  }
	}
	
	def setDead(s:Boolean)={
	    dead=s
	}
	def setImmune(s:Boolean)={
	  if(!dead )
	  {
		  immune=s
		  if(s)
			  sick = false;
	  }
	}
	def setHealthy(s:Boolean)={
	  if(!dead){
		  immune=false
		  infected=false
		  sick=false
		  gotInfection=false
	  }
	}
	def rowUp():Int={if(row==0) roomRows-1 else row-1}
	def rowDown():Int={if(row==(roomRows-1)) 0 else row+1}
	def colLeft():Int={if(col==0) roomColumns-1 else col-1}
	def colRight():Int={if(col==(roomColumns-1)) 0 else col+1}
	def neighbors: List[(Int,Int) ] = List((rowUp, col),(rowDown, col),(row,colLeft),(row,colRight)).filter(isLocationOK)

	def isLocationOK(pos: (Int, Int)):Boolean=
	{
	  val g = persons.filter( p => p.row==pos._1 && p.col==pos._2 && (p.sick && p.dead))
	  g.isEmpty
	}
	def locationIsInfected(pos: (Int, Int)):Boolean=
	{
	  val g = persons.filter( p => p.row==pos._1 && p.col==pos._2 && (p.sick || p.infected || p.dead))
	  !g.isEmpty
	}
	
	def getNextDayToMove(): Int={
	  randomBelow(MaxMoveDelay-1)+1
	}
	
	
  }//end of person
	def move(p: Person):Boolean={
	  if(p.dead)
	    false
	    else{
		  val l=p.neighbors.length
		  if(l>0)
		  {
			val idx:Int=randomBelow(l)
		    val nextPos=p.neighbors(idx)
			p.row=nextPos._1
			p.col=nextPos._2
			if(p.locationIsInfected(p.row,p.col))
			  { if(getProbability(InfectionProbability) && !p.gotInfection) p.setInfected(true)}
		  }
		  mode(p)
		  true
	    }
	}
	def getProbability(margin:Double):Boolean={
	  val sample=randomBelow(101)
	  margin>=sample
	}
	def mode(p: Person)={
	  if(!p.dead)
	  {
	    afterDelay(p.getNextDayToMove()){move(p)}
		  if(!p.gotInfection && p.infected)
		    p.setInfected(true)
	  }
	}
	def initPersons()
	{
	  for(p<-persons) mode(p)
	}
	afterDelay(0){
	  initPersons
	}
}
