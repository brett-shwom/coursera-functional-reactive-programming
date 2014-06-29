package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate: Double = 0.01
    val transmissibilityRate : Double = 0.4
    val probabilityOfDeath : Double = 0.25

    // to complete: additional parameters of simulation
  }

  import SimConfig._


  val numberOfInitialInfectedPersons = (prevalenceRate * population).toInt

  val allIds = List.range(0,population)

  val idsOfInfectedPersons = Random.shuffle(allIds).slice(0,numberOfInitialInfectedPersons)

  val persons: List[Person] = List  
                                .range(0,population)
                                .map({ (id: Int) =>
                                  new Person(
                                    id = id,
                                    infected = idsOfInfectedPersons.contains(id),
                                    col = randomBelow(roomRows),
                                    row = randomBelow(roomColumns)
                                    
                                  )
                                })



  // println("infected", persons.filter(_.infected).length)

  class Person (val id: Int, var infected : Boolean, var col : Int, var row : Int) {

    var sick = false
    var immune = false
    var dead = false


    def setupNextMove = {
      afterDelay(randomBelow(4) + 1)({
        move()
      })
    }

    def startInfectionProcess : Unit = {
      afterDelay(6)({
        //println("someone is getting sick",currentTime)
        sick = true
      })
      afterDelay(14)({
        //println("someone might die")
        dead = random <= probabilityOfDeath
        //println("dead", dead)
      })
      afterDelay(16)({
        if (!dead) {
          // println("someone is becoming immune")
          immune = true
          sick = false
        }
      })
      afterDelay(18)({
        if (!dead) {
          // println("someone is no longer infected")
          infected = false
          immune = false  //should immune be false here?
        }
       
      })
    }

    def move() {

      if (!dead) {

        val up = ((row + 1) % roomRows,col)
        val down = (((row-1) % roomRows + roomRows) % roomRows,col)
        val left = (row,((col-1) % roomColumns + roomColumns) % roomColumns)
        val right = (row,(col + 1) % roomColumns)

        val adjacentRooms = List[(Int,Int)](up,down,left,right)

        val adjacentRoomsWithoutSickOrDeadPeople = 
          adjacentRooms.filter({ case (newRow , newCol ) =>
            !persons
              .filter({person => person.row == newRow && person.col == newCol})
              .exists({person => person.sick || person.dead})
          })

        if (!adjacentRoomsWithoutSickOrDeadPeople.isEmpty) { //dont move if all of the adjacent rooms have sick or dead people
          

          val (newRow : Int,newCol : Int) = Random.shuffle(adjacentRoomsWithoutSickOrDeadPeople).head
          row = newRow
          col = newCol

          val anyInfectedRoomates = persons
                            .filter({person => person.row == row && person.col == col})
                            .exists({person => person.infected})

          if (anyInfectedRoomates && !infected && !immune ) {  //person becomes infected
            infected = random <= transmissibilityRate
            if (infected) {
              //println("someone is infected", dead)
              startInfectionProcess
            }
          }
        }

        setupNextMove

      }

    }

    if (infected) startInfectionProcess //if person is infected when the simulation starts
    setupNextMove

  }



}
