package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25
  }

  import SimConfig._

  agenda = (1 to 100).map(t => new WorkItem(t, () => persons.foreach(p => p.updateState))).toList

  val persons: List[Person] = {
    val ps = List.range(0, SimConfig.population).map(id => new Person(id))

    def infectRandomPerson: Unit = {
      val p = ps(randomBelow(population))
      if(p.infected) infectRandomPerson
      else p.infected = true
    }

    def infectInitialPersons: Unit = {
      if (ps.count(p => p.infected) < population * prevalenceRate) {
        infectRandomPerson
        infectInitialPersons
      }
    }

    infectInitialPersons
    ps
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    var shouldMove = false;

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var deathChance = dieRate

    var timeSinceInfected = 0

    var timeSinceWantedToMove = 0
    var timeToMove = Math.min(1, randomBelow(5))

    def updateState = {
      updateHealthState
      move
    }

    def move: Unit = {
      if (shouldMove)
        shouldMove = false

      timeSinceWantedToMove += 1
      if (timeSinceWantedToMove == timeToMove) {
        timeToMove = Math.min(1, randomBelow(5))
        shouldMove = true
        timeSinceWantedToMove = 0

        val direction = randomBelow(3);
        if (direction == 0) row = (row + 1) % roomRows
        if (direction == 1) row = (row - 1 + roomRows) % roomRows
        if (direction == 2) col = (col + 1) % roomColumns
        if (direction == 1) col = (col - 1 + roomColumns) % roomColumns
      }
    }

    def updateHealthState: Unit = {
      if (infected)
        timeSinceInfected += 1

      if (timeSinceInfected == incubationTime)
        sick = true

      if (timeSinceInfected == dieTime && !dead)
        dead = shouldDie

      if (!dead) {
        if (timeSinceInfected == immuneTime)
          immune = true

        if (timeSinceInfected == healTime) {
          infected = false
          sick = false
          immune = false
        }
      }
    }

    def shouldDie: Boolean = {
      randomBelow(100) < (deathChance * 100)
    }
  }
}
