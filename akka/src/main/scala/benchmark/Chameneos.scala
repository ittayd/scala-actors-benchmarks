/* The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   contributed by Julien Gaugaz
   inspired by the version contributed by Yura Taras and modified by Isaac Gouy
*/
package benchmark;

import se.scalablesolutions.akka.actor.{SupervisorFactory, Actor}
case class Exit(actor: Actor, reason: String)

object chameneos {
  
  abstract class Colour
  case object RED extends Colour
  case object YELLOW extends Colour
  case object BLUE extends Colour
  case object FADED extends Colour
  
  val colours = Array(BLUE, RED, YELLOW)
  
  case class Meet(from: Actor, colour:Colour)
  case class Change(colour:Colour)
  case class MeetingCount(count:int)
  
  
  class Mall(var n: int, numChameneos: int) extends Actor {
    var waitingChameneo:Option[Actor] = None
    var sumMeetings = 0
    var numFaded = 0
    
    start
    startChameneos()
    
    def startChameneos(): Unit = {
      var i = 0
      while(i < numChameneos) {
        Chameneo(this, colours(i%3), i).start
        i = i + 1
      }
    }
    
    override def receive: PartialFunction[Any, Unit] = {
          case MeetingCount(i) => {
            numFaded = numFaded + 1
            sumMeetings = sumMeetings + i
            if(numFaded == numChameneos) {
              println(sumMeetings)
              exit
            }
          }
          
          case msg@Meet(a, c) => {
            if(n > 0) {
	      waitingChameneo match {
                case Some(chameneo) =>
                  n = n-1
                  chameneo ! msg 
                  waitingChameneo = None
                case None =>
                  waitingChameneo = sender
              }
            } else {
              waitingChameneo match {
                case Some(chameneo) =>
                  chameneo!Exit(this, "normal")
                case None => 
              }
              sender.get!Exit(this, "normal")
            }
          }
	
          
    }
  }
  
  case class Chameneo(var mall: Mall, var colour: Colour, cid:int) extends Actor {
    var meetings = 0
    override def start = {
      val r = super.start
      mall!Meet(this, colour)
      r
    }

    override def receive: PartialFunction[Any, Unit] = {
          case Meet(from, otherColour) =>
            colour = complement(otherColour)
            meetings = meetings +1
            from!Change(colour)
            mall!Meet(this,colour)	
          case Change(newColour) =>
            colour = newColour
            meetings = meetings +1
            mall!Meet(this,colour)	
          case Exit(_,_) =>
	    colour = FADED
            sender.get!MeetingCount(meetings)
            exit
    }
    
    def complement(otherColour:Colour): Colour = {
      colour match {
      case RED => otherColour match {
        case RED => RED
        case YELLOW => BLUE
        case BLUE => YELLOW
        case FADED => FADED
      }
      case YELLOW => otherColour match {
        case RED => BLUE
        case YELLOW => YELLOW
        case BLUE => RED
        case FADED => FADED
      }
      case BLUE => otherColour match {
        case RED => YELLOW
        case YELLOW => RED
        case BLUE => BLUE
        case FADED => FADED
      }
      case FADED => FADED
      }
    }
    override def toString() = cid+"("+colour+")"
  }
  
  def main(args : Array[String]) : Unit = {
    if(args.length < 1) throw new IllegalArgumentException("Syntax: scala chameneos N [numChameneos]")
    val N = Integer.parseInt(args(0))
    var numChameneos = 4
    if(args.length == 2)
      numChameneos = Integer.parseInt(args(1))
    new Mall(N, numChameneos)
  }
}
