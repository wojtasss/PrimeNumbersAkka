import akka.actor._

case class AddNumber(n: Int)
case class AssignNumber(n: Int)
case class SendNumber(n: Int)
case object Init

class NumberActor extends Actor {
	var number: Int = 0
	var nextNumberRef: ActorRef = null
	def receive = {
		case Init => {
			number = 2
			NumberActor.firstNumberRef = self
			NumberActor.lastNumberRef = self
			NumberActor.firstNumberRef ! SendNumber(number+1)
			println(number)
		}
		case SendNumber(n: Int) => {
			if ((n % number) == 0) {
				NumberActor.lastNumberRef ! AddNumber(n)
			}
			else if ((n % number) != 0 && nextNumberRef == null) {
				NumberActor.lastNumberRef ! AddNumber(n)
				println(n)
			}
			else if ((n % number) != 0 && nextNumberRef != null) {
				nextNumberRef ! SendNumber(n)
			}
		}
		case AddNumber(n: Int) => {
			nextNumberRef = context.actorOf(Props[NumberActor], "Number"+n)
			nextNumberRef ! AssignNumber(n)
			NumberActor.lastNumberRef = nextNumberRef
			NumberActor.firstNumberRef ! SendNumber(n+1)
		}
		case AssignNumber(n: Int) => {
			number = n
		}
	}
}

object NumberActor {
	var firstNumberRef: ActorRef =  null
	var lastNumberRef: ActorRef =  null
}

object Main {
	def main(args: Array[String]): Unit = {
		val system = ActorSystem("system")				
		val prime = system.actorOf(Props[NumberActor], 2.toString)
		prime ! Init
	}
}
