import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.io.Source
import java.io.FileWriter
import java.io.IOException

object AssignmentPart1 extends App {

  case class readFromFile(actorRef0: ActorRef, actorRef1: ActorRef, source_file : scala.io.BufferedSource)
  case class MarkLabel(actorRef: ActorRef, line : String)
  case class ModifyLine(mark_line : String)
  case class WriteLabel(final_line : String)

  class CountActor extends Actor {
    def receive: Receive = {
      case readFromFile(actor1, actor2, source_file) =>
        for (line <- source_file.getLines()) {
          actor1 ! MarkLabel(actor2, line)
        }

      case MarkLabel(actor2, line) =>

        var tmp_line = line
		val label_type_new = "@typ_new_"
		val label_init_new = "@ini_new_"

        if (!line.contains("return")) {
		  if (line.contains(".0")) {                                    // float
			  tmp_line = label_init_new + "float_new@" + tmp_line
			if (!line.contains("float")){
				tmp_line = label_type_new + "float_new@" + tmp_line
			}  
		  }
		  else if (line.contains("= 0;")) {                             // int
			  tmp_line = label_init_new + "int_new@" + tmp_line
			if (!line.contains("int")) {
				tmp_line = label_type_new + "int_new@" + tmp_line
			}  
		  }
		  else if (line.contains("true") || line.contains("false")) {   // bool
			  tmp_line = label_type_new + "bool_new@" + tmp_line
		  }
		  else if (line.contains("= \"")) {                             // char
			  tmp_line = label_type_new + "char_new@" + tmp_line
		  }
		  else if (line.contains("hate") || line.contains("Hate")) {    // smth strange
			  tmp_line = label_type_new + "only_love@" + tmp_line
		  }
        } 

        actor2 ! ModifyLine(tmp_line)

      case ModifyLine(mark_line) =>
        var tmp_marked_str = mark_line
        if (!tmp_marked_str.contains("return")) {
//                 *********** type *****************		
			if (tmp_marked_str.contains("@typ")){
			  if (tmp_marked_str.contains("@typ_new_bool_new@")){
				tmp_marked_str = tmp_marked_str.replace("@typ_new_bool_new@", "bool ")  
			  }	
			  if (tmp_marked_str.contains("@typ_new_char_new@")){
				tmp_marked_str = tmp_marked_str.replace("@typ_new_char_new@", "char ")  
			  }
			  if (tmp_marked_str.contains("@typ_new_only_love@")){
				tmp_marked_str = tmp_marked_str.replace("@typ_new_only_love@", "").replace("hate", "love")
			  }
			  if (tmp_marked_str.contains("@typ_new_float_new@")){
				tmp_marked_str = tmp_marked_str.replace("@typ_new_float_new@", "float")  
			  }
			  if (tmp_marked_str.contains("@typ_new_int_new@")) {
				tmp_marked_str = tmp_marked_str.replace("@typ_new_int_new@", "int")  
			  }
			}
//               *********** init ***************			
			if (tmp_marked_str.contains("@ini")) {
			  if (tmp_marked_str.contains("@ini_new_float_new@")){
				tmp_marked_str = tmp_marked_str.replace("@ini_new_float_new@", "").replace(".0", ".111")  
			  }
			  if (tmp_marked_str.contains("@ini_new_int_new@")) {
				tmp_marked_str = tmp_marked_str.replace("@ini_new_int_new@", "").replace("= 0;", "= 111;")  
			  }
			}
        } 
		
        sender() ! WriteLabel(tmp_marked_str)

      case WriteLabel(final_line) =>
        println(final_line)
        val writer = new FileWriter("text_changed.txt", true)
        try {
          writer.write(final_line)
          writer.append('\n')
          writer.flush()
        } catch {
          case ex: IOException =>
            System.out.println(ex.getMessage)
        } finally if (writer != null) writer.close()
    }
  }

  val source_file = Source.fromFile("text.txt")
  val system = ActorSystem("parser")
  val actor0 = system.actorOf(Props[CountActor], name = "parser-reader")
  val actor1 = system.actorOf(Props[CountActor], name = "parser-labeler")
  val actor2 = system.actorOf(Props[CountActor], name = "parser-replacer")

  actor0 ! readFromFile(actor1, actor2, source_file)
}
