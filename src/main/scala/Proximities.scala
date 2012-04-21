import scala.util.parsing.json.JSON
import java.text.SimpleDateFormat
import java.util.Date
import java.util.NoSuchElementException



class Proximities(jsons: Iterator[String]){
  // データが大きいので効率を考えmutable実装とする
  private val container = collection.mutable.Map[Int,List[(Int, Date, Array[Int])]]()
  
  JSON.globalNumberParser = {input:String => input.toInt} // override behavior of parser for Numeric.
  for(json <- jsons) { // ToDo:型変換大量に必要なのどうにかしたい
    val Some(parsed_json) = JSON.parseFull(json).asInstanceOf[Option[Map[String,Any]]]
    val seq = parsed_json("Sequence").asInstanceOf[Int]
    val date = new SimpleDateFormat("\"yyy/MM/dd HH:mm:ss\"\n").parse(parsed_json("Time").asInstanceOf[String])
    val res = parsed_json("Result").asInstanceOf[Map[String,Any]]
    val user_id = res("Sample").asInstanceOf[Int]
    val topk = res("Topk").asInstanceOf[List[Int]].toArray
    
    val tupple = (seq,date,topk)
    val new_tupple = (container.get(user_id) match {
      case Some(s) => tupple :: s
      case None => tupple :: Nil
    })
    container += user_id -> new_tupple
  }
  container.transform((k,v) => v.reverse)
  

  def get_users:Set[Int] = container.keySet.toSet

  def find(user_id: Int):(Int,List[(Int, Date, Array[Int])]) = (user_id,container(user_id))
  
  def find():(Int,List[(Int, Date, Array[Int])]) = next // fix?

  private var user_iterator = get_users.iterator
  def next():(Int,List[(Int, Date, Array[Int])]) = {
    try {
      val user_id = user_iterator.next
      (user_id,container(user_id))
    } catch {
      case e:NoSuchElementException => {
	user_iterator = get_users.iterator
	val user_id = user_iterator.next
	(user_id,container(user_id)) // ToDo:中括弧外す
      }
    }
  }
  

}
