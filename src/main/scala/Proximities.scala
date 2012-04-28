import scala.util.parsing.json.JSON
import java.text.SimpleDateFormat
import java.util.Date
import java.util.NoSuchElementException



class Proximities(jsons: Iterator[String]){
  // データが大きいので効率を考えmutable実装とする
  private val container = collection.mutable.Map[Int,List[(Int, Date, Array[Int])]]()
  
  JSON.globalNumberParser = {input:String => input.toInt} // override behavior of parser for Numeric.
  for(json <- jsons) { // ToDo:型変換大量に必要なのどうにかしたい
    val Some(parsedJson) = JSON.parseFull(json).asInstanceOf[Option[Map[String,Any]]]
    val seq = parsedJson("Sequence").asInstanceOf[Int]
    val date = new SimpleDateFormat("\"yyy/MM/dd HH:mm:ss\"\n").parse(parsedJson("Time").asInstanceOf[String])
    val res = parsedJson("Result").asInstanceOf[Map[String,Any]]
    val userId = res("Sample").asInstanceOf[Int]
    val topk = res("Topk").asInstanceOf[List[Int]].toArray
    
    val tupple = (seq,date,topk)
    val newTupple = (container.get(userId) match {
      case Some(s) => tupple :: s
      case None => tupple :: Nil
    })
    container += userId -> newTupple
  }
  container.transform((k,v) => v.reverse)
  

  def getUsers:Set[Int] = container.keySet.toSet

  def find(userId: Int):(Int,List[(Int, Date, Array[Int])]) = (userId,container(userId))
  
  def find():(Int,List[(Int, Date, Array[Int])]) = next // fix?

  private var userIterator = getUsers.iterator
  def next():(Int,List[(Int, Date, Array[Int])]) = {
    if (!userIterator.hasNext) userIterator = getUsers.iterator
    val userId = userIterator.next
    (userId,container(userId))
  }
  

}
