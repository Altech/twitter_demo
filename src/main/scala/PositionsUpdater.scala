class PositionsUpdater(var ids:Array[Int]) {
  val mainId = ids(0)
  def otherIds = ids.filter(_ != mainId)
  val nodes = collection.mutable.Map[Int,Node]()
  val dt = 0.1d
  initNodes()
  initPositions()

  def initNodes() {
    for((id,i) <- ids zipWithIndex)
      nodes += id -> Node(i)
    for(otherId <- otherIds){
      nodes(mainId) << nodes(otherId)
      nodes(otherId) << nodes(mainId)
    }
  }

  def initPositions() {
    for (i <- 1 to 5000) {
      for((id,node) <- nodes){
	var f = Vector((0,0,0))
	for(neighbor <- node.neighbors)
	  f = f + node.getSpringForce(neighbor)
	for((otherId,otherNode) <- nodes if id != otherId)
	  f = f + node.getReplusiveForce(otherNode)
	f = f + node.getFrictionalForce
	node.moveEular(dt,f)
      }
    }
  }

  def update(newIds:Array[Int]) {
    val deletedIds = ids.toSet -- newIds.toSet
    val insertedIds = newIds.toSet -- ids.toSet
    ids = newIds

    // new
    for((id,i) <- ids zipWithIndex){
      nodes.get(id) match {
    	case Some(node) => node.setRank(i)
    	case None => nodes += id -> Node(i)
      }
    }
    
    // delete
    val deletedIdIter = deletedIds.iterator
    val basePosition = nodes(mainId).r
    for(insertedId <- insertedIds; deletedId = deletedIdIter.next){
      val oldPosition = nodes(deletedId).r
      val getNewPosition = ((old:Double,base:Double)=> (old-base)*5+base)
	val newPosition = Vector((getNewPosition(oldPosition.x,basePosition.x),getNewPosition(oldPosition.y,basePosition.y),getNewPosition(oldPosition.z,basePosition.z)))
      nodes(insertedId).r = newPosition
      nodes.remove(deletedId)
    }
    
    // set neighbors
    for((id,i) <- ids zipWithIndex)
      nodes(id).neighbors = Nil
    for(otherId <- otherIds){
      nodes(mainId) << nodes(otherId)
      nodes(otherId) << nodes(mainId)
    }
  
  }

  def next {
    for (i <- 1 to 5) {
      for((id,node) <- nodes if id != mainId){ // 中心ノードは固定
	var f = Vector((0,0,0))
	for(neighbor <- node.neighbors)
	  f = f + node.getSpringForce(neighbor)
	for((otherId,otherNode) <- nodes if id != otherId) 
	  f = f + node.getReplusiveForce(otherNode)
	f = f + node.getFrictionalForce
	node.moveEular(dt,f)
      }
    }
  }
  
  def get():Map[Int,(Int,Int,Int)] = {
    val basePosition = nodes(mainId).r
    val returnValue = nodes.toMap.mapValues(_.r).mapValues(p => (p-basePosition)).mapValues(_.p).mapValues(p => (p._1.toInt,p._2.toInt,p._3.toInt))
    returnValue    
  }

  def dumpPosition {
    println("mainId:" + mainId)
    println("otherId:" + otherIds)
    for((id,node) <- nodes){
      println("id:"+id)
      println(" position:" + node.r.toString)
      println(" neighbor:" + node.neighbors.toString)
    }
  }

  case class Vector(val p:(Double,Double,Double)) {
    val x = p._1
    val y = p._2
    val z = p._3
    def +(that:Vector):Vector = Vector((this.x+that.x,this.y+that.y,this.z+that.z))
    def -(that:Vector):Vector = Vector((this.x-that.x,this.y-that.y,this.z-that.z))
    def *(t:Double):Vector = Vector((this.x*t,this.y*t,this.z*t))
    def /(t:Double):Vector = Vector((this.x/t,this.y/t,this.z/t))
    def norm = x*x+y*y+z*z
  }
  
  case class Node(var i:Int) {
    var neighbors:List[Node] = Nil
    def << (neighbor:Node) { neighbors = neighbor :: neighbors }
    var r = Vector(0,0,0)
    var v = Vector(0,0,0)
    var m = 1.0
    val k = 0.1d //バネ定数
    val g = 5000.0d //反発力定数
    var l = 50.0d * Math.log(Math.E+i) //バネの自然長
    val a = 1.0d //空気抵抗の比例定数
    def moveEular(dt:Double, a:Vector) {
      v = v + a * dt / m
      r = r + v * dt / m
    }
    def getSpringForce(n:Node):Vector = {
      val dr = r - n.r
      val d3 = dr.norm
      if (d3 < scala.Double.Epsilon){
	val r = new scala.util.Random
	return Vector((r.nextDouble-0.5d,r.nextDouble-0.5d,r.nextDouble-0.5d))
      }
      val d = Math.sqrt(d3)
      val dl = d-l
      return Vector((-k*dl*(dr.x/d), -k*dl*(dr.y/d), -k*dl*(dr.z/d)))
    }
    def getReplusiveForce(n:Node):Vector = {
      val dr = r - n.r
      val d3 = dr.norm
      if (d3 < scala.Double.Epsilon){
	val r = new scala.util.Random
	return Vector((r.nextDouble-0.5d,r.nextDouble-0.5d,r.nextDouble-0.5d))
      }
      val d = Math.sqrt(d3)
      return Vector((g/d3*(dr.x/d), g/d3*(dr.y/d), g/d3*(dr.z/d)))
    }
    def getFrictionalForce():Vector = {
      return Vector((-a*v.x, -a*v.y, -a*v.z))
    }
    def setRank(i:Int) {
      this.i = i
      l = 50.0d * Math.log(Math.E+i)
    }
  }
  
}
