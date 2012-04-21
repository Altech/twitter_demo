class PositionsUpdater(var ids:Array[Int]) {
  val main_id = ids(0)
  def other_ids = ids.filter(_ != main_id)
  val nodes = collection.mutable.Map[Int,Node]()
  val dt = 0.1d
  init_nodes()
  init_positions()

  def init_nodes() {
    for((id,i) <- ids zipWithIndex)
      nodes += id -> Node(i)
    for(other_id <- other_ids){
      nodes(main_id) << nodes(other_id)
      nodes(other_id) << nodes(main_id)
    }
    nodes(main_id).m = 10000 // 中心ノードは質量大（あまり動かない）
  }

  def init_positions() {
    for (i <- 1 to 5000) {
      for((id,node) <- nodes){
	var f = Vector((0,0,0))
	for(neighbor <- node.neighbors)
	  f = f + node.get_spring_force(neighbor)
	for((other_id,other_node) <- nodes if id != other_id)
	  f = f + node.get_replusive_force(other_node)
	f = f + node.get_frictional_force
	node.move_eular(dt,f)
      }
    }
  }

  def update(new_ids:Array[Int]) {
    val deleted_ids = ids.toSet -- new_ids.toSet
    val inserted_ids = new_ids.toSet -- ids.toSet
    ids = new_ids
    println("deleted_ids" + deleted_ids.toString)
    println("inserted_ids" + inserted_ids.toString)
    

    // new
    for((id,i) <- ids zipWithIndex){
      nodes.get(id) match {
    	case Some(node) => node.set_rank(i)
    	case None => nodes += id -> Node(i)
      }
    }

    // // debug
    // if(!deleted_ids.isEmpty){
    //   println("\nbefore:")
    //   dump_position
    // }
      
    
    // delete
    val deleted_id_iter = deleted_ids.iterator
    val base_position = nodes(main_id).r
    for(inserted_id <- inserted_ids; deleted_id = deleted_id_iter.next){
      val old_position = nodes(deleted_id).r
      val get_new_position = ((old:Double,base:Double)=> (old-base)*10+base)
	val new_position = Vector((get_new_position(old_position.x,base_position.x),get_new_position(old_position.y,base_position.y),get_new_position(old_position.z,base_position.z)))
      // println("old_position:" + old_position.toString)
      // println("base_position:" + base_position.toString)
      // println("new_position:" + new_position.toString)
      nodes(inserted_id).r = new_position
      nodes.remove(deleted_id)
    }
    
    // set neighbors
    for((id,i) <- ids zipWithIndex)
      nodes(id).neighbors = Nil
    for(other_id <- other_ids){
      nodes(main_id) << nodes(other_id)
      nodes(other_id) << nodes(main_id)
    }

    // if(!deleted_ids.isEmpty){
    //   println("\nafter:")
    //   dump_position
    // }
  
  }

  def next {
    for (i <- 1 to 2) {
      for((id,node) <- nodes){
	var f = Vector((0,0,0))
	for(neighbor <- node.neighbors)
	  f = f + node.get_spring_force(neighbor)
	for((other_id,other_node) <- nodes if id != other_id)
	  f = f + node.get_replusive_force(other_node)
	f = f + node.get_frictional_force
	node.move_eular(dt,f)
      }
    }
  }
  
  def get():Map[Int,(Int,Int,Int)] = {
    val base_position = nodes(main_id).r
    val return_value = nodes.toMap.mapValues(_.r).mapValues(p => (p-base_position)).mapValues(_.p).mapValues(p => (p._1.toInt,p._2.toInt,p._3.toInt))
    // for((k,v) <- return_value)
    //   println(k.toString + ':' + v.toString)
    return_value    
  }

  def dump_position {
    println("main_id:" + main_id)
    println("other_id:" + other_ids)
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
    def move_eular(dt:Double, a:Vector) {
      v = v + a * dt / m
      r = r + v * dt / m
    }
    def get_spring_force(n:Node):Vector = {
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
    def get_replusive_force(n:Node):Vector = {
      val dr = r - n.r
      val d3 = dr.norm
      if (d3 < scala.Double.Epsilon){
	val r = new scala.util.Random
	return Vector((r.nextDouble-0.5d,r.nextDouble-0.5d,r.nextDouble-0.5d))
      }
      val d = Math.sqrt(d3)
      return Vector((g/d3*(dr.x/d), g/d3*(dr.y/d), g/d3*(dr.z/d)))
    }
    def get_frictional_force():Vector = {
      return Vector((-a*v.x, -a*v.y, -a*v.z))
    }
    def set_rank(i:Int) {
      this.i = i
      l = 50.0d * Math.log(Math.E+i)
    }
  }
  
}
