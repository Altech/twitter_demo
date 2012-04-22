import java.applet.Applet
import java.util.Date
import processing.core._
import processing.core.PConstants._

object ApplicationMain  {

  
  class Embedded(var sequences: List[(Int, Date, Array[Int])]) extends PApplet with MyPExtention with PositionCalclator  {
    var (seq,time,Array(user_id,_*)) = sequences.head
    // val (user_id = sequences.head._3(0)
    // var (time: Date = sequences.head._2
    var r = 0

    override def setup {
      super.setup
  
      // frameRate(120)
      size(1024,512,P3D)
      // noLoop
      val myFont = createFont("Helvetica",32)
      textFont(myFont)
    }

    def draw_init {
      background(0)
      fill(255)
    }

    var first_positions:Map[Int,(Int,Int,Int)] = Map[Int,(Int,Int,Int)]()
    
    override def draw {
      draw_init

      val sequence = sequences.head
      // sequences = sequences.tail
  
      bottom {
	text("Target User : " + user_id, 20, -80)
	text("Top10 :" + sequence._3.mkString(","), 20, -30)
      }
      
      directionalLight(126, 126, 126, 0, 0, -1)
      ambientLight(102, 102, 102)

      // centering
      translate(width/2, height/2)
      
      // rotation
      r += 1
      rotateY(radians(r))
      if (r == 360)
      	r == 0
      
      val positions = if(first_positions.isEmpty) {
			first_positions = get_positions(sequence._3)
			first_positions }
		      else
			first_positions

      var a = 40
      for((id,position) <- positions) {
	translate_tmp(position) {
	  if (id == user_id)
	    fill(204, 102, 0)
	  else
	    fill(255)
	  sphere(30)
	  a += 20
	  // println("distance[" + id.toString + "]:" + Math.sqrt(position._1*position._1 + position._2*position._2 + position._3*position._3).toString)
	}
	// line(0,0,0,position._1,position._2,position._3)
	stroke_and_smooth_draw(255) {
	  line_to_position(position)
	}
      }
      
    }
    // private
  
    override def mousePressed(){
      // rotation
      r += 1
      rotateY(radians(r))
      if (r == 360)
      	r == 0
    }
    
    def radians(i:Int):Float = (i * 3.141592f)/180.0f
    
  }
  
  def main(args: Array[String]) {
    println("Hello, world!")
    
    val root_frame = new javax.swing.JFrame("Test")
    val root_applet = new Applet // ブラウザ上ではアプレットがルートになる
    root_frame.getContentPane().add(root_applet)

    val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/graphdemo/dataset/single_topn.json").getLines)
    val sample_data = proximities.find()
    
    
    val embedded_applet = new Embedded(sample_data)
    root_applet.add(embedded_applet)
    embedded_applet.init
    
    root_frame.pack
    root_frame.setVisible(true)
  }

  trait MyPExtention extends PApplet {

    override def setup {
      noSmooth
      noStroke
    }
    
    def smooth_draw(f: => Unit) = {smooth(); f; noSmooth()}
    def stroke_draw(color:Int)(f: => Unit) = {stroke(color); f; noStroke()}
    def stroke_and_smooth_draw(color:Int)(f: => Unit) = {stroke(color); smooth(); f; noStroke(); noSmooth()}
    def bottom(f: => Unit) = {
      translate(0,height)
      f
      translate(0,-height)
    }
    def translate_tmp(dx:Int, dy:Int, dz:Int)(f: => Unit) = {
      translate(dx,dy,dz)
      f
      translate(-dx,-dy,-dz)
    }
    def translate_tmp(dr:(Int,Int,Int))(f: => Unit) = {
      translate(dr._1,dr._2,dr._3)
      f
      translate(-dr._1,-dr._2,-dr._3)
    }
    def line_to_position(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
    
  }

  
  trait PositionCalclator {

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
    
    case class Node(val i:Double) {
      var neighbors:List[Node] = Nil
      def << (neighbor:Node) { neighbors = neighbor :: neighbors }
      var r = Vector(0,0,0)
      var v = Vector(0,0,0)
      val k = 0.1d //バネ定数
      val l = 50.0d * Math.log(Math.E+i) //バネの自然長
      val g = 5000.0d //反発力定数
      val a = 0.3d //空気抵抗の比例定数
      def move_eular(dt:Double, a:Vector) {
	v = v + a * dt
	r = r + v * dt
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
	// println(d3)
	return Vector((g/d3*(dr.x/d), g/d3*(dr.y/d), g/d3*(dr.z/d)))
      }
      def get_frictional_force():Vector = {
	return Vector((-a*v.x, -a*v.y, -a*v.z))
      }
      
    }

    def get_positions(ids:Array[Int]):Map[Int,(Int,Int,Int)] = {
      val main_id = ids(0)
      val other_ids = ids.filter(_ != main_id)
      
      val nodes = collection.mutable.Map[Int,Node]()
      for((id,i) <- ids zipWithIndex){
	nodes += id -> Node(i)
      }
      for(other_id <- other_ids){
	nodes(main_id) << nodes(other_id)
	nodes(other_id) << nodes(main_id)
      }

      val dt = 0.1d
      for (i <- 1 to 20000) {
	// println(i)
	for((id,node) <- nodes){
	  var f = Vector((0,0,0))
	  for(neighbor <- node.neighbors)
	    f = f + node.get_spring_force(neighbor)
	  for((other_id,other_node) <- nodes if id != other_id)
	    f = f + node.get_replusive_force(other_node)
	  f = f + node.get_frictional_force
	  node.move_eular(dt,f)
	  // println(id.toString + ':' + node.r.toString)
	}
      }

      val base_position = nodes(main_id).r
      val return_value = nodes.toMap.mapValues(_.r).mapValues(p => (p-base_position)).mapValues(_.p).mapValues(p => (p._1.toInt,p._2.toInt,p._3.toInt))
      for((k,v) <- return_value)
	println(k.toString + ':' + v.toString)
      return_value
    }
    
  }

  
}
