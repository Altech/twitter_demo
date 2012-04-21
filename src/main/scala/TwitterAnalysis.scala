import java.applet.Applet
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import processing.core._
import processing.core.PConstants._

class TwitterAnalysis {

  class Embedded extends PApplet with MyPExtention with MyDrawingTools {
    // data
    val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/twitter_demo/dataset/single_topn2.json").getLines)
    var sequences: List[(Int, Date, Array[Int])] = proximities.find()
    var (seq_no,time,topk) = sequences.head; sequences = sequences.tail;
    var target_id = topk(0)
    var positions = new PositionsUpdater(topk)
    var visible_related_users = 10
    
    // animation status
    var r = 0
    var frame_count = 0
    
    override def draw {
      draw_init

      set_ambient_right

      top {
	text(time,10,40)
	text("Frame Count: " + frame_count,10,70)
      }

      // translate(10,90) {
	text("Switch target user",10,100)
	text("Top " + visible_related_users + " of related users.",10,130)
      // }
      
      bottom {
	text("Target User : " + target_id, 20, -80)
	text("Top10 :" + topk.mkString(","), 20, -30)
      }

      center {
	rotate_next
	draw_positions(target_id,positions.get)
      }

      update_time
      if (!(time.before(sequences.head._2))){
	// println("update_bef:" + topk.mkString(","))
      	seq_no = sequences.head._1
      	time  = sequences.head._2
      	topk  = sequences.head._3
      	sequences = sequences.tail
      	positions.update(topk) // ToDo
	// println("update_aft:" + topk.mkString(","))
      }
      positions.next

      frame_count += 1
    }


    // setting
    override def setup {
      super.setup
      frameRate(30)
      size(1024,512,P3D)
      // noLoop
      val myFont = createFont("Helvetica",32)
      textFont(myFont)
    }
    def set_ambient_right {
      directionalLight(126, 126, 126, 0, 0, -1)
      ambientLight(102, 102, 102)
    }

    // event
    override def mousePressed {
      
    }
      
    // routines
    def rotate_next {
      r += 1
      rotateY(radians(r))
      if (r == 360)
      	r == 0
    }
    def update_time {
      val interval = 1
      val cal = Calendar.getInstance(); cal.setTime(time); cal.add(Calendar.MINUTE, interval)
      time = cal.getTime()
    }

  
  }

  trait MyDrawingTools extends PApplet with MyPExtention {
    def draw_positions(target_id:Int, positions:Map[Int,(Int,Int,Int)]) {
      for((id,position) <- positions) {
	translate(position) {
	  if (id == target_id)
	    fill(204, 102, 0)
	  else
	    fill(255)
	  sphere(30)
	  // println("distance[" + id.toString + "]:" + Math.sqrt(position._1*position._1 + position._2*position._2 + position._3*position._3).toString)
	}
	// line(0,0,0,position._1,position._2,position._3)
	stroke_and_smooth_draw(255) {
	  line_to_position(position)
	}
      }
    }
  }
  
  trait MyPExtention extends PApplet {

    override def setup {
      noSmooth
      noStroke
    }

    def draw_init {
      background(0)
      fill(255)
    }
    
    def radians(i:Int):Float = (i * 3.141592f)/180.0f
    def smooth_draw(f: => Unit) = {smooth(); f; noSmooth()}
    def stroke_draw(color:Int)(f: => Unit) = {stroke(color); f; noStroke()}
    def stroke_and_smooth_draw(color:Int)(f: => Unit) = {stroke(color); smooth(); f; noStroke(); noSmooth()}
    def top(f: => Unit) = { translate(0,0); f; translate(0,0) }
    def bottom(f: => Unit) = { translate(0,height); f; translate(0,-height) }
    def center(f: => Unit) = { translate(width/2, height/2); f; translate(-width/2, -height/2); }
    // def translate(dx:Int, dy:Int)(f: => Unit) = { super.translate(dx,dy); f; super.translate(-dx,-dy);}
    def translate(dx:Int, dy:Int, dz:Int)(f: => Unit) = { super.translate(dx,dy,dz); f; super.translate(-dx,-dy,-dz);}
    def translate(dr:(Int,Int,Int))(f: => Unit) = { super.translate(dr._1,dr._2,dr._3); f; super.translate(-dr._1,-dr._2,-dr._3) }
    def line_to_position(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
    def text(time:Date,x:Int,y:Int) = { super.text(new SimpleDateFormat("yyyy'年'MM'月'dd'日' kk:mm").format(time),x,y) }
    
  }
  
  
  def main(args: Array[String]) {
    println("Hello, world!")
    
    val root_frame = new javax.swing.JFrame("Test")
    val root_applet = new Applet // ブラウザ上ではアプレットがルートになる
    root_frame.getContentPane().add(root_applet)

    // val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/graphdemo/dataset/single_topn.json").getLines)
    // val sample_data = proximities.find()
    
    val embedded_applet = new Embedded//(sample_data)
    root_applet.add(embedded_applet)
    embedded_applet.init
    
    root_frame.pack
    root_frame.setVisible(true)
  }

}
