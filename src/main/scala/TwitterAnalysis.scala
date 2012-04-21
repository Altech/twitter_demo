import java.applet.Applet
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import processing.core._
import processing.core.PConstants._

object TwitterAnalysis {

  class Embedded extends PApplet with MyPExtention with MyDrawingTools {
    // data
    val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/twitter_demo/src/main/resources/converted_topn.json").getLines)
    var (target_id,sequences): (Int,List[(Int, Date, Array[Int])]) = proximities.find()
    var (seq_no,time,topk) = sequences.head; sequences = sequences.tail;
    var positions = new PositionsUpdater(topk)
    var visible_related_users = 10
    
    // animation status
    var isPlaying = true
    var r = 0
    var frame_count = 0

    // view 
    val mini_width = 500
    val mini_height = 500
    val default_font = createFont("Helvetica",32)
    val time_font = createFont("Krungthep",32)
    val time_format = new java.text.SimpleDateFormat("yyy/MM/dd HH:mm")
    val bg = loadImage("bg3.png")
    val button_play = loadImage("button_play.png")
    val button_stop = loadImage("button_stop.png")
    val button_end = loadImage("button_end.png")
    // objects
    var button_play_coordinates = (0,0)
    var button_stop_coordinates = (0,0)
    var button_end_coordinates = (0,0)
    
    override def draw {
      draw_init

      translate((width-mini_width)/2,50){
	top {
	  text("Frame Count: " + frame_count,10,30)
	}

	translate(10,mini_height-60){
	  textFont(time_font)
	  fill(73,142,255)
	  text(time_format.format(time),10,40)
	  textFont(default_font) // fix later
	}
	translate(0,0,-500){
	
	  set_ambient_right
	
	  top {
	    text("Target User : " + target_id, 0, -80)
	    text("Top10 :" + topk.mkString(","), 0, -30)
	  }

	  translate(mini_width/2,mini_height/2) {
	    rotate_next {
	      draw_positions(target_id,positions.get)
	    }
	  }
	}
		
      }

      noLights
      imageMode(CORNER)
      image(bg,0,0)

      translate(width/2,height-50){
	imageMode(CENTER)
	image(button_end,-60,0)
	image(button_play,0,0)
	image(button_stop,60,0)
	button_end_coordinates = (screenX(-60,0,0).toInt,screenY(-60,0,0).toInt)
	button_play_coordinates = (screenX(0,0,0).toInt,screenY(0,0,0).toInt)
	button_stop_coordinates = (screenX(60,0,0).toInt,screenY(60,0,0).toInt)
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
      frameRate(15)
      size(1000,650,P3D)
      // noLoop
      textFont(default_font)
    }
    def set_ambient_right {
      directionalLight(126, 126, 126, 0, 0, -1)
      ambientLight(102, 102, 102)
    }

    // event
    override def mousePressed {
      if(is_in_square((mouseX,mouseY),button_end_coordinates,25)){
	switchUser
	noLoop
	isPlaying = false
      }
      else if(is_in_square((mouseX,mouseY),button_play_coordinates,25)){
	if(!isPlaying)
	  loop
	isPlaying = true
      }
      else if(is_in_square((mouseX,mouseY),button_stop_coordinates,25)){
	if(isPlaying)
	  noLoop
	isPlaying = false
      }
    }

    def switchUser(userID:Int) {
      val proximity = proximities.find()
      target_id = proximity._1
      sequences = proximity._2
      seq_no = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3
      sequences = sequences.tail
      positions = new PositionsUpdater(topk)
    }

    def switchUser() {
      val proximity = proximities.find()
      target_id = proximity._1
      sequences = proximity._2
      seq_no = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3
      sequences = sequences.tail
      target_id = topk(0)
      positions = new PositionsUpdater(topk)
    }
    
    // routines
    def rotate_next(f: => Unit) {
      r += 1
      rotateY(radians(r))
      f
      rotateY(radians(-r))
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
    def top(f: => Unit) = { super.translate(0,0); f; super.translate(0,0) }
    def bottom(f: => Unit) = { super.translate(0,height); f; super.translate(0,-height) }
    def center(f: => Unit) = { super.translate(width/2, height/2); f; super.translate(-width/2, -height/2); }
    def center_x(f: => Unit) = { super.translate(width/2, 0); f; super.translate(-width/2, 0); }
    def center_y(f: => Unit) = { super.translate(0, height/2); f; super.translate(0, -height/2); }
    def translate(dx:Int, dy:Int)(f: => Unit) = { super.translate(dx,dy,0); f; super.translate(-dx,-dy,0);}
    def translate(dx:Int, dy:Int, dz:Int)(f: => Unit) = { super.translate(dx,dy,dz); f; super.translate(-dx,-dy,-dz);}
    def translate(dr:(Int,Int,Int))(f: => Unit) = { super.translate(dr._1,dr._2,dr._3); f; super.translate(-dr._1,-dr._2,-dr._3) }
    def line_to_position(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
    def text(time:Date,x:Int,y:Int) = { super.text(new SimpleDateFormat("yyyy'年'MM'月'dd'日' kk:mm").format(time),x,y) }
    def is_in_square(point:(Int,Int), center:(Int,Int), r:Int) = {
      center._1-r <= point._1 && point._1 <= center._1+r  &&  center._2-r <= point._2 && point._2 <= center._2+r
    }
    
  }
  
  
  def main(args: Array[String]) {
    println("Application will be open")
    
    val root_frame = new javax.swing.JFrame("Test")
    val root_applet = new Applet // ブラウザ上ではアプレットがルートになる
    root_frame.getContentPane().add(root_applet)
    
    val embedded_applet = new Embedded//(sample_data)
    root_applet.add(embedded_applet)
    embedded_applet.init
    
    root_frame.pack
    root_frame.setVisible(true)
  }

}
