// import java.awt.
import java.applet.Applet
import java.util.Date
import processing.core._
import processing.core.PConstants._
import ddf.minim._;


// scala -classpath /Users/Altech/dev/processing/Processing/core.jar:/Users/Altech/dev/graphdemo/scratch/container.scala applet.scala
// scala -classpath ../../processing/Processing/core.jar applet.scala 


object ApplicationMain  {

  class Embedded(var sequences: List[(Int, Date, Array[Int])]) extends PApplet with MyPExtention  {
    var (seq,time,Array(user_id,_*)) = sequences.head
    // val (user_id = sequences.head._3(0)
    // var (time: Date = sequences.head._2
    var r = 0
    val minim = new Minim(this)
    val player = minim.loadFile("sample.mp3")
    var is_generating = false
    val logo = loadImage("twianalysis.png")

    override def setup {
      super.setup
  
      size(1024,512,P3D)
      // noLoop
      frameRate(30)
      // ambientLight(20, 20, 20)    //環境光を当てる
      directionalLight(51, 102, 126, -1, 0, 0)
      val myFont = createFont("Helvetica",32)
      textFont(myFont)

      
      player.play()
      is_generating = true
      pushMatrix()
    }

    def draw_init {
      background(0)
      image(logo,0,-30)
      fill(255)
    }
    
    override def draw {
      draw_init
  
      bottom {
	text("Target User : " + user_id, 20, -80)
	text("Top10 :" + sequences.head._3.mkString(","), 20, -30)
	sequences = sequences.tail
      }

      directionalLight(126, 126, 126, 0, 0, -1)
      ambientLight(102, 102, 102)

      // // popMatrix()

      // translate(0,0)
      // rotateY(-r)
      
      // text("Target User : " + user_id, 20, 20)
      // text("Top10 :" + sequences.head._3.mkString(","), 20, 50)
      // sequences = sequences.tail
      // rect(200,200,100,100)

      // rotateY(r)
      // // pushMatrix()
      
      // // popMatrix()
  
      // background(mouseX)
      translate(width/2, height/2)

      // rotation
      r += 1
      rotateY(radians(r))
      if (r == 360)
      	r == 0
      fill(204, 102, 0)
      sphere(30)

      fill(213, 29, 221)
      translate(50, 10, 50)
      sphere(20)
      translate(-50, -10, -50)
      
      fill(9, 102, 222)
      translate(50, -10, -70)
      sphere(20)
      translate(-50, 10, 70)
      
      smooth_draw {
	stroke_draw(255) {
	  line(0, 0, 0, 50, 10, 50)
	  line(0, 0, 0, 50, -10, -70)
	}
      }

    }

    override def mousePressed(){
      if(is_generating) {
	noLoop
	player.pause
	is_generating = false
      }
      else {
	loop
	player.play
	is_generating = true
      }
    }

    // private
  
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
    
    // PApplet.runSketch(Array("title"), Applet)
    
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
    def bottom(f: => Unit) = {
      translate(0,height)
      f
      translate(0,-height)
    }
    
  }
  
}


  
// import scala.swing._
// import java.awt.Color

// class MainApplet extends Applet {
//   class MainUI(backgroundColor: Color) extends UI {
//     val mainPanel = new BoxPanel(Orientation.Vertical) {
//       contents += new Button("HI")
//     }
//     mainPanel.background = backgroundColor // no need for ugly _=
//     contents = mainPanel

//     def init(): Unit = {
//       println("Hello")
//     }
//   }
//   val ui = new MainUI(Color.WHITE)
//       println("Hello")
//   ui.init()
//   ui.start()
// }

