import java.applet.Applet
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import processing.core._
import processing.core.PConstants._

object TwitterAnalysis {

  class Embedded extends PApplet with MyPExtention with MyDrawingTools {
    // data
    val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/twitter_demo/src/main/resources/single_topn.json").getLines)
    var (targetId,sequences): (Int,List[(Int, Date, Array[Int])]) = proximities.find()
    var (seqNo,time,topk) = sequences.head; sequences = sequences.tail;
    var positions = new PositionsUpdater(topk)
    var visibleRelatedUsers = 10
    
    // animation status
    var isPlaying = true
    var r = 0

    // view 
    val miniWidth = 500
    val miniHeight = 500
    val defaultFont = createFont("Helvetica",32)
    val defaultFontSmall = createFont("Helvetica",18)
    val timeFont = createFont("Krungthep",32)
    val timeFormat = new java.text.SimpleDateFormat("yyy/MM/dd HH:mm")
    val bg = loadImage("bg3.png")
    val buttonPlay = loadImage("button_play.png")
    val buttonStop = loadImage("button_stop.png")
    val buttonEnd = loadImage("button_end.png")
    
    // objects
    var buttonPlayCoordinates = (0,0)
    var buttonStopCoordinates = (0,0)
    var buttonEndCoordinates = (0,0)
    
    override def draw {
      drawInit

      translate((width-miniWidth)/2,50){
	top {
	  textFont(defaultFontSmall)
	  fill(73,142,255)
	  text("Target User : " + targetId, 10, 30)
	  text("Top10 :" + topk.mkString(","), 10, 60)
	  // text("Frame Count: " + frameCount,10,30)
	}

	translate(10,miniHeight-60){
	  textFont(timeFont)
	  fill(73,142,255)
	  text(timeFormat.format(time),10,40)
	  textFont(defaultFont) // fix later
	}
	
	translate(0,0,0){
	  setAmbientRight
	
	  translate(miniWidth/2,miniHeight/2) {
	    rotateNext {
	      drawPositions(targetId,positions.get)
	    }
	  }
	}
		
      }

      noLights
      imageMode(CORNER)
      image(bg,0,0)

      translate(width/2,height-50){
	imageMode(CENTER)
	image(buttonEnd,-60,0)
	image(buttonPlay,0,0)
	image(buttonStop,60,0)
	buttonEndCoordinates = (screenX(-60,0,0).toInt,screenY(-60,0,0).toInt)
	buttonPlayCoordinates = (screenX(0,0,0).toInt,screenY(0,0,0).toInt)
	buttonStopCoordinates = (screenX(60,0,0).toInt,screenY(60,0,0).toInt)
      }
      
      updateTime
      if (!(time.before(sequences.head._2))){
	// println("update_bef:" + topk.mkString(","))
      	seqNo = sequences.head._1
      	time  = sequences.head._2
      	topk  = sequences.head._3
      	sequences = sequences.tail
      	positions.update(topk) // ToDo
	// println("update_aft:" + topk.mkString(","))
      }
      positions.next

      frameCount += 1
    }


    // setting
    override def setup {
      super.setup
      // frameRate(15)
      size(1000,650,P3D)
      // noLoop
      textFont(defaultFont)
    }
    def setAmbientRight {
      directionalLight(255, 255, 255, 0, 0, -1)
      // directionalLight(126, 126, 126, 0, 0, -1)
      ambientLight(102, 102, 102)
      // ambientLight(256, 256, 256)
    }

    // event
    override def mousePressed {
      if(isInSquare((mouseX,mouseY),buttonEndCoordinates,25)){
	switchUser
	noLoop
	isPlaying = false
      }
      else if(isInSquare((mouseX,mouseY),buttonPlayCoordinates,25)){
	if(!isPlaying)
	  loop
	isPlaying = true
      }
      else if(isInSquare((mouseX,mouseY),buttonStopCoordinates,25)){
	if(isPlaying)
	  noLoop
	isPlaying = false
      }
    }

    def switchUser(userID:Int) {
      val proximity = proximities.find()
      targetId = proximity._1
      sequences = proximity._2
      seqNo = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3
      sequences = sequences.tail
      positions = new PositionsUpdater(topk)
    }

    def switchUser() {
      val proximity = proximities.find()
      targetId = proximity._1
      sequences = proximity._2
      seqNo = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3
      sequences = sequences.tail
      positions = new PositionsUpdater(topk)
    }
    
    // routines
    def rotateNext(f: => Unit) {
      r += 1
      rotateY(radians(r))
      f
      rotateY(radians(-r))
      if (r == 360)
      	r == 0
    }
    def updateTime {
      val interval = 1
      val cal = Calendar.getInstance(); cal.setTime(time); cal.add(Calendar.MINUTE, interval)
      time = cal.getTime()
    }

  
  }

  trait MyDrawingTools extends PApplet with MyPExtention {
    def drawPositions(targetId:Int, positions:Map[Int,(Int,Int,Int)]) {
      for((id,position) <- positions) {
	translate(position) {
	  if (id == targetId){
	    // fill(0,255,255) // bright blue
	    fill(0,200,200)
	    sphere(30)
	  }
	  else {
	    // fill(0,38,188) // deep blue
	    // fill(231,223,0) // bright yellow
	    fill(235,96,160)
	    sphere(30)
	  }
	  // println("distance[" + id.toString + "]:" + Math.sqrt(position._1*position._1 + position._2*position._2 + position._3*position._3).toString)
	}
	strokeAndSmoothDraw(255) {
	  lineToPosition(position)
	}
      }
    }
  }
  
  trait MyPExtention extends PApplet {

    override def setup {
      noSmooth
      noStroke
    }

    def drawInit {
      background(0)
      fill(255)
    }
    
    def radians(i:Int):Float = (i * 3.141592f)/180.0f
    def smoothDraw(f: => Unit) = {smooth(); f; noSmooth()}
    def strokeDraw(color:Int)(f: => Unit) = {stroke(color); f; noStroke()}
    def strokeAndSmoothDraw(color:Int)(f: => Unit) = {stroke(color); smooth(); f; noStroke(); noSmooth()}
    def top(f: => Unit) = { super.translate(0,0); f; super.translate(0,0) }
    def bottom(f: => Unit) = { super.translate(0,height); f; super.translate(0,-height) }
    def center(f: => Unit) = { super.translate(width/2, height/2); f; super.translate(-width/2, -height/2); }
    def centerX(f: => Unit) = { super.translate(width/2, 0); f; super.translate(-width/2, 0); }
    def centerY(f: => Unit) = { super.translate(0, height/2); f; super.translate(0, -height/2); }
    def translate(dx:Int, dy:Int)(f: => Unit) = { super.translate(dx,dy,0); f; super.translate(-dx,-dy,0);}
    def translate(dx:Int, dy:Int, dz:Int)(f: => Unit) = { super.translate(dx,dy,dz); f; super.translate(-dx,-dy,-dz);}
    def translate(dr:(Int,Int,Int))(f: => Unit) = { super.translate(dr._1,dr._2,dr._3); f; super.translate(-dr._1,-dr._2,-dr._3) }
    def lineToPosition(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
    def text(time:Date,x:Int,y:Int) = { super.text(new SimpleDateFormat("yyyy'年'MM'月'dd'日' kk:mm").format(time),x,y) }
    def isInSquare(point:(Int,Int), center:(Int,Int), r:Int) = {
      center._1-r <= point._1 && point._1 <= center._1+r  &&  center._2-r <= point._2 && point._2 <= center._2+r
    }
    
  }
  
  
  def main(args: Array[String]) {
    println("Application will be open")
    
    val rootFrame = new javax.swing.JFrame("Twitter Analysis")
    val rootApplet = new Applet // ブラウザ上ではアプレットがルートになる
    rootFrame.getContentPane().add(rootApplet)
    
    val embeddedApplet = new Embedded//(sample_data)
    rootApplet.add(embeddedApplet)
    embeddedApplet.init
    
    rootFrame.pack
    rootFrame.setVisible(true)
  }

}
