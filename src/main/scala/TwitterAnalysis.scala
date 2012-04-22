import java.applet.Applet
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import processing.core._
import processing.core.PConstants._

object TwitterAnalysis {

  class Embedded extends PApplet with MyPExtention with MyDrawingTools {
    // data
    val proximities = new Proximities(scala.io.Source.fromFile("/Users/Altech/dev/twitter_demo/src/main/resources/single_topn_50.json").getLines)
    val nameList = scala.io.Source.fromFile("/Users/Altech/dev/twitter_demo/src/main/resources/single_topn_50_userlist.tsv").getLines.toList.map(_.split("\t").toList.take(2)).map(ls => (ls.head.toInt,ls.tail.head)).toMap
    var (targetId,sequences): (Int,List[(Int, Date, Array[Int])]) = proximities.find()
    var (seqNo,time,topk) = sequences.head; sequences = sequences.tail; 
    var visibleRelatedUsers = 10
    val maxVisibleRelatedUsers = 100
    var cameraDistance = 500
    var maxCameraDistance = 1000
    topk = topk.take(visibleRelatedUsers+1)
    var positions = new PositionsUpdater(topk)
    
    // animation status
    var isPlaying = true
    var r = 0

    // view 
    val miniWidth = 500
    val miniHeight = 500
    val defaultFont = createFont("Helvetica",32)
    val defaultFontSmall = createFont("Helvetica",18*2)
    val defaultFontMini = createFont("Helvetica",8)
    val arialFont = createFont("Arial",32)
    val arialFontSmall = createFont("Arial",16)
    val timeFont = createFont("Krungthep",32*2)
    val timeFormat = new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm")
    val bg = loadImage("bg5-5.png")
    val slideBar = loadImage("bar.png")
    val buttonPlay = loadImage("button_play.png")
    val buttonStop = loadImage("button_stop.png")
    val buttonEnd = loadImage("button_end.png")
    
    // objects
    var vartexCoordinates = Map[Int,(Float,Float,Float)]()
    var scaleBarCoordinates = ((0,0),(0,0));
    var zoomBarCoordinates =  ((0,0),(0,0));
    var buttonSwitchUser = ((777,497),(974,545));
    var buttonPlayCoordinates = (0,0);
    var buttonStopCoordinates = (0,0);
    var buttonEndCoordinates = (0,0);
      
    override def draw {
      drawInit
      background(bg)
      
      translate((width-miniWidth)/2,50){
	top {
	  textFont(defaultFontSmall)
	  fill(73,142,255)
	  scale(0.5f)
	  text("Target User : " + nameList(targetId), 10*2, 25*2)
	  text("Top3 : " + topk.drop(1).take(3).map(nameList(_)).mkString(", "), 10*2, 50*2)
	  scale(2.0f)
	  stroke(73*0.8f,142*0.8f,255*0.8f)
	  line(10,60,miniWidth-10,60)
	  stroke(73*0.6f,142*0.6f,255*0.6f)
	  line(10,61,miniWidth-10,61)
	  noStroke
	}

	translate(10,miniHeight-60){
	  textFont(timeFont)
	  fill(73,142,255)
	  val hereScale = 2
	  scale(1.0f/hereScale)
	  text(timeFormat.format(time),10*hereScale,40*hereScale)
	  scale(hereScale)
	  textFont(defaultFont) // fix later
	}
	
	translate(0,0,500-cameraDistance){
	  setAmbientRight
	
	  translate(miniWidth/2,miniHeight/2) {
	    rotateNext {
	      drawPositions(targetId,positions.get)
	      vartexCoordinates = positions.get.mapValues(xyz => (modelX(xyz),modelY(xyz),modelZ(xyz)))
	    }
	    
	    for((id,(x,y,z)) <- positions.get){
	      pushMatrix
	      rotateY(radians(r))
	      var (dx,dy,dz) = (0.0f,0.0f,0.0f)
	      translate(x,y,z){
		dx = modelX(x,y,z)
		dy = modelY(x,y,z)
		dz = modelZ(x,y,z)
	      }
	      popMatrix
	      pushMatrix
	      fill(255)
	      textFont(defaultFont)
	      scale(0.5f)
	      text(nameList(id),dx-miniWidth/2-(width-miniWidth)/2+60,dy-miniHeight/2-50,dz-(500-cameraDistance))
	      popMatrix
	    }

	  }
	}

      }
      noLights      
        
      imageMode(CORNER)
      // image(bg,0,0)

      translate(width/2,height-50){
	imageMode(CENTER)
	image(buttonEnd,-60,0)
	image(buttonPlay,0,0)
	image(buttonStop,60,0)
	buttonEndCoordinates = (screenX(-60,0,0).toInt,screenY(-60,0,0).toInt)
	buttonPlayCoordinates = (screenX(0,0,0).toInt,screenY(0,0,0).toInt)
	buttonStopCoordinates = (screenX(60,0,0).toInt,screenY(60,0,0).toInt)
      }

      translate(width/2+miniWidth/2,0){
	val barWidth = 137
	val barLeft = 95
	translate(barLeft,86){
	  scaleBarCoordinates = ((screenX(0,0,0).toInt,screenY(0,0,0).toInt),(screenX(barWidth,3,0).toInt,screenY(barWidth,3,0).toInt))
	  imageMode(CENTER)
	  image(slideBar,(visibleRelatedUsers.toFloat/maxVisibleRelatedUsers.toFloat)*barWidth,0)
	}
	translate(barLeft,154){
	  zoomBarCoordinates = ((screenX(0,0,0).toInt,screenY(0,0,0).toInt),(screenX(barWidth,3,0).toInt,screenY(barWidth,3,0).toInt))
	  imageMode(CENTER)
	  image(slideBar,barWidth-(cameraDistance.toFloat/maxCameraDistance.toFloat)*barWidth,0)
	}
      }
      
      sequences match {
	case head :: tail => {
	  updateTime
	  if (!(time.before(head._2))){
      	    seqNo = head._1
      	    time  = head._2
      	    topk  = head._3.take(visibleRelatedUsers+1)
	    sequences = tail
	    // println("update_bef:" + topk.mkString(","))
	    positions.update(topk)
	    // println("update_aft:" + topk.mkString(","))
	  }
	}
	case Nil => Nil
      }
      positions.next
  

    }


    // setting
    override def setup {
      super.setup
      frameRate(15)
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
	switchUser(targetId)
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
      else if(isInRectangle((mouseX,mouseY),scaleBarCoordinates)){
	visibleRelatedUsers = ((mouseX.toFloat - scaleBarCoordinates._1._1)/(scaleBarCoordinates._2._1 - scaleBarCoordinates._1._1)*maxVisibleRelatedUsers).toInt
	sequences match {
	  case head :: tail => {
      	    seqNo = head._1
      	    time  = head._2
      	    topk  = head._3.take(visibleRelatedUsers+1)
	    println("topk:" + topk.mkString(","))
	    sequences = tail
	    positions = new PositionsUpdater(topk)
	  }
	  case Nil => Nil
	}
      }
      else if(isInRectangle((mouseX,mouseY),zoomBarCoordinates)){
	cameraDistance = ((1 - (mouseX.toFloat - zoomBarCoordinates._1._1)/(zoomBarCoordinates._2._1 - zoomBarCoordinates._1._1))*maxCameraDistance).toInt
      }
      else if(isInRectangle((mouseX,mouseY),buttonSwitchUser)){
	switchUser
      }
    }

    def switchUser(userID:Int) {
      val proximity = proximities.find()
      targetId = proximity._1
      sequences = proximity._2
      seqNo = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3.take(visibleRelatedUsers+1)
      sequences = sequences.tail
      positions = new PositionsUpdater(topk)
    }

    def switchUser() {
      val proximity = proximities.find()
      targetId = proximity._1
      sequences = proximity._2
      seqNo = sequences.head._1
      time  = sequences.head._2
      topk  = sequences.head._3.take(visibleRelatedUsers+1)
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
    def screenX(xyz:(Int,Int,Int)) = { super.screenX(xyz._1,xyz._2,xyz._3)}
    def screenY(xyz:(Int,Int,Int)) = { super.screenY(xyz._1,xyz._2,xyz._3)}
    def screenZ(xyz:(Int,Int,Int)) = { super.screenZ(xyz._1,xyz._2,xyz._3)}
    def modelX(xyz:(Int,Int,Int)) = { super.modelX(xyz._1,xyz._2,xyz._3)}
    def modelY(xyz:(Int,Int,Int)) = { super.modelY(xyz._1,xyz._2,xyz._3)}
    def modelZ(xyz:(Int,Int,Int)) = { super.modelZ(xyz._1,xyz._2,xyz._3)}
    def translate(dx:Int, dy:Int)(f: => Unit) = { super.translate(dx,dy,0); f; super.translate(-dx,-dy,0);}
    def translate(dx:Int, dy:Int, dz:Int)(f: => Unit) = { super.translate(dx,dy,dz); f; super.translate(-dx,-dy,-dz);}
    def translate(dr:(Int,Int,Int))(f: => Unit) = { super.translate(dr._1,dr._2,dr._3); f; super.translate(-dr._1,-dr._2,-dr._3) }
    def lineToPosition(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
    def text(time:Date,x:Int,y:Int) = { super.text(new SimpleDateFormat("yyyy'年'MM'月'dd'日' kk:mm").format(time),x,y) }
    def isInSquare(point:(Int,Int), center:(Int,Int), r:Int) = {
      center._1-r <= point._1 && point._1 <= center._1+r  &&  center._2-r <= point._2 && point._2 <= center._2+r
    }
    def isInRectangle(point:(Int,Int), corners:((Int,Int),(Int,Int))) = {
      corners._1._1 <= point._1 && point._1 <= corners._2._1 && corners._1._2 <= point._2 && point._2 <= corners._2._2
    }
    
  }
  
  
  def main(args: Array[String]) {
    println("Application will be open.")
    
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
