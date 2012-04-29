import java.applet.Applet
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import processing.core._
import processing.core.PConstants._
import com.nootropic.processing.layers._


class Embedded extends PApplet with MyPExtention with MyDrawingTools {
  // data
  val proximities = new Proximities(scala.io.Source.fromURL(getClass.getResource("/10users_topn_100.json")).getLines)
  val nameList = scala.io.Source.fromURL(getClass.getResource("/10users_topn_100_userlist.tsv")).getLines.toList.map(_.split("\t").toList.take(2)).map(ls => (ls.head.toInt,ls.tail.head)).toMap
  var (targetId,sequences): (Int,List[(Int, Date, Array[Int])]) = proximities.find()
  var (seqNo,time,topk) = sequences.head; sequences = sequences.tail; 
  var visibleRelatedUsers = 50
  val maxVisibleRelatedUsers = 100
  var cameraDistance = 500
  val maxCameraDistance = 1000
  val minCameraDistance = 0
  val rotationSpeed = 1.0f/3.0f
  topk = topk.take(visibleRelatedUsers+1)
  var positions = new PositionsUpdater(topk)
  
  // animation status
  var isPlaying = true
  var isRotation = true
  var r = 0.0f

  // view
  var frameLayer:FrameLayer = null
  val miniWidth = 500
  val miniHeight = 500
  val defaultFont = createFont("Helvetica",32)
  val defaultFontSmall = createFont("Helvetica",18*2)
  val timeFont = createFont("Krungthep",32*2)
  val timeFormat = new java.text.SimpleDateFormat("yyyy/MM/dd HH:mm")
  val bg = loadImage("bg1.png")
  val top_bg = loadImage("bg2.png")
  val slideBar = loadImage("bar.png")
  val check = loadImage("check.png")
  val buttonPlay = loadImage("button_play.png")
  val buttonStop = loadImage("button_stop.png")
  val buttonEnd = loadImage("button_end.png")
  
  // object coordinates 
  var scaleBarCoordinates = ((0,0),(0,0));
  var zoomBarCoordinates =  ((0,0),(0,0));
  var checkBoxCoordinates =  ((893,205),(921,233));
  var buttonSwitchUser = ((777,497),(974,545));
  var buttonPlayCoordinates = (0,0);
  var buttonStopCoordinates = (0,0);
  var buttonEndCoordinates = (0,0);
  
  override def draw {
    drawInit
    background(bg)
    
    translate((width-miniWidth)/2,50){
      translate(0,0,minCameraDistance-cameraDistance){
	setAmbientRight
	translate(miniWidth/2,miniHeight/2) {
	  /// draw vertex
	  if (isRotation)
	    rotateNext { drawPositions(targetId,positions.get) }
	  else
	    rotateYDraw(myRadians(r)*rotationSpeed){ drawPositions(targetId,positions.get) }
	  // draw user name
	  for((id,(x,y,z)) <- positions.get){
	    pushMatrix
	    rotateY(myRadians(r)*rotationSpeed)
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
	    text(nameList(id),dx-miniWidth/2-(width-miniWidth)/2+60,dy-miniHeight/2-50,dz+cameraDistance)
	    popMatrix
	  }
	}
      }
    }
    
    noLights

    // buttons
    translate(width/2,height-50){
      imageMode(CENTER)
      image(buttonEnd,-60,0)
      image(buttonPlay,0,0)
      image(buttonStop,60,0)
      buttonEndCoordinates = (screenX(-60,0,0).toInt,screenY(-60,0,0).toInt)
	buttonPlayCoordinates = (screenX(0,0,0).toInt,screenY(0,0,0).toInt)
	  buttonStopCoordinates = (screenX(60,0,0).toInt,screenY(60,0,0).toInt)
    }
    // slide bars
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
    // check box
    imageMode(CORNER)
    if(isRotation) image(check,895,198)

    // update data
    sequences match {
      case head :: tail => {
	updateTime
	if (!(time.before(head._2))){
      	  seqNo = head._1
      	  time  = head._2
      	  topk  = head._3.take(visibleRelatedUsers+1)
	  sequences = tail
	  positions.update(topk)
	}
      }
      case Nil => Nil
    }
    positions.next
    

  }

  // setting
  override def setup {
    super.setup
    frameLayer = new FrameLayer(this,top_bg)
    layers.addLayer(frameLayer)
    frameRate(15)
    size(1000,650,P3D)
    textFont(defaultFont)
  }
  def setAmbientRight {
    directionalLight(255, 255, 255, 0, 0, -1)
    ambientLight(102, 102, 102)
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
	else if(isInRectangle((mouseX,mouseY),checkBoxCoordinates)){
	  isRotation = if (isRotation) false else true
	}
  }

  def switchUser(userID:Int) {
    val proximity = proximities.find(userID)
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
    rotateY(myRadians(r)*rotationSpeed)
    f
    rotateY(myRadians(-r)*rotationSpeed)
    if (r == 360)
      r == 0
  }
  def updateTime {
    val interval = 2
    val cal = Calendar.getInstance(); cal.setTime(time); cal.add(Calendar.MINUTE, interval)
    time = cal.getTime()
  }
  
}

trait MyDrawingTools extends PApplet with MyPExtention {
  def drawPositions(targetId:Int, positions:Map[Int,(Int,Int,Int)]) {
    for((id,position) <- positions) {
      translate(position) {
	if (id == targetId){
	  fill(0,200,200)
	  sphere(30)
	}
	else {
	  fill(235,96,160)
	  sphere(30)
	}
      }
      strokeAndSmoothDraw(255) {
	stroke(255,200)
	lineToPosition(position)
      }
    }
  }
}

trait MyPExtention extends PApplet {
  var layers:AppletLayers = null
  override def paint(g:java.awt.Graphics) {
    // This method MUST be present in your sketch for layers to be rendered
    if (layers != null) 
      layers.paint(this)
    else 
      super.paint(g)
  }

  override def setup {
    layers = new AppletLayers(this)
    noSmooth
    noStroke
  }
  
  def drawInit {
    background(0)
    fill(255)
  }
  
  def myRadians(i:Float):Float = (i * 3.141592f)/180.0f
  def smoothDraw(f: => Unit) = {smooth(); f; noSmooth()}
  def strokeDraw(color:Int)(f: => Unit) = {stroke(color); f; noStroke()}
  def strokeAndSmoothDraw(color:Int)(f: => Unit) = {stroke(color); smooth(); f; noStroke(); noSmooth()}
  def scaleDraw(s:Float)(f: => Unit) = {scale(s); f; scale(1.0f/s)}
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
  def rotateYDraw(r:Float)(f: => Unit) = { super.rotateY(r); f; super.rotateY(-r) }
  def lineToPosition(dr:(Int,Int,Int)) = line(0,0,0,dr._1,dr._2,dr._3)
  def text(time:Date,x:Int,y:Int) = { super.text(new SimpleDateFormat("yyyy'年'MM'月'dd'日' kk:mm").format(time),x,y) }
  def isInSquare(point:(Int,Int), center:(Int,Int), r:Int) = {
    center._1-r <= point._1 && point._1 <= center._1+r  &&  center._2-r <= point._2 && point._2 <= center._2+r
  }
  def isInRectangle(point:(Int,Int), corners:((Int,Int),(Int,Int))) = {
    corners._1._1 <= point._1 && point._1 <= corners._2._1 && corners._1._2 <= point._2 && point._2 <= corners._2._2
  }
  
}

class FrameLayer(parent:Embedded,bg:PImage) extends Layer(parent) {
  
  override def draw {
    background(0,0)
    imageMode(CORNER)
    image(bg,0,0)

      translate((parent.width-parent.miniWidth)/2,50)
	textFont(parent.defaultFontSmall)
	fill(73,142,255)
	val hereScale = 0.5f
	scale(hereScale)
	  text("Analized user : " + parent.nameList(parent.targetId), 10*(1.0f/hereScale), 25*(1.0f/hereScale))
	  text("Most related : " + parent.topk.drop(1).take(3).map(parent.nameList(_)).mkString(", ") + ", ...", 10*(1.0f/hereScale), 50*(1.0f/hereScale))
	scale(1/hereScale)
      
	stroke(73*0.8f,142*0.8f,255*0.8f)
	line(10,60,parent.miniWidth-10,60)
	stroke(73*0.6f,142*0.6f,255*0.6f)
	line(10,61,parent.miniWidth-10,61)
	noStroke
      
	translate(10,parent.miniHeight-60)
	  textFont(parent.timeFont)
	  fill(53,122,235)
	  val hereScale2 = 0.5f
	  scale(hereScale2)
	    text(parent.timeFormat.format(parent.time),10*(1.0f/hereScale2),40*(1.0f/hereScale2))
	  scale(1/hereScale2)
	translate(-10,-(parent.miniHeight-60))
      translate(-(parent.width-parent.miniWidth)/2,-50)
    
    translate(parent.width/2+parent.miniWidth/2,0)
    val barWidth = 137
    val barLeft = 95
    translate(barLeft,86)
    imageMode(CENTER)
    image(parent.slideBar,(parent.visibleRelatedUsers.toFloat/parent.maxVisibleRelatedUsers.toFloat)*barWidth,0)
    translate(-barLeft,-86)
    translate(barLeft,154)
    imageMode(CENTER)
    image(parent.slideBar,barWidth-(parent.cameraDistance.toFloat/parent.maxCameraDistance.toFloat)*barWidth,0)
    translate(-barLeft,-154)
    translate(-(parent.width/2+parent.miniWidth/2),0)

    imageMode(CORNER)
    if(parent.isRotation) image(parent.check,895,198)
    
  }
}

object ApplicationMain  {
  def main(args: Array[String]) {
    val embeddedApplet = new Embedded
    PApplet.runSketch(Array("title"), embeddedApplet)
  }
}
