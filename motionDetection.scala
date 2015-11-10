import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

import java.net._
import scala.math.{sqrt, pow}

import scala.swing._
import javax.swing.{Icon, ImageIcon}
import java.awt.GridLayout


Authenticator.setDefault( new Authenticator() {
		override def getPasswordAuthentication = {
			new PasswordAuthentication("admin", "admin".toCharArray)
		}
	}
)

val imgURL = "http://192.168.1.123:81/tmpfs/auto.jpg"

def distance(c1: Color, c2: Color): Double = {
	sqrt(
		pow(c1.getRed - c2.getRed, 2) + 
		pow(c1.getGreen - c2.getGreen, 2) +
		pow(c1.getBlue - c2.getBlue, 2) 
	)
}

def getImg: BufferedImage = {
	val url = new URL(imgURL)
	val img = ImageIO.read(url)
	img
}


def diff(img1: BufferedImage, img2: BufferedImage): Int = {
	
	val width = img1.getWidth
	val height = img2.getHeight
	val difs = for {
		i <- 0 until width; 
		j <- 0 until height;
		if distance(new Color(img1.getRGB(i, j)), new Color(img2.getRGB(i, j))) > 100
	} yield 1;	
	difs.length	
}




object MotionDetection extends SimpleSwingApplication {
	
	var img = new Label {
		icon = Swing.Icon(getImg)
	}
	
	val panel = new FlowPanel(img)

	def top = new MainFrame {
		title = "Motion Detection"
		contents = panel
	}
}





new Thread {
	override def run {
		var last = getImg
		
		
		while (true) {
			Thread.sleep(250)
			val newImg = getImg
			MotionDetection.img.icon = Swing.Icon(newImg)
			
			MotionDetection.panel.repaint
			println(diff(last, newImg))
			//println(diff2(imgs._2, last._2))
			last = newImg			
		}
	}
}.start


MotionDetection.main(Array())
