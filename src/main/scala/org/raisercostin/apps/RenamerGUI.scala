package org.raisercostin.apps

import scala.swing._
import scala.swing.BorderPanel.Position._
import event._
import java.awt.{ Color, Graphics2D }
import scala.util.Random
import org.raisercostin.util.gui.MessageConsole
object RenamerGUI extends SimpleSwingApplication {
  import javax.swing.UIManager
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
  def getDirectoryListing(title: String = "", default:Option[String]): Option[java.io.File] = {
    val chooser = new FileChooser(default.map(x=>new java.io.File(x)).getOrElse(null))
    chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      import collection.JavaConverters._
      Some(chooser.selectedFile)
    } else None
  }
  
  var args:Array[String] = Array()
  override def startup(args: Array[String]){
    this.args = args
    super.startup(args)
  }

  def top = new MainFrame { // top is a required method
    title = "Organize "
    val textArea = new TextArea {
      text = ""
      background = Color.white
    }
    val scroll = new ScrollPane(textArea) 
    contents = new BorderPanel {
      layout(scroll) = Center
    }
    size = new Dimension(600, 400)
    centerOnScreen()
    val console = new MessageConsole(textArea.peer, true)
    console.redirectOut()
    console.redirectErr(Color.RED, null)

    val file = getDirectoryListing("titlu",args.headOption)
    title = "organize " + file.map(_.getAbsolutePath()).getOrElse("")
    //in your swing gui event listener (e.g. button clicked, combo selected, ...)
    import scala.concurrent.future
    //needed to execute futures on a default implicit context
    import scala.concurrent.ExecutionContext.Implicits._
    future {
      Renamer.organize(file.map(x => x.getAbsolutePath))
    }
  }
}
