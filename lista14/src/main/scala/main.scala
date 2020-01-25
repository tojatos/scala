import scala.swing._
import scala.swing.event._

object TextFields extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Text Fields"
    size = new Dimension(200, 100)
    preferredSize = size
    resizable = false
    val inText = new TextField(10)
    val outText = new TextField(10) { editable = false }
    val inLabel = new Label(" input")
    val outLabel = new Label("output")
    contents = new FlowPanel() {
      contents += inLabel
      contents += inText
      contents += outLabel
      contents += outText
    }
    listenTo(inText)
    reactions += {
      case EditDone(_) =>
        outText.text = inText.text
        inText.text = ""
    }
    centerOnScreen()
  }
}