package gui

import java.io._
import java.awt._
import javax.swing._
import javax.swing.event._
import javax.swing.text._

class MessageConsole(private var textComponent: JTextComponent, private var isAppend: Boolean) {

  private var document: Document = textComponent.getDocument

  private var limitLinesListener: DocumentListener = _

  textComponent.setEditable(false)

  def this(textComponent: JTextComponent) {
    this(textComponent, true)
  }

  def redirectOut() {
    redirectOut(null, null)
  }

  def redirectOut(textColor: Color, printStream: PrintStream) {
    val cos = new ConsoleOutputStream(textColor, printStream)
    System.setOut(new PrintStream(cos, true))
  }

  def redirectErr() {
    redirectErr(null, null)
  }

  def redirectErr(textColor: Color, printStream: PrintStream) {
    val cos = new ConsoleOutputStream(textColor, printStream)
    System.setErr(new PrintStream(cos, true))
  }

  def setMessageLines(lines: Int) {
    if (limitLinesListener != null) document.removeDocumentListener(limitLinesListener)
    limitLinesListener = new LimitLinesDocumentListener(lines, isAppend)
    document.addDocumentListener(limitLinesListener)
  }

  class ConsoleOutputStream(textColor: Color, private var printStream: PrintStream) extends ByteArrayOutputStream {

    private val EOL = System.getProperty("line.separator")

    private var attributes: SimpleAttributeSet = _

    private var buffer: StringBuffer = new StringBuffer(80)

    private var isFirstLine: Boolean = _

    if (textColor != null) {
      attributes = new SimpleAttributeSet()
      StyleConstants.setForeground(attributes, textColor)
    }

    if (isAppend) isFirstLine = true

    override def flush() {
      val message = toString
      if (message.length == 0) return
      if (isAppend) handleAppend(message) else handleInsert(message)
      reset()
    }

    private def handleAppend(message: String) {
      if (document.getLength == 0) buffer.setLength(0)
      if (EOL == message) {
        buffer.append(message)
      } else {
        buffer.append(message)
        clearBuffer()
      }
    }

    private def handleInsert(message: String) {
      buffer.append(message)
      if (EOL == message) {
        clearBuffer()
      }
    }

    private def clearBuffer() {
      if (isFirstLine && document.getLength != 0) {
        buffer.insert(0, "\n")
      }
      isFirstLine = false
      val line = buffer.toString
      try {
        if (isAppend) {
          val offset = document.getLength
          document.insertString(offset, line, attributes)
          textComponent.setCaretPosition(document.getLength)
        } else {
          document.insertString(0, line, attributes)
          textComponent.setCaretPosition(0)
        }
      } catch {
        case ble: BadLocationException => 
      }
      if (printStream != null) {
        printStream.print(line)
      }
      buffer.setLength(0)
    }
  }
}
