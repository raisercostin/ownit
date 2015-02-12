package org.raisercostin.util.gui

import javax.swing._
import javax.swing.event._
import javax.swing.text._

class LimitLinesDocumentListener(maximumLines1: Int, private var isRemoveFromStart: Boolean) extends DocumentListener {

  private var maximumLines: Int = _

  setLimitLines(maximumLines1)

  def this(maximumLines: Int) {
    this(maximumLines, true)
  }

  def getLimitLines(): Int = maximumLines

  def setLimitLines(maximumLines: Int) {
    if (maximumLines < 1) {
      val message = "Maximum lines must be greater than 0"
      throw new IllegalArgumentException(message)
    }
    this.maximumLines = maximumLines
  }

  def insertUpdate(e: DocumentEvent) {
    SwingUtilities.invokeLater(new Runnable() {

      def run() {
        removeLines(e)
      }
    })
  }

  def removeUpdate(e: DocumentEvent) {
  }

  def changedUpdate(e: DocumentEvent) {
  }

  private def removeLines(e: DocumentEvent) {
    val document = e.getDocument
    val root = document.getDefaultRootElement
    while (root.getElementCount > maximumLines) {
      if (isRemoveFromStart) {
        removeFromStart(document, root)
      } else {
        removeFromEnd(document, root)
      }
    }
  }

  private def removeFromStart(document: Document, root: Element) {
    val line = root.getElement(0)
    val end = line.getEndOffset
    try {
      document.remove(0, end)
    } catch {
      case ble: BadLocationException => println(ble)
    }
  }

  private def removeFromEnd(document: Document, root: Element) {
    val line = root.getElement(root.getElementCount - 1)
    val start = line.getStartOffset
    val end = line.getEndOffset
    try {
      document.remove(start - 1, end - start)
    } catch {
      case ble: BadLocationException => println(ble)
    }
  }
}
