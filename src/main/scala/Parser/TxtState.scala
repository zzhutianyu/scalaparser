package Parser

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, TreeMap}

class TxtState(val txt: String, val newLine: Char = '\n') extends CommonState[Char] {
  override val content: Seq[Char] = txt.toCharArray.toSeq

  val lines = {
    val txtLength = txt.length
    @tailrec
    @inline
    def stringToLines(txt: String, nextIndex: Int, next: SortedMap[Int, Int]): SortedMap[Int, Int] = nextIndex match {
      case _nextIndex if _nextIndex == txtLength => next
      case _ => txt.charAt(nextIndex) match {
        case _newLine if _newLine == newLine => {
          stringToLines(txt, nextIndex + 1, next + (next.lastKey + 1 -> nextIndex))
        }
        case _ => stringToLines(txt, nextIndex + 1, next)
      }
    }
    val res = stringToLines(txt, 0, TreeMap(-1 -> 0)) - -1
  }

  def lineByIndex(int: Int) = ???

}

object TxtState {
  def apply(txt: String, newLine: Char = '\n'): TxtState = new TxtState(txt, newLine)
}
