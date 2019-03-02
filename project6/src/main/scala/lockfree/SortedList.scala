package lockfree

class SortedList extends AbstractSortedList {

  // The sentinel node at the head.
  private val _head = createNode(0, None, isHead=true)

  // The first logical node is referenced by the head.
  def firstNode: Option[Node] = _head.next

  // Finds the first node whose value satisfies the predicate.
  // Returns the predecessor of the node and the node.
  def findNodeWithPrev(pred: Int => Boolean): (Node, Option[Node]) = {

    def helper( previousNode : Node, currentNode : Option[Node]): (Node, Option[Node]) = (previousNode, currentNode) match {
        case (prev, Some(curr)) => {
          if (curr.deleted) {
            prev.atomicState.compareAndSet((currentNode, false), (curr.next, false))
            findNodeWithPrev(pred)
          } else {
            if (pred(curr.value)) {
              (previousNode, currentNode)
            } else {
            helper(curr, curr.next)
            }
          }
        }
        case (last, None) => (last, None)
      }


      helper(_head, firstNode)



  }

  // Insert an element in the list.
  def insert(e: Int): Unit = {
    val (prev, next) : (Node, Option[Node]) = findNodeWithPrev(_ >= e)

    val current : Node = createNode(e, next)

    if ( !prev.atomicState.compareAndSet((next,false ) , (Some(current) ,false))) {
      insert(e)
    }


  }

  // Checks if the list contains an element.
  def contains(e: Int): Boolean = {
    val (_, next) : (Node, Option[Node]) = findNodeWithPrev(_ == e)
    next match {
      case None => false
      case Some(x) => !x.deleted

    }

  }

  // Delete an element from the list.
  // Should only delete one element when multiple occurences are present.
  def delete(e: Int): Boolean = {
    findNodeWithPrev(_ == e)._2 match {
      case Some(x) => if (!x.mark) delete(e) else true
      case None => false
    }
  }



}
