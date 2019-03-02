/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import actorbintree.BinaryTreeNode.{CopyFinished, CopyTo}
import actorbintree.BinaryTreeSet._
import akka.actor._

import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
  case op: Operation => root ! op
  case GC =>
    val empty: ActorRef = createRoot
    root ! CopyTo(empty)
    context become garbageCollecting(empty)

  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
      case op: Operation =>
        pendingQueue = pendingQueue enqueue op
      case CopyFinished =>
        pendingQueue foreach {newRoot ! _}
        root ! PoisonPill
        root = newRoot
        pendingQueue = Queue.empty
        context become normal

  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  def chooseHop(e: Int): Position = if (e > elem) Right else Left

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Contains(requester, id, to_find) =>
      if (to_find != elem || (to_find == elem && removed)) subtrees get chooseHop(to_find) match {
        case Some(subtree) => subtree ! Contains(requester, id, to_find)
        case None => requester ! ContainsResult(id, result = false)
      } else requester ! ContainsResult(id, result = true)

    case Insert(requester, id, to_insert) =>

      if (to_insert != elem || (to_insert == elem && removed)) subtrees get chooseHop(to_insert) match {
        case Some(subtree) => subtree ! Insert(requester, id, to_insert)
        case None =>
          subtrees += (chooseHop(to_insert) -> context.actorOf(BinaryTreeNode.props(to_insert, initiallyRemoved = false)))
          requester ! OperationFinished(id)
      } else requester ! OperationFinished(id)

    case Remove(requester, id, to_delete) =>
      if (to_delete != elem || (to_delete == elem && removed)) subtrees get chooseHop(to_delete) match {
        case Some(subtree) => subtree ! Remove(requester, id, to_delete)
        case None => requester ! OperationFinished(id)
      } else {
        removed = true
        requester ! OperationFinished(id)
      }

    case CopyTo(newRoot) =>
      if (!removed) newRoot ! Insert(self, elem, elem)
      subtrees.values foreach {_ ! CopyTo(newRoot)}
      if (removed && subtrees.isEmpty) {
        context.parent ! CopyFinished
      }
      else context become copying(subtrees.values.toSet, insertConfirmed = removed)
  }


  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(_) =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
      } else context become copying(expected, insertConfirmed = true)

    case CopyFinished =>
      val waitingFor: Set[ActorRef] = expected - sender
      if (insertConfirmed && waitingFor.isEmpty) {
        context.parent ! CopyFinished
      } else context become copying(waitingFor, insertConfirmed)
  }
}
