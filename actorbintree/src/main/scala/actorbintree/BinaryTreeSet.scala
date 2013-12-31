/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

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
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = { 
    case o:Operation =>{
      //println(root)
      root ! o
    }
    case GC =>{
      var newRoot=createRoot
      //println("got gc newRoot="+newRoot+" oldRoot="+root)
      context.become(garbageCollecting(newRoot))
      root!(new CopyTo(newRoot))
    }

    
    case any:Any => println("got any in BinaryTreeSet"); 
    }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op:Operation => {
      pendingQueue = pendingQueue.enqueue(op)
      }
    case CopyFinished => {
      //println("current root copied!!!!!")
      context.stop(root)
      //root ! PoisonPill
      while(pendingQueue.size >0) {
        val (operation,q) = pendingQueue.dequeue
        pendingQueue = q
        //println("sending to the new queue")
        newRoot!operation
      }
        //println(pendingQueue.size)
      root=newRoot
      context.become(normal)
    }
    case GC =>
    {
      //println (" got GC but ignoring, since a GC operation is already in progress")
    }
    
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
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = { 
    case i:Insert =>{
      if(elem==i.elem){
        removed=false
        i.requester ! OperationFinished(i.id)
      }else
      {
        val newElemPos=if(elem<i.elem) Right else Left;
        if(subtrees.contains(newElemPos))
        {
          val child= subtrees(newElemPos)
          child ! i
        }
        else{
          val newChild=context.actorOf(BinaryTreeNode.props(i.elem, initiallyRemoved = false))
          subtrees+=((newElemPos,newChild))
          newChild !i
        }
          
      }
    }
      case r:Remove => {
	      if(elem==r.elem){
	        removed=true
	        r.requester ! OperationFinished(r.id)
	      }else
	      {
	        val newElemPos=if(elem<r.elem) Right else Left;
	        if(subtrees.contains(newElemPos))
	        {
	          val child= subtrees(newElemPos)
	          child ! r
	        }
	        else{
	          r.requester ! OperationFinished(r.id)
	        }
	          
	      }
      }
      case c:Contains => {
	      if(elem==c.elem){
	        c.requester ! ContainsResult (c.id,!removed)
	      }else{
	        val newElemPos=if(elem<c.elem) Right else Left;
	        if(subtrees.contains(newElemPos))
	        {
	          val child= subtrees(newElemPos)
	          child ! c
	        }
	        else{
	          c.requester ! ContainsResult (c.id,false)
	        }
	        
	      }
        
      }
      case ct:CopyTo =>{
	      val setOfChild = (for(p <- subtrees) yield p._2)
	      
	      if(removed && setOfChild.isEmpty ){
	        sender ! CopyFinished
	      }
	      else
	      {
		      context.become(copying(setOfChild.toSet[ActorRef], removed))
		      if(!removed){
		        ct.treeNode!Insert(self, id = elem, elem)
		      }
		   	  for(child <- setOfChild) child!ct
	      }
        
      }

     
  case any:Any=> println("root got unprocessed message "+any); 
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(senders: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case o:OperationFinished =>{
      if(senders.isEmpty){
        //println(self)
        context.parent ! CopyFinished
      }else
      context.become(copying(senders, true))
    }
    case CopyFinished =>{
      val newExpected=senders-sender
      if(newExpected.isEmpty && insertConfirmed){
        //println(self)
        context.parent ! CopyFinished
      }else{
        context.become(copying(newExpected, insertConfirmed))
      }
    }
    case a:Any => println("unknown message:"+a)
    
  }

}
