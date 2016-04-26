package backend.ast

object SequenceType extends Enumeration {
  type SequenceType = Value
  val BeginSequence, AndSequence, OrSequence = Value
}
