module SequenceQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import qualified Data.Sequence as Sequence
import AbstractQueue

newtype Queue t = QueueImpl (Sequence.Seq t)

instance AbstractQueue Queue where
    empty = QueueImpl Sequence.empty
    isEmpty (QueueImpl s) = Sequence.null s
    enqueue (QueueImpl s) x = QueueImpl (x Sequence.<| s)
    dequeue (QueueImpl s) = (x, QueueImpl q) where
        (q Sequence.:> x) = Sequence.viewr s