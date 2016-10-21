generic
   type Element_Type is private;
package Unbounded_Queue is

   -- This package implements FIFO queue, a data structure in which
   --  elements are added to the rear and removed from the front;
   --  a "first in, first out" (FIFO) structure.
   --
   -- This package has been modified to appropriately model data used
   --  in the "Hump Yard" program.

   type Queue_Type (Max_Size : Positive) is tagged limited private;

   OVERFLOW  : exception;
   UNDERFLOW : exception;
   QUEUE_CONSTRAINT_ERROR : exception;

   --------------------------------------------------------------------------
   function Size (Queue : in Queue_Type) return Natural;
   -- returns the number of elements in Queue
   --
   -- Preconditions  : none
   --
   -- Postconditions : Size = (the number of elements in Queue)

   --------------------------------------------------------------------------
   function Value_At (Queue    : in Queue_Type;
                      Position : in Positive) return Element_Type;
   -- Returns the element at the given Position.  Position 1 is the front
   -- of the queue and Position Size(Queue) is the back of the queue.
   --
   -- Preconditions  : none
   --
   -- Postconditions : Value_At = (copy of element at Position)
   --
   -- Exceptions       UNDERFLOW              is raised if Queue is empty
   --                  QUEUE_CONSTRAINT_ERROR is raised Queue is not empty and
   --                                         Position  > Size(Queue)

   ----------------------------------------------------------------------------
   procedure Clear (Queue : in out Queue_Type);
   -- Purpose        : Remove all elements from the queue
   -- Preconditions  : None
   -- Postconditions : Queue is empty; it contains no elements

   ----------------------------------------------------------------------------
   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type);
   -- Purpose        : Adds Item to the rear of Queue
   -- Preconditions  : None
   -- Postconditions : Queue = original Queue with Item added to its rear
   -- Exceptions     : OVERFLOW  Raised on attempt to Enqueue an element onto
   --                            a full queue.  Queue is unchanged.

   ----------------------------------------------------------------------------
   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type);
   -- Purpose        : Removes the front element from Queue and returns it
   -- Preconditions  : None
   -- Postconditions : Queue = original Queue with front element removed
   --                  Item  = front element of original Queue
   -- Exceptions     : UNDERFLOW  Raised on attempt to dequeue an element from
   --                             an empty Queue.  Queue remains empty.

   ----------------------------------------------------------------------------
   function Full (Queue : in Queue_Type) return Boolean;
   -- Purpose        : Tests whether a queue is full.  A queue is full when
   --                     no more elements can be enqueued into it
   -- Preconditions  : None
   -- Postconditions : Full = (no more elements can be enqueued into Queue)

   ----------------------------------------------------------------------------
   function Empty (Queue : in Queue_Type) return Boolean;
   -- Purpose        : Tests whether a queue is empty (contains no elements)
   -- Preconditions  : None
   -- Postconditions : Empty = (Queue is empty)

private

   type Node_Type;
   type Node_Ptr is access Node_Type;
   type Node_Type is
      record
         Position : Positive;  -- Designates location of node in the queue
         Info     : Element_Type;
         Next     : Node_Ptr;
      end record;

   type Queue_Type (Max_Size : Positive) is tagged limited
      record
         Front    : Node_Ptr;  -- Designates first item in the queue
         Rear     : Node_Ptr;  -- Designates last item in the queue
      end record;

   procedure Finalize (Object : in out Queue_Type) renames Clear;

end Unbounded_Queue;
