with Ada.Unchecked_Deallocation;
package body Unbounded_Queue is

   -- Package body modified by   Vincent T. Mossman
   -- Date                       April 3, 2014

   -- Instantiate a procedure to recycle node memory
   procedure Free is new Ada.Unchecked_Deallocation (Object => Node_Type,
                                                     Name   => Node_Ptr);

   --------------------------------------------------------------------------

   function Size (Queue : in Queue_Type) return Natural is

      -- Variables
      Result    : Natural;  -- Size of Queue
      Queue_Ptr : Node_Ptr; -- Node alias

   begin   -- Size

      -- Initialize Variables
      Result := 0;
      Queue_Ptr := Queue.Front;

      -- Loop determines the size of a queue using a count
      -- Each iteration, increments the count by one
      loop
         exit when Queue_Ptr = null;
         Queue_Ptr := Queue_Ptr.all.Next;
         Result := Result + 1;
      end loop;

      return Result;
   end Size;

   --------------------------------------------------------------------------

   function Value_At (Queue    : in Queue_Type;
                      Position : in Positive) return Element_Type is

      -- Variables
      Queue_Ptr : Node_Ptr; -- Node alias

   begin   -- Value_At

      -- Initialize Variables
      Queue_Ptr := Queue.Front;

      -- Loop traverses queue until desired node is found
      -- Each iteration, processes one node
      for Index in 1 .. Position - 1 loop
         Queue_Ptr := Queue_Ptr.all.Next;
         if Queue_Ptr = null then
            raise QUEUE_CONSTRAINT_ERROR;
         end if;
      end loop;

      return Queue_Ptr.all.Info;

   end Value_At;

   ----------------------------------------------------------------------------

   procedure Clear (Queue : in out Queue_Type) is

      -- Variables
      To_Recycle : Node_Ptr; -- For recycling nodes

   begin   -- Clear

      -- Loop empties a queue
      -- Each iteration, frees one node
      loop
         exit when Queue.Front = null;
         To_Recycle  := Queue.Front;           -- Unlink the
         Queue.Front := Queue.Front.all.Next;  --    front node
         Free (To_Recycle);                    -- Recycle the node
      end loop;

      Queue.Rear := null; -- Clean up Rear pointer

   end Clear;

   ----------------------------------------------------------------------------

   procedure Enqueue (Queue : in out Queue_Type;
                      Item  : in     Element_Type) is

   begin   -- Enqueue

      if Queue.Front = null then
         Queue.Front := new Node_Type'(Position => 1,
                                       Info => Item,
                                       Next => null);
         Queue.Rear  := Queue.Front;
      elsif Queue.Rear.all.Position < Queue.Max_Size then
         Queue.Rear.all.Next := new Node_Type'(Position => Queue.Rear.all.Position + 1,
                                               Info => Item,
                                               Next => null);
         Queue.Rear := Queue.Rear.all.Next;
      else
         raise OVERFLOW;
      end if;

   exception
      when STORAGE_ERROR =>
         raise OVERFLOW;

   end Enqueue;

   ----------------------------------------------------------------------------

   procedure Dequeue (Queue : in out Queue_Type;
                      Item  :    out Element_Type) is

      -- Variables
      To_Recycle : Node_Ptr; -- For recycling nodes

   begin   -- Dequeue

      if Queue.Front = null then
         raise UNDERFLOW;
      end if;

      Item := Queue.Front.all.Info;        -- Get the value from the front node
      To_Recycle  := Queue.Front;          -- Save access to old front
      Queue.Front := Queue.Front.all.Next; -- Change the front
      Free (To_Recycle);                   -- Recycle the memory
      if Queue.Front = null then           -- Is the queue now empty?
         Queue.Rear := null;               -- Set Rear to null as well
      end if;

   end Dequeue;

   --------------------------------------------------------------------------

   function Full (Queue : in Queue_Type) return Boolean is

   begin   -- Full

      return Queue.Rear.all.Position = Queue.Max_Size;

   end Full;

   --------------------------------------------------------------------------

   function Empty (Queue : in Queue_Type) return Boolean is

   begin   -- Empty

      return Queue.Front = null;

   end Empty;

end Unbounded_Queue;
