generic
   type Element_Type is private;  -- The stack element type
package Stack is

-- This package implements a stack, a data structure in which 
-- elements are added and removed from only one end.  
-- A "last in, first out" (LIFO) structure.

   type Stack_Type (Max_Size : Natural) is tagged limited private;

   UNDERFLOW : exception;
   OVERFLOW  : exception;

   ----------------------------------------------------------------------------
   procedure Clear (Stack : in out Stack_Type);
   -- Purpose        : Remove all elements from the stack
   -- Preconditions  : None
   -- Postconditions : Stack is empty; it contains no elements

   ----------------------------------------------------------------------------
   function Empty (Stack : in Stack_Type) return Boolean;
   -- Purpose        : Tests whether a stack is empty (contains no elements)
   -- Preconditions  : None
   -- Postconditions : Empty = (stack is empty)

   ----------------------------------------------------------------------------
   function Full (Stack : in Stack_Type) return Boolean;
   -- Purpose        : Tests whether a stack is full.  A stack is full when
   --                     no more elements can be pushed on it
   -- Preconditions  : None
   -- Postconditions : Full = (no more elements can be pushed onto Stack)

   ----------------------------------------------------------------------------
   procedure Push (Stack       : in out Stack_Type;
                   New_Element : in     Element_Type);
   -- Purpose        : Adds New_Element to the top of Stack
   -- Preconditions  : None
   -- Postconditions : Stack = original Stack with New_Element added on top
   -- Exceptions     : OVERFLOW  Raised on attempt to Push a new element onto
   --                            a full stack.  Stack is unchanged.
   ----------------------------------------------------------------------------
   procedure Pop (Stack          : in out Stack_Type;
                  Popped_Element :    out Element_Type);
   -- Purpose        : Removes the top element from Stack and returns it
   -- Preconditions  : None
   -- Postconditions : Stack          = original Stack with top element removed
   --                  Popped_Element = top element of original Stack
   -- Exceptions     : UNDERFLOW  Raised on attempt to Pop an element from an 
   --                             empty stack.  Stack remains empty.

private

   type Stack_Array is array (Positive range <>) of Element_Type;
   type Stack_Type (Max_Size : Natural) is tagged limited 
      record
         Top      : Natural := 0;
         Elements : Stack_Array (1..Max_Size);
      end record;

end Stack;