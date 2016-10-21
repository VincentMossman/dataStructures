package body Stack is


   procedure Clear (Stack : in out Stack_Type) is
   begin
      Stack.Top := 0;
   end Clear;

   ----------------------------------------------------------------------------
   function Empty (Stack : in Stack_Type) return Boolean is
   begin
      return Stack.Top = 0;
   end Empty;

   ----------------------------------------------------------------------------
   function Full (Stack : in Stack_Type) return Boolean is
   begin
      return Stack.Top = Stack.Max_Size;
   end Full;

   ----------------------------------------------------------------------------
   procedure Push (Stack       : in out Stack_Type;
                   New_Element : in     Element_Type) is
   begin
      if not Full (Stack) then
         Stack.Top := Stack.Top + 1;
         Stack.Elements (Stack.Top) := New_Element;
      else
         raise OVERFLOW;
      end if;
   end Push;

   ----------------------------------------------------------------------------
   procedure Pop (Stack          : in out Stack_Type;
                  Popped_Element :    out Element_Type) is
   begin
      if not Empty (Stack) then
         Popped_Element := Stack.Elements (Stack.Top);
         Stack.Top := Stack.Top - 1;
      else
         raise UNDERFLOW;
      end if;
   end Pop;
end Stack;
