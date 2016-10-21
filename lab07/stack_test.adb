-- Written by Walter Beck

-- Test the two additional operations in the Stack package:

--    Size      is a function which should return the number of
--              elements on the stack

--    Value_At  is procedure which allows access to stack elements
--              without modifying the content of the stack.
--              Can raise UNDERFLOW or STACK_CONSTRAINT

   with Ada.Text_IO;
   with Stack;
   procedure Stack_Test is

      package Char_Stack is new Stack (Element_Type => Character);
      package Int_Stack  is new Stack (Element_Type => Integer);

      Characters : Char_Stack.Stack_Type (Max_Size => 50);
      Ints       : Int_Stack.Stack_Type  (Max_Size => 20);

      Character_Data : constant String := "Some characters to push onto the stack";

      Counter     : Natural;  -- Number of errors detected
      Error_Count : Natural;
      Ch      : Character;    -- Used to capture Value_At value
      Int     : Integer;      -- Used to capture Value_At value

   begin
   -- Put some data in the character stack.  Count how much and compare to the
   -- value returned by function Size;
      Ada.Text_IO.Put_Line ("****** Start of Testing ******");
      Counter     := 0;
      Error_Count := 0;
      Ada.Text_IO.Put_Line ("Starting Size test on Character data.");
      Characters.Clear;
      if Characters.Size /= 0 then
         Ada.Text_IO.Put_Line ("Char_Stack.Size failed");
         Error_Count := Error_Count + 1;
      end if;
      for I in reverse Character_Data'Range loop
         Characters.Push (New_Element => Character_Data (I));
         Counter := Counter + 1; -- Should match the count on the stack
         if Characters.Size /= Counter then
            Ada.Text_IO.Put_Line ("Char_Stack.Size failed");
            Error_Count := Error_Count + 1;
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Integer'Image (Error_Count) & " errors detected");
      Ada.Text_IO.Put_Line ("Ending Size test on Character data.");
      Ada.Text_IO.New_Line;


   -- Now do a test of Value_At for the character stack
      Counter     := 0;
      Error_Count := 0;
      Ada.Text_IO.Put_Line ("Starting Value_At test on Character data.");
      for I in 1 .. Characters.Size loop
         Counter := Counter + 1;
         Ch := Characters.Value_At (Position => Counter);
         if Ch /= Character_Data (Counter) then
            Ada.Text_IO.Put_Line ("Char_Stack.Value_At failed");
            Error_Count := Error_Count + 1;
         end if;
      end loop;
      Ada.Text_IO.Put_Line (Integer'Image (Error_Count) & " errors detected");
      Ada.Text_IO.Put_Line ("Ending Value_At test on Character data.");
      Ada.Text_IO.New_Line;


   -- Test UNDERFLOW exception using Char_Stack
      Ada.Text_IO.Put_Line ("Starting UNDERFLOW exception test on Character data.");
      begin
         Characters.Clear; -- Nothing in the stack
         Ch := Characters.Value_At;
         Ada.Text_IO.Put_Line ("UNDERFLOW not detected! An error!");
      exception
         when Char_Stack.UNDERFLOW =>
            Ada.Text_IO.Put_Line ("Expected UNDERFLOW detected.");
         when others =>
            Ada.Text_IO.Put_Line ("Unexpected exception testing UNDERFLOW.");
      end;
      Ada.Text_IO.New_Line;

   -- Test STACK_CONSTRAINT_ERROR exceptions using Int_Stack
      Ada.Text_IO.Put_Line ("Starting STACK_CONSTRAINT_ERROR exception test on Integer data.");
      begin
         Ints.Clear; -- Nothing in the stack
         Ints.Push (5);
         Int := Ints.Value_At (2);
         Ada.Text_IO.Put_Line ("STACK_CONSTRAINT_ERROR not detected! An error!");
      exception
         when Int_Stack.STACK_CONSTRAINT_ERROR =>
            Ada.Text_IO.Put_Line ("Expected STACK_CONSTRAINT_ERROR detected.");
         when others =>
            Ada.Text_IO.Put_Line ("Unexpected exception testing STACK_CONSTRAINT_ERROR.");
      end;
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("****** End of Testing ******");
   end Stack_Test;