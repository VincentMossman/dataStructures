with Ada.Text_IO;
with Unbounded_Queue;
with Ada.Strings.Bounded;
with Ada.Exceptions;

-- Test Value_At and Size in the package Queue

procedure Lab09 is

   package Strings      is new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 20);
   package String_Queue is new Unbounded_Queue (Element_Type => Strings.Bounded_String);

   type String_Array is array (positive range <>) of Strings.Bounded_String;

   Test_Data : constant String_Array :=
                        (1  => Strings.To_Bounded_String ("Head"),
                         2  => Strings.To_Bounded_String ("Second"),
                         3  => Strings.To_Bounded_String ("Third"),
                         4  => Strings.To_Bounded_String ("Fourth"),
                         5  => Strings.To_Bounded_String ("Fifth"),
                         6  => Strings.To_Bounded_String ("Sixth"),
                         7  => Strings.To_Bounded_String ("Seventh"),
                         8  => Strings.To_Bounded_String ("Eighth"),
                         9  => Strings.To_Bounded_String ("Ninth"),
                         10 => Strings.To_Bounded_String ("Tenth"),
                         11 => Strings.To_Bounded_String ("Tail"));

   ----------------------------------------------------------------------------
   procedure Put_Line (Item : in Strings.Bounded_String) is
   begin
      Ada.Text_IO.Put_Line (Item => Strings.To_String (Source => Item));
   end Put_Line;

-------------------------------------------------------------------------------
   A_Queue : String_Queue.Queue_Type;     -- A queue to manipulate
   Item    : Strings.Bounded_String;
   Success : Boolean;

begin
   String_Queue.Clear (Queue => A_Queue);

   Ada.Text_IO.Put_Line ("Starting to build a queue and test size.");
   if String_Queue.Size (Queue => A_Queue) /= 0 then
      Ada.Text_IO.Put_Line ("Function Size has failed for empty queue");
   end if;
   Success := True;
   for Index in Test_Data'Range loop
      A_Queue.Enqueue (Item  => Test_Data (Index));
      if String_Queue.Size (Queue => A_Queue) /= Index then
         Ada.Text_IO.Put_Line ("Function Size has failed");
         Success := False;
      end if;
   end loop;
   if Success then
      Ada.Text_IO.Put_Line ("Size test was successful!");
   end if;
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Press enter to continue on to remaining tests.");
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Testing Value_At, front to back.");
   for Position in 1 .. String_Queue.Size (Queue => A_Queue) loop
      Item := String_Queue.Value_At (Queue    => A_Queue,
                                     Position => Position);
      Put_Line (Item);
   end loop;
   Ada.Text_IO.Put_Line ("Done testing Value_At, front to back.");
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Testing Value_At, back to front.");
   for Position in reverse 1 .. String_Queue.Size (Queue => A_Queue) loop
      Item := String_Queue.Value_At (Queue    => A_Queue,
                                     Position => Position);
      Put_Line (Item);
   end loop;
   Ada.Text_IO.Put_Line ("Done testing Value_At, back to front.");
   Ada.Text_IO.New_Line;


   Ada.Text_IO.Put_Line ("Starting UNDERFLOW test.");
   String_Queue.Clear (A_Queue);
   begin
      Item := String_Queue.Value_At (Queue    => A_Queue,
                                     Position => 1);
      Ada.Text_IO.Put_Line ("UNDERFLOW test failed.");
   exception
      when String_Queue.QUEUE_CONSTRAINT_ERROR =>
         Ada.Text_IO.Put_Line ("Unexpected QUEUE_CONSTRAINT_ERROR occurred.");
      when String_Queue.OVERFLOW =>
         Ada.Text_IO.Put_Line ("Unexpected OVERFLOW occurred.");
      when String_Queue.UNDERFLOW =>
         Ada.Text_IO.Put_Line ("The expected UNDERFLOW occurred.");
      when others =>
         Ada.Text_IO.Put_Line ("Unexpected unknown exception occurred.");
   end;
   Ada.Text_IO.Put_Line ("Done with UNDERFLOW test.");
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Starting QUEUE_CONSTRAINT_ERROR test.");


   -- Insert code to test QUEUE_CONSTRAINT_ERROR
   String_Queue.Clear (A_Queue);
   begin
      for Index in Test_Data'Range loop
         A_Queue.Enqueue (Test_Data (Index));
      end loop;
      Item := A_Queue.Value_At (Test_Data'Last + 1);
      Ada.Text_IO.Put_Line ("QUEUE_CONSTRAINT_ERROR test failed.");
   exception
      when String_Queue.QUEUE_CONSTRAINT_ERROR =>
         Ada.Text_IO.Put_Line ("The expected QUEUE_CONSTRAINT_ERROR occurred.");
      when String_Queue.OVERFLOW =>
         Ada.Text_IO.Put_Line ("Unexpected OVERFLOW occurred.");
      when String_Queue.UNDERFLOW =>
         Ada.Text_IO.Put_Line ("Unexpected UNDERFLOW occurred.");
      when others =>
         Ada.Text_IO.Put_Line ("Unexpected unknown exception occurred.");
   end;



   Ada.Text_IO.Put_Line ("Done with QUEUE_CONSTRAINT_ERROR test.");
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Done with all testing.");
   Ada.Text_IO.New_Line (2);


   -- Insert code to test others handler
   for Index in 1 .. 50 loop
      Ada.Text_IO.Put_Line (Strings.To_String (Test_Data (Index)));
   end loop;


exception  -- Handle exceptions which were not intentionally generated.
   when String_Queue.OVERFLOW =>
      Ada.Text_IO.Put_Line ("Unexpected OVERFLOW occurred at unknown point.");
   when String_Queue.UNDERFLOW =>
      Ada.Text_IO.Put_Line ("Unexpected UNDERFLOW occurred at unknown point.");
   when The_Error : others =>
      Ada.Text_IO.Put_Line ("Unexpected exception " &
                            Ada.Exceptions.Exception_Name (The_Error));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

end Lab09;