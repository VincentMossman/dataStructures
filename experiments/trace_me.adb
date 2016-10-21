with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Trace_Me is

   type Natural_Array is array (Positive range <>) of Natural;
   subtype Uppercase is Character range 'A' .. 'Z';
   subtype Name_String is String (5 .. 11);
   subtype Move_Array is Natural_Array (4 .. 12);

   ----

   procedure Shift_Char (Char : in out Character;
                         Amt : in Natural) is

   begin
      for Count in 1 .. Amt loop
         Char := Character'Succ (Char);
      end loop;
   end Shift_Char;

   ----

   procedure Shift_String (Word : in out String;
                           By : in Natural_Array) is

   begin
      Ada.Integer_Text_IO.Put (Item => By'First, Width => 3);
      Ada.Integer_Text_IO.Put (Item => By'Last, Width => 3);
      Ada.Integer_Text_IO.Put (Item => By'Length, Width => 3);
      Ada.Text_IO.New_Line;

      for Index in Word'Range loop
         Shift_Char (Char => Word (Index),
                     Amt => By (Index));
      end loop;
   end Shift_String;

   ----

   procedure Swap (Word : in out String) is
      Index : Positive;
   begin
      Index := Word'First;
      loop
         exit when Index > Word'Last or else Word (Index) in Uppercase;
         Index := Index + 1;
      end loop;

      if Index <= Word'Last then
         Word := Word (Index .. Word'Last) & Word (Word'First .. Index - 1);
      end if;
   end Swap;

   ----

      Name : Name_String;
      Move : Move_Array;
   begin
      Name := "qeaLfjb";
      Ada.Text_IO.Put_Line (Name);
      Swap (Name);
      Ada.Text_IO.Put_Line (Name);
      Move := (5, 1, 3, 2, 2, 1, 0, 3, 8);
      Shift_String (Word => Name,
                    By => Move (5 .. 11));
      Ada.Text_IO.Put_Line (Name);
   end Trace_Me;