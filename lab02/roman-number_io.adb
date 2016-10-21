with Ada.IO_Exceptions;
package body Roman.Number_IO is

   -- Package for Input and Output of Roman Numerals
   package Numeral_IO is new Ada.Text_IO.Enumeration_IO (Numeral);


   -----------------------------------------------------------------------------
   -- Local subprograms
   -----------------------------------------------------------------------------

   function Valid_Order (Roman_Number : in Number) return Boolean is
   -- Returns True if the order of numerals in Roman_Number is valid
   --
   -- Preconditions  : None
   --
   -- Postconditions : True is returned if numerals are followed only by
   --                  numerals of the same or lower value.

      Result : Boolean;   -- The answer

   begin
      Result := True;          -- Innocent till proven guilty.
      -- Check the order of all numerals
      -- Each iteration, compare the value of one numeral to the next
      for Index in 2 .. Roman_Number.Length loop
         if Roman_Number.Numerals (Index - 1) < Roman_Number.Numerals (Index) then
            Result := False;   -- Proven guilty
         end if;
      end loop;
      return Result;
   end Valid_Order;

   -----------------------------------------------------------------------------
   function Valid_Counts (Roman_Number : in Number) return Boolean is
   -- Returns True if the quantity of each numeral is valid
   --
   -- Preconditions  : None
   --
   -- Postconditions : True is returned if number of each numeral is acceptable
   --                  according to the definition of an additive Roman number

      -- Need one counter for each Roman numeral
      type Count_Array is array (Numeral) of Natural;
      Counts : Count_Array;

   begin -- Valid
      Counts := (Numeral => 0);  -- Initialize counts to zero

      -- Count up all the numerals
      -- Each iteration, count one numeral
      for Index in 1 .. Roman_Number.Length loop
         Counts (Roman_Number.Numerals (Index)) :=
                 Counts (Roman_Number.Numerals (Index)) + 1;
      end loop;

      return Counts (I) <= 4 and Counts (V) <= 1 and
             Counts (X) <= 4 and Counts (L) <= 1 and
             Counts (C) <= 4 and Counts (D) <= 1;
   end Valid_Counts;


   -----------------------------------------------------------------------------
   function Valid (Roman_Number : in Number) return Boolean is
   -- Returns True if Roman_Number is a valid additive Roman number
   --
   -- Preconditions  : None
   --
   -- Postconditions : True is returned if Roman_Number is a valid additive
   --                  Roman number

   begin -- Valid
      return Valid_Order  (Roman_Number)  and then
             Valid_Counts (Roman_Number);
   end Valid;

   -----------------------------------------------------------------------------
   -- Operations for the class Number
   -----------------------------------------------------------------------------

   procedure Put (Item          : in Number;
                  Width         : in Positive;
                  Justification : in Justification_Type) is
   begin
      -- Simply call the File version passing Standard Output as the file
      -- (Call the function Standard_Output to locate this file)
      Put (File          => Ada.Text_IO.Standard_Output,
           Item          => Item,
           Width         => Width,
           Justification => Justification);
   end Put;


   -----------------------------------------------------------------------------
   procedure Put (File          : in Ada.Text_IO.File_Type;
                  Item          : in Number;
                  Width         : in Positive;
                  Justification : in Justification_Type) is
   begin
      if Justification = Right then
         -- Add necessary leading blanks
         for Blank_Count in 1 .. Width - Item.Length loop
            Ada.Text_IO.Put (File => File, Item => ' ');
         end loop;
      end if;

      -- Display all the numerals in the Roman number
      for Index in 1 .. Item.Length loop
         Numeral_IO.Put (File  => File,
                         Item  => Item.Numerals (Index),
                         Width => 1);
      end loop;

      if Justification = Left then
         -- Add necessary trailing blanks
         for Blank_Count in 1 .. Width - Item.Length loop
            Ada.Text_IO.Put (File => File, Item => ' ');
         end loop;
      end if;
   end Put;

   -----------------------------------------------------------------------------
   procedure Get (Item : out Number) is
   begin
      -- Simply call the File version passing Standard Input as the file
      -- (Call the function Standard_Input to locate this file)
      Get (File  => Ada.Text_IO.Standard_Input,
           Item  => Item);
   end Get;

   -----------------------------------------------------------------------------
   procedure Get (File : in  Ada.Text_IO.File_Type;
                  Item : out Number) is

      Char : Character;  -- One character from the input file
      Done : Boolean;    -- True if end of Roman number marked by end of line

   begin -- Get
      Blank_Loop : -- Skip over any leading blanks and leading line terminators
      loop         -- Each iteration, get one character
         Ada.Text_IO.Get (File => File, Item => Char);
         exit Blank_Loop when Char /= ' ';
      end loop Blank_Loop;

      -- Assertion:  At this point Char contains the first Roman
      --             numeral in the Number

      -- Put the first numeral into the Roman number
      Item.Length := 1;
      Item.Numerals (1) := Numeral'Value ((1 => Char)); -- Raises CONSTRAINT_ERROR
      --                                                -- if Char not a valid numeral

      Numeral_Loop :  -- Get all the numerals in the number
      loop            -- Each iteraion, get one numeral for the number
         Ada.Text_IO.Look_Ahead (File        => File,
                                 Item        => Char,
                                 End_Of_Line => Done);
         exit Numeral_Loop when Done or else Char = ' ';
         Ada.Text_IO.Get (File => File, Item => Char);
         Item.Length := Item.Length + 1;
         Item.Numerals (Item.Length) := Numeral'Value ((1 => Char));
      end loop Numeral_Loop;

      if not Valid (Item) then
         raise Ada.IO_Exceptions.Data_Error;  -- Raise the exeception for invalid number
      end if;
   end Get;
end Roman.Number_IO;