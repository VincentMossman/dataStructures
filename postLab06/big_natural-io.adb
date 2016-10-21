with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
package body Big_Natural.IO is

   ----------------------------------------------------------------------------
   -- Local operations
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function To_Character (Value : in Digit) return Character is
   -- Convert a digit into the corresponding character
   -- Preconditions  : None
   -- Postconditions : Returns the character corresponding to Digit
   begin
      return Character'Val (Character'Pos ('0') + Value);
   end To_Character;

   ----------------------------------------------------------------------------
   function To_Digit (Char : in Character) return Digit is
      -- Converts a digit character ('0' .. '9') into a numeric digit
      -- Preconditions  : None
      -- Postconditions : The numeric equivalent of Char is returned
      -- Exceptions     : CONSTRAINT_ERROR is raised when Char not in '0'..'9'
   begin
      return Character'Pos (Char) - Character'Pos ('0');
   end To_Digit;

   ----------------------------------------------------------------------------
   procedure Put (File      : in Ada.Text_IO.File_Type;
                  Separator : in Separator_Type) is
   -- Put a seperator into the file
   -- Preconditions  : None
   -- Postconditions : The character corresponding to Separator is written to File.
   --                  Nothing is written to File if Separator is None
   begin
      if Separator = Comma then
         Ada.Text_IO.Put (File => File, Item => ',');
      elsif Separator = Underscore then
         Ada.Text_IO.Put (File => File, Item => '_');
      end if;
   end Put;


   ----------------------------------------------------------------------------
   -- Bodies of operations in specification
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Put (File      : in Ada.Text_IO.File_Type;
                  Item      : in Big_Natural;
                  Width     : in Natural;
                  Separator : in Separator_Type := Comma) is

      Leading : Integer;  -- The number of leading blanks needed for the Width

   begin
      -- Calculate the number of leading blanks (may be negative, zero, or positive)
      if Separator = None then
         Leading := Width - Item.Reference.all'Length;
      else -- The separators take up additional room
         Leading := Width - (Item.Reference.all'Length +
                            (Item.Reference.all'Length - 1) / 3);
      end if;

      -- Display leading blanks (if any)
      Ada.Text_IO.Put (Item => (1 .. Leading => ' '),
                       File => File);

      -- Display the Big_Natural number
      -- Each iteration, display one digit
      for Index in reverse Item.Reference'Range loop
         Ada.Text_IO.Put (Item => To_Character (Item.Reference.all (Index)),
                          File => File);
         if Index rem 3 = 0  and  Index /= 0 then
            Put (File, Separator);
         end if;
      end loop;
   end Put;

   ----------------------------------------------------------------------------
   procedure Put (Item      : in Big_Natural;
                  Width     : in Natural;
                  Separator : in Separator_Type := Comma) is
   begin
      Put (File      => Ada.Text_IO.Standard_Output,
           Item      => Item,
           Width     => Width,
           Separator => Separator);
   end Put;


   ----------------------------------------------------------------------------
   procedure Get (File : in  Ada.Text_IO.File_Type;
                  Item : out Big_Natural) is

      Char   : Character;         -- One input "digit"
      Number : Unbounded_String;  -- Number as a string
      Index  : Natural;           -- Index for Digit_Array

      White_Set : constant Ada.Strings.Maps.Character_Set := To_Set (' ');

   begin
      Finalize (Item);  -- Recycle memory used by Item
      Number := Null_Unbounded_String;

      -- Skip over any leading blanks and leading line terminators
      -- Each iteration, process one input character
      Skip_White_Space :
      loop
         Ada.Text_IO.Get (File => File,
                          Item => Char);
         exit Skip_White_Space when not Is_In (Element => Char,
                                               Set     => White_Set);
      end loop Skip_White_Space;

      Number := Number & Char;  -- Got the first digit

      -- Get the rest of the digits in the number
      -- Each iteration, get one digit
      Get_Digits :
      loop
         exit Get_Digits when Ada.Text_IO.End_Of_Line (File);
         Ada.Text_IO.Get (Char);
         exit Get_Digits when Is_In (Element => Char,
                                     Set     => White_Set);
         Number := Number & Char;
      end loop Get_Digits;

      if Length (Number) > 1 then
         -- Trim off any leading zeros
         Index := 1;
         loop
            exit when Index = Length (Number) or Element (Number, Index) /= '0';
            Index := Index + 1;
         end loop;
         Number := Unbounded_Slice (Source => Number,
                                    Low    => Index,
                                    High   => Length (Number));
      end if;

      -- Set up array of digits
      Item.Reference := new Digit_Array (0 .. Length (Number) - 1);
      Index := 0;

      -- Convert the string to an array of digits
      -- Each iteration, convert one digit
      for String_Index in reverse 1 .. Length (Number) loop
         Item.Reference.all (Index) := To_Digit (Element (Source => Number,
                                                          Index  => String_Index));
         Index := Index + 1;
      end loop;

   end Get;

   ----------------------------------------------------------------------------
   procedure Get (Item : out Big_Natural) is
   begin
      Get (File => Ada.Text_IO.Standard_Input,
           Item => Item);
   end Get;

end Big_Natural.IO;
