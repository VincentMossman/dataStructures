with Discrete_Set;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

procedure Essay_Analysis is

-- This program analyses the characters used in an essay
--
-- Started   by John McCormick
-- Completed by Vincent T. Mossman

-- Input
--
--   From Keyboard
--      Name of the text file containing the essay
--
--   From File
--      The essay
--
-- Output
--   For each line of the essay
--       The line number
--       The set of vowels used in the line
--       The set of consonants used in the line
--       The set of punctuation marks used in the line
--       The set of digits used in the line
--   For the entire essay
--       The set of letters, digits, and punctuation marks not used
--       The set of characters used that are not letters, digits, or
--           punctuation marks.
--
-- Assumptions
--   1.  The name of the file containing the essay has no more than 80 characters.
--   2.  The user enters the file name correctly.
--   3.  No line in the essay contains more than 99 characters.


   Tab : constant Ada.Text_IO.Positive_Count := 15;

   -- Package instantiation for Character Sets
   package Character_Set is new Discrete_Set (Element_Type => Character);
   use Character_Set;

   -- Set Constants
   Vowel_Set       : constant Set_Type := (Empty_Set + "aeiouy");
   Consonant_Set   : constant Set_Type := (Empty_Set + "bcdfghjklmnpqrstvwxyz");
   Punctuation_Set : constant Set_Type := (Empty_Set + ".,;:?!");
   Digit_Set       : constant Set_Type := (Empty_Set + "0123456789");

   -- Subtypes
   subtype File_Name_String is String (1..80);
   subtype Line_String      is String (1..100);

   ----------------------------------------------------------------------------

   procedure Put (Item : in Set_Type) is

   --  Display all of the characters in the given set
   --
   --  Preconditions  : none
   --
   --  Postconditions : All of the characters in the set Item are displayed
   --                   with no spaces or line terminators between them.

   begin

      -- Loop displays all characters in set Item
      -- Each iteration, displays one character in set Item
      for Index in Character loop
         if Is_Member (Element => Index,
                       Set     => Item) then
            Ada.Text_IO.Put (Index);
         end if;
      end loop;

   end Put;

-------------------------------------------------------------------------------

   -- The file containing the essay
   Essay     : Ada.Text_IO.File_Type;  -- File for analysis
   Name      : File_Name_String;       -- Name of file
   Length    : Natural;                -- Used for both file name and input line
   Line_Set  : Set_Type;               -- Set for storing line analysis
   Total_Set : Set_Type;               -- Set for storing total essay analysis
   Line      : Line_String;            -- One line of input

begin

   -- Prepare the file containing the essay
   Ada.Text_IO.Put_Line ("Enter the name of the file with the essay.");
   Ada.Text_IO.Get_Line (Item => Name, Last => Length);
   Ada.Text_IO.Open (File => Essay,
                     Name => Name (1 .. Length),
                     Mode => Ada.Text_IO.In_File);
   Ada.Text_IO.New_Line;

   -- Initialize loop variables
   Total_Set := Empty_Set;

   -- Process the essay
   -- Each iteration, process one line of the essay
   Input_Loop :
   loop
      exit Input_Loop when Ada.Text_IO.End_Of_File (Essay);

      -- Display the line number (obtained directly from reading marker location)
      Ada.Text_IO.Put ("Line ");
      Ada.Integer_Text_IO.Put (Item  => Integer (Ada.Text_IO.Line (Essay)),
                               Width => 1);
      Ada.Text_IO.New_Line;

      -- Get a line and convert all uppercase letters to lowercase
      Ada.Text_IO.Get_Line (File => Essay,
                            Item => Line,
                            Last => Length);
      Line (1 .. Length) := Ada.Characters.Handling.To_Lower (Line (1 .. Length));

      -- Construct a set of characters from Line
      Line_Set := Empty_Set + Element_Array (Line (1 .. Length));
      Total_Set := Total_Set + Line_Set;

      -- Display all the sets for the current line
      Ada.Text_IO.Put ("Vowels");
      Ada.Text_IO.Set_Col (Tab);
      Put (Line_Set * Vowel_Set);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Consonants");
      Ada.Text_IO.Set_Col (Tab);
      Put (Line_Set * Consonant_Set);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Punctuation");
      Ada.Text_IO.Set_Col (Tab);
      Put (Line_Set * Punctuation_Set);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Digits");
      Ada.Text_IO.Set_Col (Tab);
      Put (Line_Set * Digit_Set);
      Ada.Text_IO.New_Line (2);
   end loop Input_Loop;

   -- Display "totals"

   Ada.Text_IO.Put_Line ("Letters, digits, and punctuation marks not used in essay");
   Put ((Universal_Set - Total_Set) * (Vowel_Set + Consonant_Set + Punctuation_Set + Digit_Set));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("Characters in essay that are not letters, " &
                         "digits, or punctuation marks");
   Put (Total_Set - (Vowel_Set + Consonant_Set + Punctuation_Set + Digit_Set));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Close (Essay);
   Ada.Text_IO.Put_Line ("All Done!");

end Essay_Analysis;