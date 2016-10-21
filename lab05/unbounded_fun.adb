with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;
procedure Unbounded_Fun is

-- A program for practicing with Unbounded Length Strings


   -----------------------------------------------------------------------
   procedure Search_And_Replace (Source  : in out Unbounded_String;
                                 Pattern : in     Unbounded_String;
                                 By      : in     Unbounded_String) is
   -- Replace ALL the occurences of Pattern in Source with By
   -- Search is not case sensitive

      Location : Natural;  -- Location of Pattern in Source

   begin

      Location := 1;

      -- Replace all occurrences of Pattern with By
      -- Each iteration, make one replacement
      loop
         -- Find the location of Pattern in Source
         Location := Index (Source  => Source,
                            Pattern => To_Lower (To_String (Pattern)),
                            From    => Location,
                            Mapping => Lower_Case_Map);

         -- We are done when we can't find Pattern in Source
         exit when Location = 0;

         -- Replace the found Pattern with By
         Replace_Slice (Source => Source,
                        Low    => Location,
                        High   => Location + Length (Pattern) - 1,
                        By     => To_String (By));

         Location := Location + Length (By);

      end loop;
   end Search_And_Replace;

   ----------------------------------------------------------------------------
   Line     : Unbounded_String;
   Pattern  : Unbounded_String;
   New_Text : Unbounded_String;

begin

   -- Process many test cases
   -- Each iteration, process one test case
   loop
      Ada.Text_IO.Put_Line ("-------------------------------------------------");
      Ada.Text_IO.Put_Line ("Enter a string (null string to terminate program)");
      Ada.Text_IO.Unbounded_IO.Get_Line (Line);

      exit when Length (Line) = 0;

      Ada.Text_IO.New_Line (2);
      Ada.Text_IO.Put_Line ("Enter a pattern to replace");
      Ada.Text_IO.Unbounded_IO.Get_Line (Pattern);
      Ada.Text_IO.New_Line (2);
      Ada.Text_IO.Put_Line ("Enter the replacement text");
      Ada.Text_IO.Unbounded_IO.Get_Line (New_Text);
      Ada.Text_IO.New_Line (2);

      -- Make the replacements
      Search_And_Replace (Source  => Line,
                          Pattern => Pattern,
                          By      => New_Text);

      Ada.Text_IO.Put_Line ("The revised line is");
      Ada.Text_IO.Unbounded_IO.Put_Line (Line);
      Ada.Text_IO.New_Line (2);
   end loop;
end Unbounded_Fun;