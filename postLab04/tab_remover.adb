with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Tab_Remover is

-- Written by   Vincent T. Mossman
-- Date         February 2014
--
-- Input
--   From the keyboard
--      None
--
--   From the command line
--      1. Name of input file
--      2. Name of output file
--      3. Number of blanks to substitute for each tab character
--
-- Output
--   To the screen
--      1. Error messages
--      2. Number of tab characters found in the input file and replaced by blanks
--      3. Number of lines in the input (and output) file
--
--   To the file (if program arguments are valid)
--      1. Copy of the input file with each tab character replaced by the desired number of blank characters
--
-- Assumptions
--   Program is being run from command line

   -- Constant for default tab length
   Default_Tab_Length : constant := 3;

----------------------------------------------------------------------------

   procedure Process_Line (Input_File  : in out Ada.Text_IO.File_Type;
                           Output_File : in out Ada.Text_IO.File_Type;
                           Tab_Length  : in     Positive;
                           Tabs_Found  :    out Natural) is

   -- Procedure processes one line of Input_File and transfers it to Output_File
   --
   -- Preconditions  : Input_File and Output_File must be valid file types
   --
   -- Postconditions : 1. Tab characters found in Input_File are replaced with
   --                      Tab_Length spaces in Output_File
   --                  2. Tabs_Found is returned as the number of tabs found and replaced

      -- Variable for character I/O
      Current_Character : Character;

   begin

      -- Initialize variables
      Tabs_Found := 0;

      -- Loop replaces all tabs in one line of input file and writes to output file
      -- Each iteration, processes one character
      Replace_Tabs_Line :
      loop
         exit Replace_Tabs_Line when Ada.Text_IO.End_Of_Line (Input_File) or
                                     Ada.Text_IO.End_Of_File (Input_File);

         Ada.Text_IO.Get (File => Input_File,
                          Item => Current_Character);

         if Current_Character = Ada.Characters.Latin_1.HT then
            Ada.Text_IO.Put (File => Output_File,
                             Item => Tab_Length * ' ');

            Tabs_Found := Tabs_Found + 1;

         else

            Ada.Text_IO.Put (File => Output_File,
                             Item => Current_Character);

         end if;

      end loop Replace_Tabs_Line;

   end Process_Line;

----------------------------------------------------------------------------

   procedure Display_Results (Lines_Processed : in Natural;
                              Tabs_Replaced   : in Natural) is

   -- Procedure displays the results of tab removal
   --
   -- Preconditions  : None
   --
   -- Postconditions : File statistics (Lines_Replaced and Tabs_Replaced) are
   --                   displayed neatly

   begin

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("File successfully processed");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Tabs replaced: ");
      Ada.Integer_Text_IO.Put (Item  => Tabs_Replaced,
                               Width => 0);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Lines processed: ");
      Ada.Integer_Text_IO.Put (Item  => Lines_Processed,
                               Width => 0);

   end Display_Results;

----------------------------------------------------------------------------

   procedure Remove_Tabs (Input_File      : in out Ada.Text_IO.File_Type;
                          Output_File     : in out Ada.Text_IO.File_Type;
                          Tab_Length      : in     Positive;
                          Lines_Processed :    out Natural;
                          Tabs_Replaced   :    out Natural) is

   -- Procedure processes Input_File and transfers it to Output_File
   --
   -- Preconditions  : Input_File and Output_File must be valid file types
   --
   -- Postconditions : 1. Tab characters found in Input_File are replaced with
   --                      Tab_Length spaces in Output_File
   --                  2. Tabs_Replaced is returned as the number of tabs found and replaced
   --                  3. Lines_Processed is returned as the number of lines processed

      -- Variable for tabs found and replaced per line
      Tabs_Found : Natural;

   begin

      Lines_Processed := 0;
      Tabs_Found      := 0;
      Tabs_Replaced   := 0;

      -- Loop replaces tab characters with specified number of spaces
      -- Each iteration, replaces tabs with spaces in one line
      Replace_Tabs :
      loop
         exit Replace_Tabs when Ada.Text_IO.End_Of_File (Input_File);

         Process_Line (Input_File    => Input_File,
                       Output_File   => Output_File,
                       Tab_Length    => Tab_Length,
                       Tabs_Found    => Tabs_Found);

         Lines_Processed := Lines_Processed + 1;
         Tabs_Replaced   := Tabs_Replaced + Tabs_Found;

         if not Ada.Text_IO.End_Of_File (Input_File) then

            Ada.Text_IO.Skip_Line (Input_File);
            Ada.Text_IO.New_Line (Output_File);

         end if;

      end loop Replace_Tabs;

   end Remove_Tabs;

----------------------------------------------------------------------------

   -- Variables for files and statistics
   Input_File      : Ada.Text_IO.File_Type;
   Output_File     : Ada.Text_IO.File_Type;
   Tabs_Replaced   : Natural;
   Lines_Processed : Natural;

begin

   -- Open Input_File
   Ada.Text_IO.Open (File => Input_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => Ada.Command_Line.Argument (1));

   -- Create Output_File
   Ada.Text_IO.Create (File => Output_File,
                       Mode => Ada.Text_IO.Out_File,
                       Name => Ada.Command_Line.Argument (2));

   -- Determine Tab width and remove tabs
   if Ada.Command_Line.Argument_Count = 2 then

      Remove_Tabs (Input_File      => Input_File,
                   Output_File     => Output_File,
                   Tab_Length      => Default_Tab_Length,
                   Lines_Processed => Lines_Processed,
                   Tabs_Replaced   => Tabs_Replaced);

   elsif Ada.Command_Line.Argument_Count = 3 then

      Remove_Tabs (Input_File      => Input_File,
                   Output_File     => Output_File,
                   Tab_Length      => Integer'Value (Ada.Command_Line.Argument (3)),
                   Lines_Processed => Lines_Processed,
                   Tabs_Replaced   => Tabs_Replaced);

   end if;

   -- Display Results
   Display_Results (Lines_Processed => Lines_Processed,
                    Tabs_Replaced   => Tabs_Replaced);

   exception
      when Ada.Text_IO.Use_Error =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("ERROR: Input and Output files are the same");
         Ada.Text_IO.Put_Line ("       File not processed");
      when Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("ERROR: File '" & Ada.Command_Line.Argument (1) & "' does not exist");
         Ada.Text_IO.Put_Line ("       File not processed");
      when CONSTRAINT_ERROR =>
         Ada.Text_IO.Delete (Output_File);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("ERROR: Invalid tab width");
         Ada.Text_IO.Put_Line ("       File not processed");

end Tab_Remover;