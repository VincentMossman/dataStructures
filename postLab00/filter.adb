with Ada.Text_IO;             -- For input and output of strings and characters
with Ada.Integer_Text_IO;     -- For input and output of integers
with Ada.Float_Text_IO;       -- For input and output of floats
with Ada.IO_Exceptions;       -- For input and output exceptions

procedure Filter is

-- This program uses a moving average to filter out anomalies from
-- a set of sensor data stored in a file.
--
-- Written by    John W. McCormick
--
-- Completed by  Vincent T. Mossman
--
--
-- Input
--    From keyboard
--      1. Name of file containing sensor data.
--      2. The half width of the filter (the number of values on either
--         side of a reading to use in calculating the moving average).
--         2 * Half_Width + 1 readings are used in each average calculation.
--
--    From the file
--      1. An unknown number of lines of sensor data.
--
-- Output to screen
--      1. Appropriate prompts for input data.
--      2. Appropriate error messages for invalid keyboard input
--         a)  Data file does not exist.
--         b)  Number of values for running average is out of range.
--      3. Each line of sensor data.
--      4. Each line of filtered sensor data.
--
-- Assumptions
--      1.  The data file name contains no more than 60 characters.
--      2.  All values in the file are valid numbers.
--      3.  No line in the data file contains more than Max_Readings values.
--      4.  Each line contains at least Half_Width valid readings.
--      5.  The last value on every line in the file is
--          immediately followed by a line terminator.
--      6.  The last value in the file is immediately
--          followed by the file terminator.

   -- Values for formatting the output of sensor readings
   Reading_Fore    : constant := 4;
   Reading_Aft     : constant := 1;
   Reading_Exp     : constant := 0;
   Reading_Columns : constant := Reading_Fore + Reading_Aft + 1;

   -- Maximum readings per line of input
   Max_Readings    : constant := 500;


   subtype File_String        is String (1 .. 60);
   subtype Half_Width_Range   is Integer range  0 .. 5;
   subtype Sensor_Value_Range is Float   range -99.999999 .. 99.999999;
   type    Sensor_Value_Array is array (Positive range <>) of Sensor_Value_Range;


   ----------------------------------------------------------------------------
   function Average (Values : in Sensor_Value_Array) return Sensor_Value_Range is
   --
   -- Preconditions  : Values'Length > 0
   --
   -- Postconditions : Returns the average of the numbers in Values
   --

      Sum : Float;  -- Sum of all numbers in Values

   begin
      Sum := 0.0;
      -- Add up all numbers in Values
      -- Each iteration, add one value to the sum
      for Index in Values'First .. Values'Last loop
         Sum := Sum + Values (Index);
      end loop;

      -- Return the average
      return Sum / Float (Values'Length);
   end Average;


   ----------------------------------------------------------------------------
   procedure Filter_Readings (Raw        : in  Sensor_Value_Array;
                              Half_Width : in  Half_Width_Range;
                              Filtered   : out Sensor_Value_Array) is
   --
   -- Filter the Raw sensor readings using a running average
   -- of 2 * Half_Width + 1 readings
   --
   -- Preconditions  : Filtered'Range = Raw'Range
   --                  Raw'Length >= Half_Width
   --
   -- Postconditions : The first Half_Width values of Filtered are zero
   --                  The last Half_Width values of Filtered are zero
   --                  The Ith value of Filtered is the average of the
   --                     2 * Half_Width + 1 Raw readings centered on I

   begin
      -- Calculate the filtered readings from the raw readings.
      for Index in Raw'First + Half_Width  ..  Raw'Last - Half_Width loop
         Filtered (Index) := Average (Raw (Index - Half_Width .. Index + Half_Width));
      end loop;

      -- The following code illustrates two different ways (loop and assignment
      -- statement) to assign the same value to multiple array elements.

      -- Fill the first Half_Width values with zero
      -- Using a loop
      for Index in Filtered'First  ..  Filtered'First + Half_Width - 1  loop
         Filtered (Index) := 0.0;
      end loop;

      -- Fill in the last Half_Width values with zero
      -- This time use an assignment statement with aggregate
      Filtered (Filtered'Last - Half_Width + 1 .. Filtered'Last) := (others => 0.0);

   end Filter_Readings;


   ----------------------------------------------------------------------------
   procedure Prepare_File (Prompt : in     String;
                           File   : in out Ada.Text_IO.File_Type) is
   --
   -- This procedure displays Prompt to ask the user to enter a file name
   -- and opens that file for input.
   --
   -- Preconditions  : none
   --
   -- Postconditions : File is open for input.

      File_Name : File_String;  -- The name of the data file
      Length    : Natural;      -- Number of characters in the data file's name

   begin
      Validation_Loop : -- Open the file for input
      loop              -- Each iteration, attempt to open a file
         Validation_Block :
         begin
            -- Get a file name from the user
            Ada.Text_IO.Put_Line (Prompt);
            Ada.Text_IO.Get_Line (Item => File_Name,
                                  Last => Length);
            -- Try to open the file
            Ada.Text_IO.Open (File => File,
                              Name => File_Name (1 .. Length),
                              Mode => Ada.Text_IO.In_File);
            -- If we made it here, we have an open file and are done
            exit Validation_Loop;

         exception
            when Ada.IO_Exceptions.NAME_ERROR =>    -- File does not exist
               Ada.Text_IO.Put_Line ("Unable to find that file, let's try again");
               Ada.Text_IO.New_Line;
         end Validation_Block;
      end loop Validation_Loop;
   end Prepare_File;


   ----------------------------------------------------------------------------
   procedure Get_Line (File    : in  Ada.Text_IO.File_Type;
                       Values  : out Sensor_Value_Array;
                       Last    : out Natural;
                       Invalid : out Natural) is
   --
   -- Read one line of sensor readings from File
   --
   -- Preconditions  : File is open for input.
   --                  File contains only numbers.
   --                  Current line contains no more than Values'Length numbers.
   --                  A line terminator immediately follows the
   --                     last number on the line.
   --
   -- Postconditions : Values (Values'First .. Last) contain the valid sensor
   --                     readings from the current line of File.
   --                  Last is the index of the last valid sensor reading found
   --                     on the current line.
   --                  Invalid is the number of invalid sensor readings found
   --                     on the current line.
   --                     Valid readings are in Sensor_Value_Range.

   begin
      Last    := Values'First - 1;  -- So far no numbers have
      Invalid := 0;                 --     been read

      Input_Loop : -- Get all the numbers on the line
      loop         -- Each iteration, get one number from File
         exit Input_Loop when Ada.Text_IO.End_Of_Line (File);
         Last := Last + 1;

         Validation_Block : -- To handle out of range numbers
         begin
            Ada.Float_Text_IO.Get (File => File,
                                   Item => Values (Last));
         exception
            when CONSTRAINT_ERROR =>
               Invalid := Invalid + 1;  -- Found an invalid sensor reading
               Last    := Last - 1;     -- Correct the value of Last
         end Validation_Block;

      end loop Input_Loop;
   end Get_Line;


   ----------------------------------------------------------------------------
   procedure Get_Half_Width (Prompt      : in  String;
                             Half_Width  : out Half_Width_Range) is
   -- Preconditions  : none
   --
   -- Postconditions : The filter width entered by the user is returned

   begin
      Validation_Loop : -- Get the filter half width
      loop              -- Each iteration, one value is read
         Validation_Block :
         begin
            -- Get a filter half width from the user
            Ada.Text_IO.Put_Line (Prompt);
            Ada.Integer_Text_IO.Get (Half_Width);
            -- If we made it here, we have a valid filter half width
            exit Validation_Loop;
         exception
            when CONSTRAINT_ERROR =>
               Ada.Text_IO.Put ("Invalid filter width.  It must be between ");
               Ada.Integer_Text_IO.Put (Item  => Half_Width_Range'First,
                                        Width => 1);
               Ada.Text_IO.Put (" and ");
               Ada.Integer_Text_IO.Put (Item  => Half_Width_Range'Last,
                                        Width => 1);
               Ada.Text_IO.Put_Line ("!");
               Ada.Text_IO.New_Line;
            when Ada.IO_Exceptions.DATA_ERROR =>
               Ada.Text_IO.Put_Line ("Invalid filter width.  It must be a number!");
               Ada.Text_IO.Skip_Line;  -- Skip over bad data
               Ada.Text_IO.New_Line;
         end Validation_Block;
      end loop Validation_Loop;

   end Get_Half_Width;


   ----------------------------------------------------------------------------
   procedure Put (Readings : in Sensor_Value_Array) is
   --
   -- Preconditions  : none
   --
   -- Postconditions : All readings in the array are displayed

   begin
      -- Loop displays all values in array Readings
      -- Each iteration, displays one value
      for Index in Readings'Range loop
         Ada.Float_Text_IO.Put (Item => Readings (Index),
                                Fore => Reading_Fore,
                                Aft  => Reading_Aft,
                                Exp  => Reading_Exp);
      end loop;
   end Put;


   ----------------------------------------------------------------------------
   procedure Put_Blanks (Count : in Natural) is
   --
   -- Display Count blanks
   --
   -- Preconditions   : None
   --
   -- Postcondiations : Count blanks are displayed

   begin
      -- Display Count blanks
      -- Each iteration, display one blank
      for Index in 1 .. Count loop
         Ada.Text_IO.Put (' ');
      end loop;
   end Put_Blanks;


-------------------------------------------------------------------------------
   subtype Reading_Array is Sensor_Value_Array (1 .. Max_Readings);

   Sensor_Data_File  : Ada.Text_IO.File_Type;  -- The data file

   Line_Count        : Natural;        -- Number of lines of readings processed
   Filter_Half_Width : Natural;        -- Number of values on either side of a
                                       --    reading used to calculate the average
   Raw_Values        : Reading_Array;  -- One line of valid sensor readings
   Number_Of_Values  : Natural;        -- The number of readings in Raw_Values
   Number_Of_Invalid : Natural;        -- The number of invalid sensor readings
                                       --    on the current input line
   Filtered_Values   : Reading_Array;  -- The filtered sensor readings


begin  -- Filter

   -- Open the data file for input
   Prepare_File (Prompt => "Enter the name of the file containing sensor data.",
                 File   => Sensor_Data_File);
   Ada.Text_IO.New_Line;

   -- Get the filter width
   Get_Half_Width (Prompt      => "Enter the filter half width.",
                   Half_Width  => Filter_Half_Width);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("The filter half width you entered was ");
   Ada.Integer_Text_IO.Put (Item  => Filter_Half_Width,
                            Width => 0);
   Ada.Text_IO.New_Line (4);

   Line_Count := 0;
   Line_Loop : -- Process all the lines of sensor data in the file
   loop        -- Each iteration, one line of sensor data is processed
      exit Line_Loop when Ada.Text_IO.End_Of_File (Sensor_Data_File);

      -- Get all of the sensor readings on the current line
      Line_Count := Line_Count + 1;
      Get_Line (File    => Sensor_Data_File,
                Values  => Raw_Values,
                Last    => Number_Of_Values,
                Invalid => Number_Of_Invalid);

      -- Advance the reading marker to the next line of sensor readings
      if not Ada.Text_IO.End_Of_File (Sensor_Data_File) then
         Ada.Text_IO.Skip_Line (Sensor_Data_File);
      end if;

      -- Describe current line of sensor readings
      Ada.Text_IO.Put ("Line ");
      Ada.Integer_Text_IO.Put (Item => Line_Count, Width => 1);
      Ada.Text_IO.Put (" contains ");
      Ada.Integer_Text_IO.Put (Item => Number_Of_Values, Width => 1);
      Ada.Text_IO.Put (" valid readings and ");
      Ada.Integer_Text_IO.Put (Item => Number_Of_Invalid, Width => 1);
      Ada.Text_IO.Put_Line (" invalid readings.");
      Ada.Text_IO.New_Line;

      -- Display the raw readings
      Put (Readings => Raw_Values (1 .. Number_Of_Values));
      Ada.Text_IO.New_Line;

      -- Filter the readings
      Filter_Readings (Raw        => Raw_Values (1 .. Number_Of_Values),
                       Half_Width => Filter_Half_Width,
                       Filtered   => Filtered_Values (1 .. Number_Of_Values));

      -- Display the filtered readings
      Put_Blanks (Filter_Half_Width * Reading_Columns);
      Put (Filtered_Values (Filter_Half_Width + 1 .. Number_Of_Values - Filter_Half_Width));
      Ada.Text_IO.New_Line (3);

   end loop Line_Loop;

   Ada.Text_IO.Close (Sensor_Data_File);

end Filter;