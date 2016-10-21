with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;

procedure Air_Miles is

-- This program allows analyzation of airline usage by the employees of
--  Honest Ed's car shop.
--
-- Written by   Vincent T. Mossman
--
-- Input
--   From the keyboard
--      1. Name of the text file containing employee flight data
--
--   From the text file
--      1. Names of employees
--      2. Flight information
--
-- Output
--   To screen
--      1. Neat table with employee names along with their individual
--          flight statistics
--      2. Flight and distance flown totals
--      3. Maximum flights made along with employee name
--      4. Maximum miles flown along with employee name
--
-- Assumptions
--   1. User enteres a valid file name containing no more than 80 characters
--   2. Each employee name is on a line by itself, each set of flight information
--       is on a line by itself, and the word DONE is on a line by itself
--   3. No employee name contains more than 20 characters
--   4. Employee names and the sentinal value DONE are spelled correctly
--   5. There are no characters between the last data item and the end of file marker
--   6. There are no ties for either most flights or most miles flown

   -- Types, subtypes, and packages for data I/O
   type    Input_Range    is (January, February, March, April, May, June, July,
                              August, September, October, November, December, Done);
   subtype Month_Range    is Input_Range range January .. December;
   package Month_IO       is new Ada.Text_IO.Enumeration_IO (Enum => Input_Range);
   type    Mileage_Array  is array (Month_Range range <>) of Integer;   -- Unconstrained mileage array
   subtype Valid_Input    is Integer range 100 .. 12_500;   -- Valid mileage range

   -- Types and constants for table output
   type Tab_Array   is array (Positive range <>) of Ada.Text_IO.Positive_Count;
   type Width_Array is array (Positive range <>) of Natural;
   Tab   : constant Tab_Array   := (1, 22, 31, 41, 54, 67);  -- Six column
   Width : constant Width_Array := (20, 5, 6, 7, 7, 5);      -- table

   ----------------------------------------------------------------------------

   procedure Put_Dashed_Line (Length : in Natural) is

   -- Preconditions  : None
   --
   -- Postconditions : A line of Length dashes is displayed

   begin

      -- Display a line of dashes
      -- Each iteration, display one dash
      for Dash_Count in 1 .. Length loop
         Ada.Text_IO.Put ('-');
      end loop;
      Ada.Text_IO.New_Line;

   end Put_Dashed_Line;

   ----------------------------------------------------------------------------

   procedure Display_Headings is

   -- Preconditions  : None
   --
   -- Postconditions : Table headings are displayed

   begin

      Ada.Text_IO.New_Line;

      -- First line
      Ada.Text_IO.Set_Col (To => Tab (1));
      Ada.Text_IO.Put ("Name");
      Ada.Text_IO.Set_Col (To => Tab (2));
      Ada.Text_IO.Put ("Flights");
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Text_IO.Put ("Distance");
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("Average 1st");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("Average 2nd");
      Ada.Text_IO.Set_Col (To => Tab (6));
      Ada.Text_IO.Put ("Bad Data");
      Ada.Text_IO.New_Line;

      -- Second line
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Text_IO.Put ("Flown");
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("Half Year");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("Half Year");
      Ada.Text_IO.Set_Col (To => Tab (6));
      Ada.Text_IO.Put ("Entries");
      Ada.Text_IO.New_Line;

      -- Third line
      Ada.Text_IO.Set_Col (To => Tab (4));
      Ada.Text_IO.Put ("miles/month");
      Ada.Text_IO.Set_Col (To => Tab (5));
      Ada.Text_IO.Put ("miles/month");
      Ada.Text_IO.New_Line;

      Put_Dashed_Line (Length => Natural (Tab (Tab'Last)) + 8);
      Ada.Text_IO.New_Line;

   end Display_Headings;

   ---------------------------------------------------------------------------

   procedure Display_Totals (Flights : in Natural;
                             Miles   : in Natural) is

   -- Preconditions  : None
   --
   -- Postconditions : The given values are displayed

   begin

      Ada.Text_IO.New_Line;
      Put_Dashed_Line (Length => Natural (Tab (Tab'Last)) + 8);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Set_Col (To => Tab (1));
      Ada.Text_IO.Put ("Totals");
      Ada.Text_IO.Set_Col (To => Tab (2));
      Ada.Integer_Text_IO.Put (Item  => Flights,
                               Width => Width (2));
      Ada.Text_IO.Set_Col (To => Tab (3));
      Ada.Integer_Text_IO.Put (Item  => Miles,
                               Width => Width (3));
      Ada.Text_IO.New_Line (2);

   end Display_Totals;

   ---------------------------------------------------------------------------

   procedure Display_Extremes (Name_Max_Flights : in String;
                               Max_Flights      : in Natural;
                               Name_Max_Miles   : in String;
                               Max_Miles        : in Natural) is

   -- Preconditions  : None
   --
   -- Postconditions : The given values are displayed

   begin

      Ada.Text_IO.Put ("The most flights (");
      Ada.Integer_Text_IO.Put (Item => Max_Flights, Width => 1);
      Ada.Text_IO.Put (") were flown by ");
      Ada.Text_IO.Put_Line (Name_Max_Flights);
      Ada.Text_IO.Put ("The most miles (");
      Ada.Integer_Text_IO.Put (Item => Max_Miles, Width => 1);
      Ada.Text_IO.Put (") were flown by ");
      Ada.Text_IO.Put_Line (Name_Max_Miles);

   end Display_Extremes;

   -------------------------------------------------------------------------------

   function Sum_Mileage (List : in Mileage_Array) return Natural is

   -- Preconditions  : None
   --
   -- Postconditions : Sum of entire year of flight mileage is returned

      -- Variables
      Total_Miles : Natural;   -- Total Mileage in entire array

   begin

      -- Initialize Total_Miles
      Total_Miles := 0;

      -- Loop determines sum of entire year of flight mileage
      -- Each iteration, increments sum by value of one month
      Sum_Loop :
      for Index in List'Range loop
         Total_Miles := Total_Miles + List (Index);
      end loop Sum_Loop;

      return Total_Miles;

   end Sum_Mileage;

   -------------------------------------------------------------------------------

   function Average_Half_Mileage (List : in Mileage_Array) return Natural is

   -- Preconditions  : Must be array containing half of a year (6 months)
   --
   -- Postconditions : Returns average mileage for half of a year

   begin

      return Natural (Float (Sum_Mileage (List)) / 6.0);

   end Average_Half_Mileage;

   -------------------------------------------------------------------------------

   procedure Display_Employee_Stats (Flights  : in Natural;
                                     List     : in Mileage_Array;
                                     Bad_Data : in Natural) is

   -- Preconditions  : None
   --
   -- Postconditions : Employee statistics are displayed neatly

   begin

      Ada.Text_IO.Set_Col (Tab (2));
      Ada.Integer_Text_IO.Put (Item  => Flights,
                               Width => 5);
      Ada.Text_IO.Set_Col (Tab (3));
      Ada.Integer_Text_IO.Put (Item  => Sum_Mileage (List),
                               Width => 6);
      Ada.Text_IO.Set_Col (Tab (4));
      Ada.Integer_Text_IO.Put (Item  => Average_Half_Mileage (List (January .. June)),
                               Width => 7);
      Ada.Text_IO.Set_Col (Tab (5));
      Ada.Integer_Text_IO.Put (Item  => Average_Half_Mileage (List (July .. December)),
                               Width => 7);
      Ada.Text_IO.Set_Col (Tab (6));
      Ada.Integer_Text_IO.Put (Item  => Bad_Data,
                               Width => 5);

   end Display_Employee_Stats;

   -------------------------------------------------------------------------------

   procedure Process_One_Employee (File     : in out Ada.Text_IO.File_Type;
                                   Flights  :    out Natural;
                                   List     :    out Mileage_Array;
                                   Bad_Data :    out Natural) is
   -- Preconditions  : None
   --
   -- Postconditions : Employee flight statistics are returned

      -- Variables
      Input_Type : Input_Range;   -- Input type
      Miles      : Valid_Input;   -- Flight mileage

   begin

      -- Initialize employee statistics variables
      Flights  := 0;
      List     := (others => 0);
      Bad_Data := 0;

      -- Loop retrieves flight data for one employee
      -- Each iteration, retrieves one month of data
      Retrieve_Info :
      loop

         -- Data validation block checks for valid data in file
         Validate_Input :
         begin

            Month_IO.Get (File => File,
                          Item => Input_Type);

            exit Retrieve_Info when Input_Type = Done;

            Ada.Integer_Text_IO.Get (File => File,
                                     Item => Miles);
            List (Input_Type) := List (Input_Type) + Miles;

            -- Increment number of flights
            Flights := Flights + 1;

            exception
               when Ada.IO_Exceptions.Data_Error =>
                  Bad_Data := Bad_Data + 1;
                  Ada.Text_IO.Skip_Line (File);
               when CONSTRAINT_ERROR =>
                  Bad_Data := Bad_Data + 1;
                  Ada.Text_IO.Skip_Line (File);

         end Validate_Input;

      end loop Retrieve_Info;

   end Process_One_Employee;

----------------------------------------------------------------------------

   subtype File_String   is String (1 .. 80);
   subtype Name_String   is String (1 .. 20);
   subtype Employee_Mileage_Array is Mileage_Array (Month_Range);

   -- Variables for the data file
   Flight_File   : Ada.Text_IO.File_Type;  -- File with employee flight information
   File_Name     : File_String;            -- Name of the file
   Length        : Natural;                -- Length of the file name

   -- Variables for employee statistics
   Name          : Name_String;            -- Name of the employee
   Name_Length   : Natural;                -- Length of employee name
   Flights       : Natural;                -- Number of flights by employee
   Mileage       : Employee_Mileage_Array; -- Array of mileage by month
   Bad_Data      : Natural;                -- Number of bad data entries per employee

   -- Variables for totals and extreme values
   Total_Flights      : Natural;           -- Total flights by all employees
   Total_Mileage      : Natural;           -- Total miles flown by all employees
   Max_Flights        : Natural;           -- Most flights by any employee
   Max_Flights_Name   : Name_String;       -- Name of employee with most flights
   Max_Flights_Length : Natural;           -- Length of name of employee with most flights
   Max_Miles          : Natural;           -- Most miles by any employee
   Max_Miles_Name     : Name_String;       -- Name of employee with most miles
   Max_Miles_Length   : Natural;           -- Length of name of employee with most miles

begin  -- Air_Miles

   -- Open the data file
   Ada.Text_IO.Put_Line ("Enter the name of the file with flight information.");
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => Length);
   Ada.Text_IO.Open (File => Flight_File,
                     Mode => Ada.Text_IO.In_File,
                     Name => File_Name (1 .. Length));

   -- Display Headings
   Display_Headings;

   -- Initialize totals and extreme values variables
   Total_Flights := 0;
   Total_Mileage := 0;
   Max_Flights   := 0;
   Max_Miles     := 0;

   -- Loop displays statistics for all employees
   -- Each iteration, displays one employees' statistics
   Process_Statistics :
   loop

      exit Process_Statistics when Ada.Text_IO.End_Of_File (Flight_File);

      -- Display Employee Name
      Ada.Text_IO.Get_Line (File => Flight_File,
                            Item => Name,
                            Last => Name_Length);
      Ada.Text_IO.Put (Name (1 .. Name_Length));

      -- Get Employee Info
      Process_One_Employee (File     => Flight_File,
                            Flights  => Flights,
                            List     => Mileage,
                            Bad_Data => Bad_Data);

      -- Increment Flight and Mileage totals
      Total_Flights := Total_Flights + Flights;
      Total_Mileage := Total_Mileage + Sum_Mileage (Mileage);

      -- Determine if new flight max has been achieved
      if Flights > Max_Flights then
         Max_Flights        := Flights;
         Max_Flights_Name   := Name;
         Max_Flights_Length := Name_Length;
      end if;

      -- Determine if new mileage max has been achieved
      if Sum_Mileage (Mileage) > Max_Miles then
         Max_Miles          := Sum_Mileage (Mileage);
         Max_Miles_Name     := Name;
         Max_Miles_Length   := Name_Length;
      end if;

      -- Display Employee statistics
      Display_Employee_Stats (Flights  => Flights,
                              List     => Mileage,
                              Bad_Data => Bad_Data);

      -- Skip line if end of file has not been reached
      if not Ada.Text_IO.End_Of_File (Flight_File) then
         Ada.Text_IO.Skip_Line (Flight_File);
      end if;

      Ada.Text_IO.New_Line;

   end loop Process_Statistics;

   -- Display flight and mileage totals
   Display_Totals (Flights => Total_Flights,
                   Miles   => Total_Mileage);

   -- Display Extremes if flights have been made
   if Total_Flights > 0 then
      Display_Extremes (Name_Max_Flights => Max_Flights_Name (1 .. Max_Flights_Length),
                        Max_Flights      => Max_Flights,
                        Name_Max_Miles   => Max_Miles_Name (1 .. Max_Miles_Length),
                        Max_Miles        => Max_Miles);
   end if;

   Ada.Text_IO.Close (Flight_File);

end Air_Miles;