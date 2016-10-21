with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Heart;
with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;
procedure Cardioverter is


-- This program simulates the action of a Cardioverter-Defibrillator, a device that
--  is implanted in a patient to check and maintain correct heart rhythms.
--
-- Written by   Vincent T. Mossman
-- Date         March 27, 2014
--
-- Input
--       From Keyboard    Patient's last name
--                        Lower bound of healthy zero crossings
--                        Upper bound of healthy zero crossings
--                        Number of readings to use in activity calculations
--
--       From Patient     Heart Muscle Activity
--
-- Output
--       Monitor initiation and termination messages.
--       Number of zero crossings for each heart monitoring cycle.
--       Statistics (mean and standard deviation of heart activity readings)
--       Status of heart for each heart monitoring cycle
--       Sensor failure messages.
--
-- Assumptions
--       1.  Patient's last name contains no more than 60 characters
--       2.  User enters a valid patient name
--       3.  User enters valid lower and upper bound values
--       4.  User enters valid number of readings for calculations
--       5.  Patient's insurance does not run out in the middle of a
--            set of heart readings

   -- Types and Subtypes
   subtype Reading_Range        is Integer range 12 .. 48;
   subtype Cardiac_Arrest_Range is Integer range 10 .. 30;
   subtype Fibrillation_Range   is Integer range 70 .. 95;
   type    Reading_Set          is array (Positive range <>) of Heart.Reading;
   subtype Name_String          is String (1 .. 60);
   type    Set_Info_Rec         is
      record
         ZCC          : Natural;   -- Zero crossings
         Average      : Float;     -- Average of heart readings
         Standard_Dev : Float;     -- Standard deviation of heart readings
         Sense_Error  : Boolean;   -- Sense error
      end record;

------------------------------------------------------------------------------------

   function Average_Of (Values : in Reading_Set) return Float is

   -- Purpose        : Find the average of a group of numbers
   -- Preconditions  : Values'Length > 0
   -- Postconditions : The average of Values is returned

      -- Variables
      Sum : Float;  -- The sum of the Values

   begin   -- Average_Of

      -- Initialize Variables
      Sum := 0.0;

      -- Sum the numbers in Values
      -- Each iteration, one value is added to the sum
      for Index in Values'Range loop
         Sum := Sum + Float (Values (Index));
      end loop;

      -- Calculate and return the average
      return Sum / Float (Values'Length);

   end Average_Of;

------------------------------------------------------------------------------------

   function Standard_Deviation_Of (Values : in Reading_Set) return Float is

   -- Purpose        : Find the standard deviation of a group of numbers
   -- Preconditions  : Values'Length > 1
   -- Postconditions : The standard deviation of Values is returned

      -- Variables
      Average        : Float; -- The average of Values
      Sum_Of_Squares : Float; -- Sum of the squared variances of Values

   begin

      -- Initialize Variables
      Sum_Of_Squares := 0.0;
      Average := Average_Of (Values);

      -- Sum the squares of the variances in Values
      -- Each iteration, add one square of a variance to the sum
      for Index in Values'Range loop
         Sum_Of_Squares := Sum_Of_Squares + (Float (Values (Index)) - Average) ** 2;
      end loop;

      -- Calculate and return the standard deviation
      return Sqrt (Sum_Of_Squares / Float (Values'Length - 1));

   end Standard_Deviation_Of;

------------------------------------------------------------------------------------

   function ZCC_Of (Initial_Reading : in Heart.Reading;
                    Set             : in Reading_Set) return Natural is

   -- Purpose        : Determine number of zero crossings in a set of readings
   -- Preconditions  : None
   -- Postconditions : Returns number of zero crossings in Set

      -- Use type clause for binary operators
      use type Heart.Reading;

      -- Variables
      Last_Reading : Heart.Reading; -- Last reading
      Result       : Natural;       -- ZCC count

   begin   -- ZCC_Of

      -- Initialize variables
      Result := 0;
      Last_Reading := Initial_Reading;

      -- Loop determines number of zero crossings in a set
      -- Each iteration, determines if one reading crossed zero
      for Index in Set'Range loop
         if Last_Reading < 0 and Set (Index) >= 0 then
            Result := Result + 1;
         elsif Last_Reading >= 0 and Set (Index) < 0 then
            Result := Result + 1;
         end if;
         Last_Reading := Set (Index);
      end loop;

      return Result;

   end ZCC_Of;

------------------------------------------------------------------------------------

   procedure Process_Set_Info (ZCC_Min  : in Positive;
                               ZCC_Max  : in Positive;
                               Set_Info : in Set_Info_Rec) is

   -- Purpose        : Process set information
   -- Preconditions  : None
   -- Postconditions : Shock is applied, if necessary
   --                  Set information is displayed on screen

   begin   -- Process_Set_Info

      -- Determine if this set contains a sensor error
      if not Set_Info.Sense_Error then

         -- Display set information
         Ada.Integer_Text_IO.Put (Item  => Set_Info.ZCC,
                                  Width => 5);
         Ada.Float_Text_IO.Put (Item => Set_Info.Average,
                                Fore => 5,
                                Aft  => 1,
                                Exp  => 0);
         Ada.Float_Text_IO.Put (Item => Set_Info.Standard_Dev,
                                Fore => 4,
                                Aft  => 2,
                                Exp  => 0);

         -- Determine if shock application is necessary
         if Set_Info.ZCC < ZCC_Min then
            Ada.Text_IO.Put ("   Cardiac Arrest!");
            Heart.Shock;
         elsif Set_Info.ZCC > ZCC_Max then
            Ada.Text_IO.Put ("   Fibrillation!");
            Heart.Shock;
         else
            Ada.Text_IO.Put ("   Normal Heart Rhythm");
         end if;

         Ada.Text_IO.New_Line;

      else
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Sensor Failure!");
         Ada.Text_IO.New_Line;
      end if;

   end Process_Set_Info;

------------------------------------------------------------------------------------

   procedure Get_Set_Info (Set_Size        : in     Reading_Range;
                           Initial_Reading : in out Heart.Reading;
                           Set_Info        :    out Set_Info_Rec) is

   -- Purpose        : Retrieve set information
   -- Preconditions  : None
   -- Postconditions : Returns necessary set information

      -- Variables
      Heart_Readings : Reading_Set (1 .. Set_Size); -- Set of heart readings

   begin   -- Get_Set_Info

      -- Loop fills Heart_Readings with heart readings
      -- Each iteration, fills with one reading
      for Index in Heart_Readings'Range loop
         Heart_Readings (Index) := Heart.Sense;
      end loop;

      -- Calculate readings
      Set_Info.ZCC := ZCC_Of (Initial_Reading => Initial_Reading,
                              Set             => Heart_Readings);
      Set_Info.Average := Average_Of (Heart_Readings);
      Set_Info.Standard_Dev := Standard_Deviation_Of (Heart_Readings);

      -- Prepare for next reading process
      Set_Info.Sense_Error := False;
      Initial_Reading := Heart_Readings (Heart_Readings'Last);

   exception
      when Heart.SENSE_ERROR =>
         Set_Info.Sense_Error := True;
         Initial_Reading := 0;

   end Get_Set_Info;

------------------------------------------------------------------------------------

   function To_Num (Base       : in Reading_Range;
                    Percentage : in Positive) return Positive is

   -- Purpose        : Return a percentage of a whole number (as a whole number)
   -- Preconditions  : None
   -- Postconditions : Percentage of Whole is returned

   begin   -- To_Num

      return Positive (Float (Base) * (Float (Percentage) / 100.0));

   end To_Num;

------------------------------------------------------------------------------------

   -- Patient data source
   Patient_Name : Name_String; -- The patient's last name
   Length       : Positive;    -- Number of characters in Name

   -- Reading and Set Variables
   Lower_Bound     : Cardiac_Arrest_Range; -- Lower bound of healthy zero crossings (percentage)
   Upper_Bound     : Fibrillation_Range;   -- Upper bound of healthy zero crossings (percentage)
   ZCC_Min         : Positive;      -- Lower bound of healthy zero crossings (whole number)
   ZCC_Max         : Positive;      -- Upper bound of healthy zero crossings (whole number)
   Num_Readings    : Reading_Range; -- Number of readings used in activity calculations
   Initial_Reading : Heart.Reading; -- Initial activity reading
   Set_Info        : Set_Info_Rec;  -- Record containing set information

begin   -- Cardioverter

   -- Prepare the patient
   Ada.Text_IO.Put_Line ("Enter the patient's name");
   Ada.Text_IO.Get_Line (Item => Patient_Name, Last => Length);
   Heart.Initialize (Patient_Name (1 .. Length));

   -- Set up the parameters of the Cardioverter-Defibrillator
   Ada.Text_IO.Put_Line ("Enter the lower bound (percent)");
   Ada.Integer_Text_IO.Get (Lower_Bound);
   Ada.Text_IO.Put_Line ("Enter the upper bound (percent)");
   Ada.Integer_Text_IO.Get (Upper_Bound);
   Ada.Text_IO.Put_Line ("Enter number of readings used in a calculation");
   Ada.Integer_Text_IO.Get (Num_Readings);
   Initial_Reading := 0;

   -- Convert percentages into a number of readings (rounded to nearest reading)
   ZCC_Min := To_Num (Base       => Num_Readings,
                      Percentage => Lower_Bound);
   ZCC_Max := To_Num (Base       => Num_Readings,
                      Percentage => Upper_Bound);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Beginning patient heart monitoring");
   Ada.Text_IO.New_Line;

   -- Loop gets and processes heart readings
   -- Each iteration, gets and processes one set of readings
   Monitor_Cycle :
   loop
      exit Monitor_Cycle when not Heart.Insured;

      -- Retrieve information for one set of readings
      Get_Set_Info (Set_Size        => Num_Readings,
                    Initial_Reading => Initial_Reading,
                    Set_Info        => Set_Info);

      -- Process results of set reading
      Process_Set_Info (ZCC_Min  => ZCC_Min,
                        ZCC_Max  => ZCC_Max,
                        Set_Info => Set_Info);

   end loop Monitor_Cycle;

   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("Insurance terminated!");
   Ada.Text_IO.Put_Line ("Patient's Cardioverter-Defibrillator removed.");

end Cardioverter;