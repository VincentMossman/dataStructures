with Ada.Numerics.Elementary_Functions;  -- For square root function
use  Ada.Numerics.Elementary_Functions;  -- OK to use "use package" here

type Array_Type is array (Integer range <>) of Integer;

-------------------------------------------------------------------------------
function Average_Of (Values : in Array_Type) return Float is
-- Find the average of a group of numbers
--
-- Preconditions  : Values'Length > 0
--
-- Postconditions : The average of Values is returned

   Sum : Float;  -- The sum of the Values
begin
   Sum := 0.0;
   -- Sum the numbers in Values
   -- Each iteration, one value is added to the sum
   for Index in Values'Range loop
      Sum := Sum + Float (Values (Index));
   end loop;
   -- Calculate and return the average
   return Sum / Float (Values'Length);
end Average_Of;


-------------------------------------------------------------------------------
function Standard_Deviation_Of (Values : in Array_Type) return Float is
-- Find the standard deviation of a group of numbers
--
-- Preconditions  : Values'Length > 1
--
-- Postconditions : The standard deviation of Values is returned

  Average        : Float;  -- The average of Values
  Sum_Of_Squares : Float;  -- Sum of the squared variances of Values
begin
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

