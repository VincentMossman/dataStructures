package body Roman is

   -----------------------------------------------------------------------------
   -- Local subprograms
   -----------------------------------------------------------------------------

   function Value_Of (Roman_Numeral : in Numeral) return Positive is
   -- Converts a Roman numeral to an Arabic number
   --
   -- Preconditions  : None
   --
   -- Postconditions : The Arabic equivalent of Numeral is returned

   begin
      case Roman_Numeral is
         when I =>
            return 1;
         when V =>
            return 5;
         when X =>
            return 10;
         when L =>
            return 50;
         when C =>
            return 100;
         when D =>
            return 500;
         when M =>
            return 1000;
      end case;
   end Value_Of;


   -----------------------------------------------------------------------------
   -- Operations for the class Number
   -----------------------------------------------------------------------------

   function "+" (Left : in Number; Right : in Number) return Number is
   begin
      return To_Roman (Value_Of (Left) + Value_Of (Right));
   end "+";

   -----------------------------------------------------------------------------
   function "-" (Left : in Number; Right : in Number) return Number is
   begin
      return To_Roman (Value_Of (Left) - Value_Of (Right));
   end "-";

   -----------------------------------------------------------------------------
   function "*" (Left : in Number; Right : in Number) return Number is
   begin
      return To_Roman (Value_Of (Left) * Value_Of (Right));
   end "*";

   -----------------------------------------------------------------------------
   function "/" (Left : in Number; Right : in Number) return Number is
   begin
      return To_Roman (Value_Of (Left) / Value_Of (Right));
   end "/";

   -----------------------------------------------------------------------------
   function "+" (Left : in Number;  Right : in Integer) return Number is
   begin
      return To_Roman (Value_Of (Left) + Right);
   end "+";

   -----------------------------------------------------------------------------
   function "+" (Left : in Integer; Right : in Number)  return Number is
   begin
      return To_Roman (Left + Value_Of (Right));
   end "+";


   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------

   function "=" (Left : in Number; Right : in Number)  return Boolean is
   begin
      return Left.Numerals (1 .. Left.Length) = Right.Numerals (1 .. Right.Length);
   end "=";

   -----------------------------------------------------------------------------

   function "<" (Left : in Number; Right : in Number)  return Boolean is
   begin
      return Left.Numerals (1 .. Left.Length) < Right.Numerals (1 .. Right.Length);
   end "<";

   -----------------------------------------------------------------------------

   function "<=" (Left : in Number; Right : in Number)  return Boolean is
   begin
      return Left.Numerals (1 .. Left.Length) <= Right.Numerals (1 .. Right.Length);
   end "<=";

   -----------------------------------------------------------------------------

   function ">" (Left : in Number; Right : in Number)  return Boolean is
   begin
      return Left.Numerals (1 .. Left.Length) > Right.Numerals (1 .. Right.Length);
   end ">";

   -----------------------------------------------------------------------------

   function ">=" (Left : in Number; Right : in Number)  return Boolean is
   begin
      return Left.Numerals (1 .. Left.Length) >= Right.Numerals (1 .. Right.Length);
   end ">=";

   -----------------------------------------------------------------------------
   -----------------------------------------------------------------------------
   function Value_Of (Roman_Number : in Number) return Positive is
      Result : Natural;  -- The answer
   begin
      Result := 0;                    -- Initialize sum

      -- Add the values of all of the numerals
      -- Each iteration, add one numeral to the sum
      for Index in 1 .. Roman_Number.Length loop
         Result := Result + Value_Of (Roman_Number.Numerals (Index));
      end loop;
      return Result;

   end Value_Of;

   -----------------------------------------------------------------------------
   function To_Roman (Value : in Positive) return Number is

   -- Note: CONSTRAINT_ERROR is raised
   --       1. If the actual parameter for Value is zero or negative.
   --       2  We attempt to add a numeral to Result beyond the Max_Numerals
   --          numeral because we go out of bounds on the Numeral_Array.

      Result       : Number;   -- The answer
      Remaining    : Natural;  -- Amount of Value remaining to be converted
      Num_Numerals : Natural;  -- The number of times one numeral occurs
      Index        : Natural;  -- Index for array of numerals

   begin -- To_Roman
      Index     := 0;     -- No numerals yet in Result
      Remaining := Value; -- Still have entire value to convert

      -- Add all the seven different numerals (from highest value to lowest)
      -- Each iteration, one of the seven numerals is added
      for Current_Numeral in reverse Numeral loop

         -- Calculate how many of the current numerals we need
         Num_Numerals := Remaining / Value_Of (Current_Numeral);
         -- Calculate how much remains after processing the current numeral
         Remaining := Remaining rem Value_Of (Current_Numeral);

         -- Add as many of the current numeral as required
         -- Each iteration, one copy of the current numeral is added
         for Count in 1 .. Num_Numerals loop
            Index := Index + 1;
            Result.Numerals (Index) := Current_Numeral;
         end loop;
      end loop;
      Result.Length := Index;  -- Set length of Result
      return Result;
   end To_Roman;

end Roman;