package Roman is

-- A data abstraction package to implement a class of Additive Roman Numbers

   -- A valid additive Roman number
   --    Contains at least one Roman numeral
   --    No numeral is followed by a numeral of greater value
   --    Contains no more than four each I's, X's, and C's
   --    Contains no more than one each V, L and D

   Max_Numerals : constant := 25;  -- Maximum size of a Roman Number

   -- For the seven Roman numerals
   type Numeral is (I, V, X, L, C, D, M);

   -- An array of Roman Numerals
   subtype Numeral_Index is Integer range 1 .. Max_Numerals;
   type    Numeral_Array is array (Numeral_Index) of Numeral;

   -- A Roman Number is a varying length collection of Roman Numerals
   type Number is
      record
         Numerals : Numeral_Array; -- The numerals making up the Roman number
         Length   : Numeral_Index; -- The number of numerals in the Roman number
      end record;

   -----------------------------------------------------------------------------
   -- Arithmetic operations on Roman Numbers
   --
   -- Preconditions  : Each number is a valid additive Roman number
   --
   -- Postconditions : The result is that of the arithmetic operator
   --
   -- Exceptions     : CONSTRAINT_ERROR is raised if the result is
   --                     zero or negative.
   --                  CONSTRAINT_ERROR is raised if the result requires
   --                     more than Max_Numerals numerals

   function "+" (Left : in Number; Right : in Number) return Number;
   function "-" (Left : in Number; Right : in Number) return Number;
   function "*" (Left : in Number; Right : in Number) return Number;
   function "/" (Left : in Number; Right : in Number) return Number;

   function "+" (Left : in Number;  Right : in Integer) return Number;
   function "+" (Left : in Integer; Right : in Number)  return Number;

   -----------------------------------------------------------------------------
   -- Relational operations on Roman Numbers
   --
   -- Preconditions  : Each number is a valid additive Roman number
   --
   -- Postconditions : The result is that of the relational operator

   function "=" (Left : in Number; Right : in Number) return Boolean;
   function "<" (Left : in Number; Right : in Number) return Boolean;
   function "<=" (Left : in Number; Right : in Number) return Boolean;
   function ">" (Left : in Number; Right : in Number) return Boolean;
   function ">=" (Left : in Number; Right : in Number) return Boolean;

   -----------------------------------------------------------------------------
   -- Conversion operations

   function Value_Of (Roman_Number : in Number) return Positive;
   -- Convert a Roman Number to an Arabic number
   --
   -- Preconditions  : Roman_Number is a valid additive Roman number
   --
   -- Postconditions : Arabic value of Roman_Number is returned

   function To_Roman (Value : in Positive) return Number;
   -- Convert an Arabic number to a Roman Number
   --
   -- Preconditions  : None
   --
   -- Postconditions : Roman Number for Value is returned
   --
   -- Exceptions     : CONSTRAINT_ERROR is raised if the result requires
   --                     more than Max_Numerals numerals

end Roman;