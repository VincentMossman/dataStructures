with Ada.Finalization;
package Big_Natural is

-- This package implements a natural number class with an unbounded number of digits

   type Big_Natural is private;  -- All Big_Natural objects are initially undefined

   ----------------------------------------------------------------------------
   function To_Big_Natural (Item : in Natural) return Big_Natural;
   -- Convert a Natural number into a Big_Natural number
   -- Preconditions  : None
   -- Postconditions : Returns the Big_Number equivlent of Item

   ----------------------------------------------------------------------------
   function To_Natural (Item : in Big_Natural) return Natural;
   -- Convert a Big_Natural number into a Natural number
   -- Preconditions  : None
   -- Postconditions : Returns the Natural equivlent of Item
   -- Exceptions     : CONSTRAINT_ERROR raised if Item > Natural'Last

   ----------------------------------------------------------------------------
   -- Arithmetic operators
   -- Preconditions  : Left and Right each contain one or more digits
   -- Postconditions : Arithmetic results are returned
   -- Exceptions     : CONSTRAINT_ERROR for subtraction of a larger number from
   --                  a smaller number or division by zero

   function "+" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural;
   function "+" (Left : in Big_Natural; Right : in Natural)     return Big_Natural;
   function "+" (Left : in Natural;     Right : in Big_Natural) return Big_Natural;

   function "-" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural;
   function "-" (Left : in Big_Natural; Right : in Natural)     return Big_Natural;
   function "-" (Left : in Natural;     Right : in Big_Natural) return Big_Natural;

   function "*" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural;
   function "*" (Left : in Natural;     Right : in Big_Natural) return Big_Natural;

   function "/" (Dividend : in Big_Natural; Divisor : in Big_Natural) return Big_Natural;
   function "/" (Dividend : in Big_Natural; Divisor : in Natural) return Big_Natural;

   ----------------------------------------------------------------------------
   function Shift_Left (Number : in Big_Natural;
                        By     : in Natural := 1) return Big_Natural;
   -- Shifts the digits in Number to the left By places
   -- Preconditions  : Number contains one or more digits
   -- Postconditions : Returns Item * (10 ** By)
   ----------------------------------------------------------------------------
   function Shift_Right (Number : in Big_Natural;
                         By     : in Natural := 1) return Big_Natural;
   -- Shifts the digits in Number to the right By places
   -- Preconditions  : Number contains one or more digits
   -- Postconditions : Returns Item / (10 ** By)

   ----------------------------------------------------------------------------
   -- Equality and Relational operators
   -- Preconditions  : Left and Right each contain one or more digits

   function "="  (Left : in Big_Natural; Right : in Big_Natural) return Boolean;
   function "="  (Left : in Big_Natural; Right : in Natural)     return Boolean;
   function "="  (Left : in Natural;     Right : in Big_Natural) return Boolean;

   function "<"  (Left : in Big_Natural; Right : in Big_Natural) return Boolean;
   function "<"  (Left : in Big_Natural; Right : in Natural)     return Boolean;
   function "<"  (Left : in Natural;     Right : in Big_Natural) return Boolean;

   function "<=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean;
   function "<=" (Left : in Big_Natural; Right : in Natural)     return Boolean;
   function "<=" (Left : in Natural;     Right : in Big_Natural) return Boolean;

   function ">"  (Left : in Big_Natural; Right : in Big_Natural) return Boolean;
   function ">"  (Left : in Big_Natural; Right : in Natural)     return Boolean;
   function ">"  (Left : in Natural;     Right : in Big_Natural) return Boolean;

   function ">=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean;
   function ">=" (Left : in Big_Natural; Right : in Natural)     return Boolean;
   function ">=" (Left : in Natural;     Right : in Big_Natural) return Boolean;


private

   -- The Big Natural class is implemented as a pointer to an array of digits.
   -- The index of the least significant digit is always zero.
   -- The index of the most significant digit varies (use 'Last to determine it).
   -- A Big Natural number has no leading zeros

   subtype Digit is Integer range 0 .. 9;
   type Digit_Array is array (Natural range <>) of Digit;
   type Number_Ptr  is access Digit_Array;


   -- We are using inheritance here.  Big_Natural is a subclass of the class
   -- Controlled with one additional field ("member data" in Java-Speak).
   type Big_Natural is new Ada.Finalization.Controlled with
      record
         Reference : Number_Ptr;   -- Pointer to the array of digits
      end record;

   -- We override two methods of the parent class (Controlled).
   -- The bodies of these procedures are in the body of this package.

   -- Redefines ":=" as cloning rather than aliasing
   overriding procedure Adjust (Object : in out Big_Natural);

   -- Deallocates the dynamic memory used by an unbounded natural object
   overriding procedure Finalize (Object : in out Big_Natural);

end Big_Natural;