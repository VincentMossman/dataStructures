with Ada.Unchecked_Deallocation;
package body Big_Natural is

   procedure Recycle is new Ada.Unchecked_Deallocation(Object => Digit_Array,
                                                       Name   => Number_Ptr);

   ----------------------------------------------------------------------------
   -- Local (helper) operations
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Remove_Leading_Zeros (Item : in out Number_Ptr) is
   -- Removes any leading zeros from Item
   -- Preconditions  : Item in not null
   -- Postconditions : Item designates any array of digits with no leading zeros

   begin
      Item := null;  -- A memory leak
   end Remove_Leading_Zeros;

   ----------------------------------------------------------------------------
   function "*" (Left : in Big_Natural; Right : in Digit) return Big_Natural is
   -- Returns the product of a big natural and a single digit.  Written
   -- as a helper function for the multiplication of two big natural numbers
   -- Preconditions  : Left has at least one digit
   -- Postconditions : The product of Left and Right is returned

      Result  : Big_Natural;

   begin
      Result := To_Big_Natural (0);
      return Result;
   end "*";

   ----------------------------------------------------------------------------
   -- Bodies of operations in specification
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function To_Big_Natural (Item : in Natural) return Big_Natural is

      Result     : Big_Natural;
      Num_Digits : Natural;
      Item_Copy  : Natural;

   begin

      Item_Copy  := Item;
      Num_Digits := 0;

      loop
         Num_Digits := Num_Digits + 1;
         exit when Item_Copy < 10;
         Item_Copy := Item_Copy / 10;
      end loop;

      Result.Reference := new Digit_Array (0 .. Num_Digits - 1);

      Item_Copy := Item;

      for Index in Result.Reference.all'Range loop
         Result.Reference.all (Index) := Item_Copy rem 10;
         Item_Copy := Item_Copy / 10;
      end loop;

      return Result;
   end To_Big_Natural;


   ----------------------------------------------------------------------------
   function To_Natural (Item : in Big_Natural) return Natural is
      Result : Natural;
   begin

      -- Initialize variable
      Result := 0;

      for Index in Item.Reference.all'Range loop
         Result := Result + (Item.Reference.all (Index) * 10 ** Index);
      end loop;


      return Result;
   end To_Natural;


   ----------------------------------------------------------------------------
   function "+" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is

      subtype Sum_Range   is Integer range 0 .. 19;
      subtype Carry_Range is Integer range 0 .. 1;

      Sum   : Sum_Range;   -- Result of adding two digits and possible carry
      Carry : Carry_Range; -- Carry from adding two digits

   begin
      return To_Big_Natural (0);
   end "+";

   --------------------------
   function "+" (Left : in Big_Natural; Right : in Natural) return Big_Natural is
   begin
      return Left + To_Big_Natural (Right);
   end "+";

   --------------------------
   function "+" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) + Right;
   end "+";


   ----------------------------------------------------------------------------
   function "-" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is

      subtype Borrow_Range is Integer range 0 .. 1;

      Difference : Digit;         -- Result of subtracting two digits
      Borrow     : Borrow_Range;  -- Borrow from next column

   begin
      return To_Big_Natural (0);
   end "-";

   --------------------------
   function "-" (Left : in Big_Natural; Right : in Natural) return Big_Natural is
   begin
      return Left - To_Big_Natural (Right);
   end "-";

   --------------------------
   function "-" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) - Right;
   end "-";


   ----------------------------------------------------------------------------
   function "*" (Left : in Big_Natural; Right : in Big_Natural) return Big_Natural is

      Result : Big_Natural;

   begin
      Result := To_Big_Natural (0);

      return Result;
   end "*";

   --------------------------
   function "*" (Left : in Natural; Right : in Big_Natural) return Big_Natural is
   begin
      return To_Big_Natural (Left) * Right;
   end "*";

   ----------------------------------------------------------------------------
   function "/" (Dividend : in Big_Natural; Divisor : in Big_Natural) return Big_Natural is
      -- The following two pragmas disable warnings for not using the parameters.
      -- If you implement this extra credit function, delete these two lines
      pragma Warnings (Off, Dividend);
      pragma Warnings (Off, Divisor);

      Quotient : Big_Natural;
   begin
      Quotient := To_Big_Natural (0);
      return Quotient;
   end "/";

   --------------------------
   function "/" (Dividend : in Big_Natural; Divisor : in Natural) return Big_Natural is
   begin
      return Dividend / To_Big_Natural (Divisor);
   end "/";

   ----------------------------------------------------------------------------
   function Shift_Left (Number : in Big_Natural;
                        By     : in Natural := 1) return Big_Natural is
   -- Do not implement this functin with multiplication.
   -- Add By zeros to the least significant end of Number.

      Result : Big_Natural;
   begin

   if Number.Reference.all (Number.Reference.all'Last) = 0 then
      return Number;
   else
      Result.Reference := new Digit_Array'((0 .. By - 1 => 0) & Number.Reference.all);
      return Result;
   end if;

   end Shift_Left;

   ----------------------------------------------------------------------------
   function Shift_Right (Number : in Big_Natural;
                         By     : in Natural := 1) return Big_Natural is

      Result : Big_Natural;

   begin

      Result.Reference := new Digit_Array (0 .. Number.Reference.all'Last - By);
      Result.Reference.all := Number.Reference.all (By .. Number.Reference.all'Last);

      return Result;
   end Shift_Right;

   ----------------------------------------------------------------------------
   function "=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return Left.Reference.all = Right.Reference.all;
   end "=";

   function "=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left = To_Big_Natural (Right);
   end "=";

   function "=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) = Right;
   end "=";


   ----------------------------------------------------------------------------
   function "<" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is

      Index : Natural;

   begin
      if Left.Reference.all'Length < Right.Reference.all'Length then
         return True;
      elsif Left.Reference.all'Length > Right.Reference.all'Length then
         return False;
      else

         Index := Left.Reference.all'Last;

         loop
            exit when Index < 1 or else Left.Reference.all (Index) /= Right.Reference.all (Index);
            Index := Index - 1;
         end loop;

         return Left.Reference.all (Index) < Right.Reference.all (Index);

      end if;

   end "<";

   function "<" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left < To_Big_Natural (Right);
   end "<";

   function "<" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) < Right;
   end "<";


   ----------------------------------------------------------------------------
   function "<=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return Left = Right or else Left < Right;
   end "<=";

   function "<=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left <= To_Big_Natural (Right);
   end "<=";

   function "<=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) <= Right;
   end "<=";


   ----------------------------------------------------------------------------
   function ">" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return not (Left <= Right);
   end ">";

   function ">" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left > To_Big_Natural (Right);
   end ">";

   function ">" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) > Right;
   end ">";

   ----------------------------------------------------------------------------
   function ">=" (Left : in Big_Natural; Right : in Big_Natural) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   function ">=" (Left : in Big_Natural; Right : in Natural) return Boolean is
   begin
      return Left >= To_Big_Natural (Right);
   end ">=";

   function ">=" (Left : in Natural;  Right : in Big_Natural) return Boolean is
   begin
      return To_Big_Natural (Left) >= Right;
   end ">=";



   ----------------------------------------------------------------------------
   -- Bodies that override the two operations of the parent class (Controlled)
   ----------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   procedure Finalize (Object : in out Big_Natural) is
   -- This procedure is automatically called when an instance of a Big_Natural
   -- is no longer neeed.  It provides "garbage collection" for Big_Naturals
   begin
      Recycle (Object.Reference);
   end Finalize;

   ---------------------------------------------------------------------------
   procedure Adjust (Object : in out Big_Natural) is
   -- This procedure is automatically called after an assignment of a value to
   -- Object as in "Object := Other_Object;"  At this point Object and Other_Object
   -- are aliases.  This procedure will make Object a copy (clone) of Other_Object
   -- instead of an alias.
   begin
      Object.Reference := new Digit_Array'(Object.Reference.all);
   end Adjust;

end Big_Natural;