with Roman;
with Roman.Number_IO;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

procedure Magic_Square is

-- This program constructs a magic square in Roman numbers

   type    Row_Range    is range 1 .. 4;
   type    Column_Range is range 1 .. 4;
   subtype Magic_Range  is Integer range 41 .. 10_000;

   type Square_Array is array (Row_Range, Column_Range) of Natural;
   type Roman_Array  is array (Row_Range, Column_Range) of Roman.Number;

   Initial_Square : constant Square_Array := (1 => ( 7, 10, 13,  0),
                                              2 => (12,  1,  6, 11),
                                              3 => ( 2, 15,  8,  5),
                                              4 => ( 9,  4,  3, 14));

   Magic_Number  : Magic_Range;   -- The sum of any row, column or diagonal
   Arabic_Square : Square_Array;  -- The Magic Square expressed in Arabic numbers
   Roman_Square  : Roman_Array;   -- Roman number equivalent to Arabic_Square

   -- Values used to construct a magic square from a magic number
   Quotient     : Positive;  -- Magic Number / 4
   Remainder    : Natural;   -- Magic Number rem 4

begin
   -- Get the desired number
   Ada.Text_IO.Put_Line ("Enter a Magic_Number (must be greater than 40)");
   Ada.Integer_Text_IO.Get (Magic_Number);

   -- Calculate values for square construction.
   Quotient  := (Magic_Number - 30) / 4;
   Remainder := (Magic_Number - 30) rem 4;

   -- Initialize the Magic Square (copy the Initial Square into the Arabic Square)
   Arabic_Square := Initial_Square;

   -- Add the quotient to all entries of the Magic Square
   for Row in Row_Range loop
      for Column in Column_Range loop
         Arabic_Square (Row, Column) := Arabic_Square (Row, Column) + Quotient;
      end loop;
   end loop;

   -- The next 4 statements add the remainder to the 4 specific locations
   Arabic_Square (1, 3) := Arabic_Square (1, 3) + Remainder;
   Arabic_Square (2, 1) := Arabic_Square (2, 1) + Remainder;
   Arabic_Square (3, 2) := Arabic_Square (3, 2) + Remainder;
   Arabic_Square (4, 4) := Arabic_Square (4, 4) + Remainder;

   -- Translate the Arabic square into a Roman Square
   for Row in Row_Range loop
      for Column in Column_Range loop
         Roman_Square (Row, Column) := Roman.To_Roman (Arabic_Square (Row, Column));
      end loop;
   end loop;



   -- Display the Magic Square using 15 columns per Roman number, right justified
   for Row in Row_Range loop
      for Column in Column_Range loop
         Roman.Number_IO.Put (Item  => Roman_Square (Row, Column),
                              Width => 15,
                              Justification => Roman.Number_IO.Right);
      end loop;
      Ada.Text_IO.New_Line;
   end loop;



end Magic_Square;