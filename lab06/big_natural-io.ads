with Ada.Text_IO;
package Big_Natural.IO is

-- Assumptions : Input data is valid (no data validation done here)
--               Input numbers are terminated by a space or line terminator


   type Separator_Type is (None, Comma, Underscore);

   ----------------------------------------------------------------------------
   procedure Put (Item      : in Big_Natural;
                  Width     : in Natural;
                  Separator : in Separator_Type := Comma);
   -- Writes a big natural number to standard output
   -- Preconditions  : None
   -- Postconditions : The Value of Item is displayed right justified in Width spaces
   --                  with a separator every three digits
   
   ----------------------------------------------------------------------------
   procedure Put (File      : in Ada.Text_IO.File_Type;
                  Item      : in Big_Natural;
                  Width     : in Natural;
                  Separator : in Separator_Type := Comma);
   -- Writes a big natural number to a text file
   -- Preconditions  : File is open for output
   -- Postconditions : The Value of Item is written to File right justified in Width spaces
   --                  with a separator every three digits
   

   ----------------------------------------------------------------------------
   procedure Get (Item : out Big_Natural);
   -- Gets a big natural number from standard input
   -- Preconditions  : None
   -- Postconditions : Item contains the number entered on one line

   ----------------------------------------------------------------------------
   procedure Get (File : in  Ada.Text_IO.File_Type;
                  Item : out Big_Natural);
   -- Gets a big natural number from File
   -- Preconditions  : File is open for input
   -- Postconditions : Item contains the number entered on one line
   
end Big_Natural.IO;