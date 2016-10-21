with Ada.Text_IO;
package Roman.Number_IO is

   type Justification_Type is (Left, Right);

   -- A valid additive Roman number 
   --    Contains at least one Roman numeral
   --    No numeral is followed by a numeral of greater value
   --    Contains no more than four each I's, X's, and C's
   --    Contains no more than one each V, L and D


   -----------------------------------------------------------------------------
   procedure Put (Item          : in Number;
                  Width         : in Positive;
                  Justification : in Justification_Type);
   -- Writes a Roman number to the standard output file
   --
   -- Preconditions   : none
   --
   -- Postconditions : Roman_Number is displayed in a field of Width characters
   --                  justified to the Left or to the Right.  If Width is not
   --                  sufficient to display the number, additional space will
   --                  be used (like the Width in Integer output).

   -----------------------------------------------------------------------------
   procedure Put (File          : in  Ada.Text_IO.File_Type;
                  Item          : in  Number;
                  Width         : in  Positive;
                  Justification : in  Justification_Type);
   -- Writes a Roman number to File
   --
   -- Preconditions   : File is open for output
   --
   -- Postconditions : Roman_Number is displayed in a field of Width characters
   --                  justified to the Left or to the Right.  If Width is not
   --                  sufficient to display the number, additional space will
   --                  be used (like the Width in Integer output).

   -----------------------------------------------------------------------------
   procedure Get (Item : out Number);
   -- Gets a Roman Number from the standard input file
   --
   -- Preconditions  : A blank or line terminator marks the end of the Roman number.
   --
   -- Postconditions : A valid additive Roman number is obtained from standard input
   --
   -- Exceptions     : Ada.IO_Exceptions.DATA_ERROR is raised if the data is not 
   --                     a valid additive Roman Number.  
   --                  CONSTRAINT_ERROR is raised if the number read contains
   --                     more than Max_Numerals numerals.

   -----------------------------------------------------------------------------
   procedure Get (File  : in  Ada.Text_IO.File_Type;
                  Item  : out Number);
   -- Gets a Roman Number from File
   --
   -- Preconditions  : A blank or line terminator marks the end of the Roman number.
   --
   -- Postconditions : A valid additive Roman number is obtained from File
   --
   -- Exceptions     : Ada.IO_Exceptions.DATA_ERROR is raised the data is not 
   --                     a valid additive Roman Number.
   --                  CONSTRAINT_ERROR is raised if the number read contains
   --                     more than Max_Numerals numerals. 

end Roman.Number_IO;