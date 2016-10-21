with Ada.Strings.Bounded;
with Ada.Text_IO;
generic
   -- Must supply a bound on length of file input lines
   Max_Input_Line_Length : Positive;

   -- Must supply a bounded length string package for
   -- processing words in the file.
   with package Word_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length (<>);

package Word_IO is

   -- This package implements a File Type for files whose components are "words"
   -- A word file is made up of lines of words. 

   -- A word is defined as a sequence of characters deliminated by blanks and
   -- end of line markers

   -- Written by  John McCormick
   --             February 18, 1998

   -- Assumptions :  No line in a file contains more than 
   --                Max_Input_Line_Length characters.

   type File_Type is limited private;   -- A file of words

   UNGET_OVERFLOW : exception;  -- potentially raised by procedure Unget
   END_ERROR      : exception;  -- potentially raised by procedure Get

   ----------------------------------------------------------------------------
   procedure Open (File : in out File_Type;
                   Name : in     String);
   -- Prepares File for input
   --
   -- Preconditions  : none
   --
   -- Postconditions : The File with name Name is prepared for input
   --
   -- Exceptions     : Ada.Text_IO.STATUS_ERROR
   --                        raised if File is already open

   ----------------------------------------------------------------------------
   procedure Close (File : in out File_Type);
   -- Terminates use of File
   --
   -- Preconditions  : none 
   --
   -- Postconditions : The connection between file and the input 
   --                  device is terminated
   --
   -- Exceptions     : Ada.Text_IO.STATUS_ERROR
   --                        raised if File is NOT open


   ----------------------------------------------------------------------------
   procedure Get (File : in out File_Type;
                  Item :    out Word_Strings.Bounded_String);
   -- Gets the next word from the file
   --
   -- Preconditions  : none
   --
   -- Postconditions : Item is the next word in File. 
   --                  Reading marker is moved to the delimiter that
   --                     marked the end of the word
   --
   -- Exceptions     : END_ERROR
   --                        raised if there are no more words in File
   --                  Ada.Text_IO.STATUS_ERROR
   --                        raised if File is not open
   --                  Word_Strings.LENGTH_ERROR
   --                        raised if the length of the next word exceeds 
   --                        the bound of a Word_String


   ----------------------------------------------------------------------------
   procedure Unget (File : in out File_Type;
                    Word : in     Word_Strings.Bounded_String);
   -- Returns the Word to the File so the next call to get returns Word.
   -- Only one word can be ungot before the next call to Get
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Word is returned to the File and the reading
   --                  marker is set to the beginning of the Word
   --
   -- Exceptions     : UNGET_OVERFLOW 
   --                       raised if an attempt is made to unget a second word
   --                       before getting the first ungot word

   ----------------------------------------------------------------------------
   function End_Of_File (File : in File_Type) return Boolean;
   -- Returns True if there are no more words remaining in the file
   --
   -- Preconditions  : none
   --
   -- Postconditions : Returns True if there are no more words in the file.
   --                  Otherwise, returns False
   --
   -- Exceptions     : Ada.Text_IO.STATUS_ERROR
   --                        raised if File was not open

private

   package Buffer_Strings is new 
           Ada.Strings.Bounded.Generic_Bounded_Length (Max_Input_Line_Length);

   type File_Type is 
      record
         File    : Ada.Text_IO.File_Type;          -- The text file
         Ungot   : Word_Strings.Bounded_String;    -- The ungot word
         Buffer  : Buffer_Strings.Bounded_String;  -- File buffer
      end record;

end Word_IO;