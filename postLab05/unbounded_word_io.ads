with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Unbounded_Word_IO is

   -- This package implements a File Type for files whose components are "words"
   -- A word file is made up of lines of words.

   -- A word is defined as a sequence of characters deliminated by blanks and
   -- end of line markers

   -- Written by  Vincent T. Mossman
   --             February 16, 2014

   type File_Type is limited private;   -- A file of words

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
                  Item :    out Ada.Strings.Unbounded.Unbounded_String);
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

   ----------------------------------------------------------------------------
   procedure Unget (File : in out File_Type;
                    Word : in     Ada.Strings.Unbounded.Unbounded_String);
   -- Returns the Word to the File so the next call to get returns Word.
   --
   -- Preconditions  : none
   --
   -- Postconditions : The Word is returned to the File and the reading
   --                  marker is set to the beginning of the Word

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

   type File_Type is
      record
         File    : Ada.Text_IO.File_Type;          -- The text file
         Buffer  : Ada.Strings.Unbounded.Unbounded_String;  -- File buffer
      end record;

end Unbounded_Word_IO;