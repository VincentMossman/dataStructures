package body Word_IO is

   use type Word_Strings.Bounded_String;
   use type Buffer_Strings.Bounded_String;

   -- Written by  John McCormick
   --             February 18, 1998

   -- The key to this abstraction is that a read-ahead buffer is maintained
   -- for each file.  This buffer always contains the next words to be
   -- read.  When this buffer is empty, the file is exhausted.


   ----------------------------------------------------------------------------
   -- Internal procedures
   ----------------------------------------------------------------------------

   procedure Fill_Buffer (File   : in out Ada.Text_IO.File_Type;
                          Buffer :    out Buffer_Strings.Bounded_String) is

   -- This procedure obtains the next line that contains a word.  All blank lines
   -- and null lines are skipped
   --
   -- Preconditions  : none
   --
   -- Postconditions : If there are no words left in the file
   --                     Buffer is a null string.
   --                  else
   --                     Buffer contains the line with the next word in File.
   -- Exceptions     : Buffer_Strings.LENGTH_ERROR
   --                        raised if the length of the line exceeds
   --                        the bound of a Buffer_String
   --                  Ada.Text_IO.STATUS_ERROR
   --                        raised if File was not open

      Ch : Character;  -- Input Character

   begin
      Buffer := Buffer_Strings.Null_Bounded_String;  -- Initialize to empty string

      Line_Loop :  -- Get the line with the next word in the file
      loop         -- Each iteration, one input line is processed

         -- Two possible reasons to exit (see postconditions)
         exit Line_Loop when Ada.Text_IO.End_Of_File (File) or
                             Buffer_Strings.Length (Buffer) > 0;

         Char_Loop : -- Read one input line;
         loop        -- Each iteration, one character is processed
            exit Char_Loop when Ada.Text_IO.End_Of_Line (File);
            Ada.Text_IO.Get (File => File, Item => Ch);  -- Get the next character
            Buffer := Buffer & Ch;
         end loop Char_loop;

         -- Assertion:  Text_IO Reading marker is on the line terminator

         -- Move reading marker to next line
         if not Ada.Text_IO.End_Of_File (File) then
            Ada.Text_IO.Skip_Line (File);
         end if;

         -- Trim BOTH leading and trailing blanks from the line
         Buffer_Strings.Trim (Source => Buffer,
                              Side   => Ada.Strings.Both);
      end loop Line_Loop;
   end Fill_Buffer;


   ----------------------------------------------------------------------------
   procedure Take_First_Word (Source      : in out Buffer_Strings.Bounded_String;
                              Destination :    out Word_Strings.Bounded_String) is
   -- Removes the first word in Source and puts it in Destination
   --
   -- Preconditions  : Source contains a word.
   --                  Source contains no leading blanks.
   -- Postconditions : Source equals the original source with the first word removed.
   --                  Source contains no leading blanks.
   --                  Destination contains the first word in the original Source

      Blank_Position : Natural;   -- Position of first blank in Source
      Last           : Positive;  -- Position of last character of a word

   begin
      -- Locate the end of the first word in Source (marked by blank or end of Source)
      Blank_Position := Buffer_Strings.Index (Source  => Source,
                                              Pattern => " ");
      if Blank_Position = 0 then
         -- No blank found, last character of word is last character of Source
         Last := Buffer_Strings.Length (Source);
      else
         -- Blank found, last character of word is just before it
         Last := Blank_Position - 1;
      end if;

      -- Copy the first word of Source to Destination
      Destination := Word_Strings.To_Bounded_String
                        (Buffer_Strings.Slice (Source => Source,  -- Slice returns a
                                               Low    => 1,       -- standard String
                                               High   => Last));  -- type
      -- Remove the first word from the Source
      Buffer_Strings.Delete (Source  => Source,
                             From    => 1,
                             Through => Last);
      -- Trim any leading blanks (left side) in the buffer
      Buffer_Strings.Trim (Source => Source,
                           Side   => Ada.Strings.Left);
   end Take_First_Word;


   ----------------------------------------------------------------------------
   -- Public procedures
   ----------------------------------------------------------------------------

   procedure Get (File : in out File_Type;
                  Item :    out Word_Strings.Bounded_String) is

   -- Internal Postcondition : File.Buffer contains a word  or
   --                          there are no words left in the file

   begin

      -- First, check if there is an Ungot word to return
      if Word_Strings.Length (File.Ungot) > 0 then
         Item := File.Ungot;
         File.Ungot := Word_Strings.Null_Bounded_String;

      -- Second, check for possible end of file
      elsif Buffer_Strings.Length (File.Buffer) = 0 then
         raise END_ERROR;

      -- Otherwise, take the first word in the file buffer
      else
         Take_First_Word (Source      => File.Buffer,
                          Destination => Item);
      end if;

      -- If necessary, refill the Buffer for the next time Get is called
      if Buffer_Strings.Length (File.Buffer) = 0 then
         Fill_Buffer (File   => File.File,
                      Buffer => File.Buffer);
      end if;
   end Get;


   ----------------------------------------------------------------------------
   procedure Unget (File : in out File_Type;
                    Word : in     Word_Strings.Bounded_String) is
   begin
      -- Is there already an ungot word?
      if Word_Strings.Length (File.Ungot) > 0 then
         raise UNGET_OVERFLOW;
      else
         File.Ungot := Word;
      end if;
   end Unget;


   ----------------------------------------------------------------------------
   procedure Open (File : in out File_Type;
                   Name : in     String) is
   begin
      -- Open file as a text file
      Ada.Text_IO.Open (File => File.File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Name);
      -- Get the first line containing a word
      Fill_Buffer (File   => File.File,
                   Buffer => File.Buffer);
      -- No ungot word at this point
      File.Ungot := Word_Strings.Null_Bounded_String;
   end Open;


   ----------------------------------------------------------------------------
   procedure Close (File : in out File_Type) is
   begin
      -- Close the Text_File
      Ada.Text_IO.Close (File => File.File);
   end Close;


   ----------------------------------------------------------------------------
   function End_Of_File (File : in File_Type) return Boolean is
   begin
      -- We are at end of file if both the file buffer is empty and
      -- there is no ungot word.
      return  Buffer_Strings.Length (File.Buffer) = 0  and
              Word_Strings.Length (File.Ungot) = 0;
   end End_Of_File;

end Word_IO;