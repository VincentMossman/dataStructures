with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Unbounded_Word_IO; use Unbounded_Word_IO;

procedure My_Test is

   -- Constants for file names
   Empty_File_Name          : constant String := ("Empty_File.txt");
   Ten_LT_File_Name         : constant String := ("Ten_LT_File.txt");
   One_Word_File_Name       : constant String := ("One_Word_File.txt");
   Ten_Word_File_Name       : constant String := ("Ten_Word_File.txt");
   Three_By_Three_File_Name : constant String := ("Three_By_Three.txt");

-------------------------------------------------------------------------------

   procedure Test_End_Of_File is

   -- Procedure tests End_Of_File function in Unbounded_Word_IO package
   --
   -- Preconditions  : None
   --
   -- Postconditions : Results of tests are displayed on screen

      -- Test File Variables
      Empty_File, Ten_LT_File, One_Word_File, Ten_Word_File : File_Type;

      -- Variable for word I/O
      Word : Ada.Strings.Unbounded.Unbounded_String;

   begin

      -- Open Test Files
      Open (File => Empty_File,
            Name => Empty_File_Name);

      Open (File => Ten_LT_File,
            Name => Ten_LT_File_Name);

      Open (File => One_Word_File,
            Name => One_Word_File_Name);

      Open (File => Ten_Word_File,
            Name => Ten_Word_File_Name);

      -- Test End_Of_File functionality for an empty file.
      Ada.Text_IO.Put ("Executing End_OF_File Test One (Empty File)");
      Ada.Text_IO.New_Line;
      if End_Of_File (Empty_File) then
         Ada.Text_IO.Put ("End_Of_File Test One Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test One Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test End_Of_File functionality for a file with no words
      --  but multiple line terminators.
      Ada.Text_IO.Put ("Executing End_Of_File Test Two (Multiple Line Terminators)");
      Ada.Text_IO.New_Line;
      if End_Of_File (Ten_LT_File) then
         Ada.Text_IO.Put ("End_Of_File Test Two Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test Two Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test End_Of_File functionality for a file with one word,
      --  and the reading marker is at the beginning of the file.
      Ada.Text_IO.Put ("Executing End_Of_File Test Three (One Word)");
      Ada.Text_IO.New_Line;
      if not End_Of_File (One_Word_File) then
         Ada.Text_IO.Put ("End_Of_File Test Three Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test Three Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test End_Of_File functionality for a file with multiple words.
      Ada.Text_IO.Put ("Executing End_Of_File Test Four (Multiple Words)");
      Ada.Text_IO.New_Line;
      if not End_Of_File (Ten_Word_File) then
         Ada.Text_IO.Put ("End_Of_File Test Four Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test Four Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test End_Of_File functionality for a file with one word, and the
      --  Get procedure has been called once (moving the reading marker
      --  to the end of the first and only word).
      Ada.Text_IO.Put ("Executing End_Of_File Test Five (One Word, Get Called Once)");
      Ada.Text_IO.New_Line;

      Get (File => One_Word_File,
           Item => Word);

      if End_Of_File (One_Word_File) then
         Ada.Text_IO.Put ("End_Of_File Test Five Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test Five Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test End_Of_File functionality for a file with multiple words, and
      --  the Get procedure has been called multiple times (moving the reading
      --  marker to the end of the last word).
      Ada.Text_IO.Put ("Executing End_Of_File Test Six (Multiple Words, Get Called Multiple Times)");
      Ada.Text_IO.New_Line;

      -- Loop gets all words in file
      -- Each iteration, gets one word
      loop
         exit when End_Of_File (Ten_Word_File);
         Get (File => Ten_Word_File,
              Item => Word);
      end loop;

      if End_Of_File (One_Word_File) then
         Ada.Text_IO.Put ("End_Of_File Test Six Passed");
      else
         Ada.Text_IO.Put ("End_Of_File Test Six Failed");
      end if;

      Ada.Text_IO.New_Line;

      -- Close used files
      Close (Empty_File);
      Close (Ten_LT_File);
      Close (One_Word_File);
      Close (Ten_Word_File);

   end Test_End_Of_File;

-------------------------------------------------------------------------------

   procedure Test_Get is

   -- Procedure tests Get function in Unbounded_Word_IO package
   --
   -- Preconditions  : None
   --
   -- Postconditions : Results of tests are displayed on screen

      -- File Type variables
      Empty_File, One_Word_File, Ten_Word_File, Three_By_Three_File : File_Type;

      -- Variable for Word I/O
      Word : Ada.Strings.Unbounded.Unbounded_String;

   begin

      -- Open Test Files
      Open (File => Empty_File,
            Name => Empty_File_Name);

      Open (File => One_Word_File,
            Name => One_Word_File_Name);

      Open (File => Ten_Word_File,
            Name => Ten_Word_File_Name);

      Open (File => Three_By_Three_File,
            Name => Three_By_Three_File_Name);

      -- Test End_Error
      Ada.Text_IO.Put ("Executing Get Test One (Empty File, END_ERROR Test)");
      Ada.Text_IO.New_Line;
      Test_Exception_Block :
      begin

         Get (File => Empty_File,
              Item => Word);

         Ada.Text_IO.Put ("Get Test One Failed");

         exception
            when Unbounded_Word_IO.END_ERROR =>
               Ada.Text_IO.Put ("Exception Raised");
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put ("Get Test One Passed");

      end Test_Exception_Block;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test Get functionality for a file with one word
      Ada.Text_IO.Put ("Executing Get Test Two (One Word)");
      Get (File => One_Word_File,
           Item => Word);

      Ada.Text_IO.Put ("Expected Result: Mildred");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Actual Result: ");
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Word));
      Ada.Text_IO.New_Line;

      if Ada.Strings.Unbounded.To_String (Word) = "Mildred" then
         Ada.Text_IO.Put ("Get Test Two Passed");
      else
         Ada.Text_IO.Put ("Get Test Two Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test Get functionality for a file with multiple words
      --  (using multiple calls to procedure Get)
      Ada.Text_IO.Put ("Executing Get Test Three (Multiple Words)");
      Ada.Text_IO.New_Line;

      -- Loop gets all words in file
      -- Each iteration, gets one word
      loop
         exit when End_Of_File (Ten_Word_File);
         Get (File => Ten_Word_File,
              Item => Word);
      end loop;

      Ada.Text_IO.Put ("Expected Result (Final Get): do");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Actual Result: ");
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Word));
      Ada.Text_IO.New_Line;

      if Ada.Strings.Unbounded.To_String (Word) = "do" then
         Ada.Text_IO.Put ("Get Test Three Passed");
      else
         Ada.Text_IO.Put ("Get Test Three Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test Get functionality for a file with multiple words on
      --  multiple lines (using multiple calls to procedure Get).
      Ada.Text_IO.Put ("Executing Get Test Four (Multiple Words, Multiple Lines)");
      Ada.Text_IO.New_Line;

      -- Loop gets all words in file
      -- Each iteration, gets one word
      loop
         exit when End_Of_File (Three_By_Three_File);
         Get (File => Three_By_Three_File,
              Item => Word);
      end loop;

      Ada.Text_IO.Put ("Expected Result (Final Get): sed");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Actual Result: ");
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Word));
      Ada.Text_IO.New_Line;

      if Ada.Strings.Unbounded.To_String (Word) = "sed" then
         Ada.Text_IO.Put ("Get Test Four Passed");
      else
         Ada.Text_IO.Put ("Get Test Four Failed");
      end if;

      Ada.Text_IO.New_Line;

      -- Close Test Files
      Close (Empty_File);
      Close (One_Word_File);
      Close (Ten_Word_File);
      Close (Three_By_Three_File);

   end Test_Get;

-------------------------------------------------------------------------------

   procedure Test_Unget is

   -- Procedure tests Unget function in Unbounded_Word_IO package
   --
   -- Preconditions  : None
   --
   -- Postconditions : Results of tests are displayed on screen

      -- File Type variables
      One_Word_File, Ten_Word_File : File_Type;

      -- Variable for Word I/O
      Word, Word_Two, Word_Three : Ada.Strings.Unbounded.Unbounded_String;

   begin

      -- Open Test File
      Open (File => One_Word_File,
            Name => One_Word_File_Name);

      Open (File => Ten_Word_File,
            Name => Ten_Word_File_Name);

      -- Test Unget functionality for a file with one word after
      --  Get has been called one time.
      Ada.Text_IO.Put ("Executing Unget Test One (One Word)");
      Ada.Text_IO.New_Line;

      Get (File => One_Word_File,
           Item => Word);

      Unget (File => One_Word_File,
             Word => Word);

      Get (File => One_Word_File,
           Item => Word);

      Ada.Text_IO.Put ("Expected Result (After Get Has Been Called Again): Mildred");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Actual Result: ");
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Word));
      Ada.Text_IO.New_Line;

      if Ada.Strings.Unbounded.To_String (Word) = "Mildred" then
         Ada.Text_IO.Put ("Unget Test One Passed");
      else
         Ada.Text_IO.Put ("Unget Test One Failed");
      end if;

      Ada.Text_IO.New_Line (Spacing => 2);

      -- Test Unget functionality for a file with one word after
      --  Get has been called one time.
      Ada.Text_IO.Put ("Executing Unget Test Two (Multiple Words)");
      Ada.Text_IO.New_Line;

      Get (File => Ten_Word_File,
           Item => Word);

      Get (File => Ten_Word_File,
           Item => Word_Two);

      Get (File => Ten_Word_File,
           Item => Word_Three);

      -- Call unget multiple times
      Unget (File => Ten_Word_File,
             Word => Word_Three);

      Unget (File => Ten_Word_File,
             Word => Word_Two);

      Unget (File => Ten_Word_File,
             Word => Word);

      Get (File => Ten_Word_File,
           Item => Word);

      Ada.Text_IO.Put ("Expected Result (After Get Has Been Called Again): Lorem");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Actual Result: ");
      Ada.Text_IO.Put (Ada.Strings.Unbounded.To_String (Word));
      Ada.Text_IO.New_Line;

      if Ada.Strings.Unbounded.To_String (Word) = "Lorem" then
         Ada.Text_IO.Put ("Unget Test Two Passed");
      else
         Ada.Text_IO.Put ("Unget Test Two Failed");
      end if;

      -- Close Test Files
      Close (One_Word_File);
      Close (Ten_Word_File);

   end Test_Unget;

-------------------------------------------------------------------------------

   procedure Put_Horizontal_Line is

   -- Procedure displays horizontal line
   --
   -- Preconditions  : None
   --
   -- Postconditions : Horizontal Line is displayed

   begin

      -- Loop displays horizontal line of dashes
      -- Each iteration, displays one dash
      for I in 1 .. 60 loop
         Ada.Text_IO.Put ('-');
      end loop;

   end Put_Horizontal_Line;

-------------------------------------------------------------------------------

begin

   -- Run Tests
   Test_End_Of_File;

   Ada.Text_IO.New_Line;
   Put_Horizontal_Line;
   Ada.Text_IO.New_Line (Spacing => 2);

   Test_Get;

   Ada.Text_IO.New_Line;
   Put_Horizontal_Line;
   Ada.Text_IO.New_Line (Spacing => 2);

   Test_Unget;

end My_Test;