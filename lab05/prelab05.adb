with Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Strings.Bounded;
with Word_IO;

-- Written by  Vincent T. Mossman
-- Please ignore the bad style, it's for a prelab

procedure PreLab05 is

   package My_Strings is new Ada.Strings.Bounded.Generic_Bounded_Length
      (Max => 32);
   package My_Word_IO is new Word_IO (Max_Input_Line_Length => 1024,
                                      Word_Strings          => My_Strings);

   Text_File        : My_Word_IO.File_Type;
   File_Name        : String (1 .. 40);
   File_Name_Length : Natural range 0 .. 40;
   Words            : Natural;
   Letters          : Natural;
   Word             : My_Strings.Bounded_String;

begin

   -- Prepare file
   Ada.Text_IO.Put_Line ("Please enter a file name");
   Ada.Text_IO.Get_Line (Item => File_Name,
                         Last => File_Name_Length);

   My_Word_IO.Open (File => Text_File,
                    Name => File_Name (1 .. File_Name_Length));

   -- Initialize variables
   Words := 0;
   Letters := 0;

   -- Count words and letters
   loop
      exit when My_Word_IO.End_Of_File (Text_File);

      My_Word_IO.Get (File => Text_File,
                      Item => Word);

      Words := Words + 1;
      Letters := Letters + My_Strings.Length (Word);

   end loop;

   -- Display results
   Ada.Text_IO.Put ("Number of words in the text file: ");
   Ada.Float_Text_IO.Put (Item => Float (Words),
                          Fore => 0,
                          Aft  => 0,
                          Exp  => 0);

   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put ("Average number of letters per word: ");
   Ada.Float_Text_IO.Put (Item => Float (Letters) / Float (Words),
                          Fore => 0,
                          Aft  => 3,
                          Exp  => 0);

end PreLab05;