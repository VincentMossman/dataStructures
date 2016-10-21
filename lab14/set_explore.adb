--with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Hash;
with Ada.Text_IO;     use Ada.Text_IO;
procedure Set_Explore is

   package Name_Set is new Ada.Containers.Indefinite_Ordered_Sets
                           (Element_Type        => String,
                            "<"                 => "<",
                            "="                 => Standard."=");
   use Name_Set;

   ----------------------------------------------------------------------------
   procedure Display_Names (Names : in Name_Set.Set) is
   -- Display all of the names in a set
   -- Preconditions  : none
   -- Postconditions : All names in the set are displayed, one name per line

      procedure Display_Names (Position : in Cursor) is
      begin
         Put_Line (Element (Position));
      end Display_Names;

   begin

      Iterate (Container => Names,
               Process   => Display_Names'Access);

      New_Line (2);
   end Display_Names;

   ----------------------------------------------------------------------------
   procedure Build_Set (File_Name : in  String;
                        Set       : out Name_Set.Set) is
   -- Create a set of strings obtained from a file
   -- Preconditions  : The file exists
   -- Postconditions : The names in the file are put into Set

      subtype Line_String is String (1 .. 80);

      Line : Line_String;   -- One input line and
      Last : Natural;       -- its length

      File : Ada.Text_IO.File_Type;  -- Input file

   begin
      Set.Clear;
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);
      loop
         exit when End_Of_File (File);
         Get_Line (File, Line, Last);
         begin
            Set.Insert (New_Item => Line (1 .. Last));
         exception
            when Constraint_Error =>
               Put_Line (Item => Line (1 .. Last) & " is a duplicate name");
         end;
      end loop;

      Ada.Text_IO.Close (File);
   end Build_Set;

-------------------------------------------------------------------------------

   -- Lecture sections
   Section_01 : Name_Set.Set;
   Section_02 : Name_Set.Set;
   -- Lab sections
   Section_71 : Name_Set.Set;
   Section_72 : Name_Set.Set;


begin
   Put_Line ("Building set for Lecture Section 01");
   Build_Set (File_Name  => "section01.txt",
              Set        => Section_01);
   New_Line;
   Display_Names (Section_01);
   New_Line (2);

   Put_Line ("Building set for Lecture Section 02");
   Build_Set (File_Name  => "section02.txt",
              Set        => Section_02);
   New_Line;
   Display_Names (Section_02);
   New_Line (2);


   Put_Line ("Building set for Lab Section 71");
   Build_Set (File_Name  => "section71.txt",
              Set        => Section_71);
   New_Line;
   Display_Names (Section_71);
   New_Line (2);


   Put_Line ("Building set for Lab Section 72");
   Build_Set (File_Name  => "section72.txt",
              Set        => Section_72);
   New_Line;
   Display_Names (Section_72);
   New_Line (2);

   Put_Line ("Students who are in both lecture sections");
   New_Line;
   Display_Names (Names => Section_01 and Section_02);
   New_Line (2);

   Put_Line ("Students who are in a lecture but not in a lab");
   New_Line;
   Display_Names (Names => (Section_01 or Section_02) - (Section_71 or Section_72));
   New_Line (2);


   Put_Line ("All done");
end Set_Explore;