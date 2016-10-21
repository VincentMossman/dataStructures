with Ada.Sequential_IO;
with Binary_Search_Tree;
with Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Text_IO.Bounded_IO;

procedure Doolittle is

   package My_Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length (30);
   use My_Bounded_String;

   -- Instantiate package for the input and output of bounded strings
   package My_IO is new Ada.Text_IO.Bounded_IO (My_Bounded_String);

   subtype Name_String    is My_Bounded_String.Bounded_String;
   subtype Country_String is My_Bounded_String.Bounded_String;

   type Animal_Record is
      record
         Name    : Name_String;
         Country : Country_String;
      end record;

   package Animal_File is new Ada.Sequential_IO (Element_Type => Animal_Record);

   function Name_Of (Element : in Animal_Record) return Name_String is
   begin
      return Element.Name;
   end Name_Of;

   package Animal_Tree is new Binary_Search_Tree (Element_Type => Animal_Record,
                                                  Key_Type     => Name_String,
                                                  Key_Of       => Name_Of,
                                                  "="          => "=",
                                                  "<"          => "<");

   use Animal_Tree;


-------------------------------------------------------------------------------

   procedure Display_One_Animal (Element : in out Animal_Record) is
   begin

      Ada.Text_IO.Set_Col (To => 3);
      My_IO.Put (Element.Name);
      Ada.Text_IO.Set_Col (To => 16);
      My_IO.Put (Element.Country);
      Ada.Text_IO.New_Line;

   end Display_One_Animal;

-------------------------------------------------------------------------------

   procedure Display_From_Country (Tree    : in out Animal_Tree.Tree_Type;
                                   Country : in Country_String) is
      procedure Put_Animal (Element : in out Animal_Record) is
      begin
         if Element.Country = Country then
            Display_One_Animal (Element);
         end if;
      end Put_Animal;
   begin
      Tree.Traverse (Order   => Inorder,
                     Process => Put_Animal'Access);
   end Display_From_Country;

-------------------------------------------------------------------------------

   -- Variables for main subprogram

   Input_File  : Animal_File.File_Type;
   Animal      : Animal_Record;
   All_Animals : Animal_Tree.Tree_Type;
   Country     : Country_String;

begin
   Animal_File.Open (File => Input_File,
                     Mode => Animal_File.In_File,
                     Name => "Animals.seq");

   Ada.Text_IO.Put_Line ("Reading and echoing animal information.");
   Ada.Text_IO.New_Line;

   Input_Loop :  -- Read all of the animals
   loop          -- Each iteration, read and display one animal
      exit Input_Loop when Animal_File.End_Of_File (File => Input_File);
      Animal_File.Read (File => Input_File,
                        Item => Animal);
      Ada.Text_IO.Set_Col (To => 3);
      My_IO.Put (Animal.Name);
      Ada.Text_IO.Set_Col (To => 16);
      My_IO.Put (Animal.Country);
      Ada.Text_IO.New_Line;

      -- Insert Animal into Animal_Tree
      All_Animals.Insert (Animal);

   end loop Input_Loop;

   Animal_File.Close (Input_File);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Listing of nodes in key alphabetic order.");
   Ada.Text_IO.New_Line;

   -- Insert your first display code here.
   All_Animals.Traverse (Order   => Inorder,
                         Process => Display_One_Animal'Access);

   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line (Item => "Listing of nodes in key reverse alphabetic order.");
   Ada.Text_IO.New_Line;

   -- Insert your second display code here.
   All_Animals.Traverse (Order   => Reverse_Inorder,
                         Process => Display_One_Animal'Access);
   Ada.Text_IO.New_Line;



   -- Insert your call to Display_From_Country here.
   loop
      Ada.Text_IO.Put_Line ("Enter a country");
      My_IO.Get_Line (Country);
      exit when Country = "";
      Ada.Text_IO.New_Line;
      Display_From_Country (Tree    => All_Animals,
                            Country => Country);
      Ada.Text_IO.New_Line;
   end loop;


end Doolittle;