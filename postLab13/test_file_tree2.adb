with File_Tree;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Test_File_Tree2 is

-- A program to test the file based binary search tree

   type Element_Rec is
      record
         Key  : Positive;
         Data : Character;
      end record;

   function Key_Of (Element : in Element_Rec) return Positive is
   begin
      return Element.Key;
   end Key_Of;

   package My_Tree is new File_Tree (Tree_File_Name   => "Tree.dat",
                                     Header_File_Name => "Header.dat",
                                     Element_Type     => Element_Rec,
                                     Key_Type         => Positive,
                                     Key_Of           => Key_Of,
                                     "="              => "=",
                                     "<"              => "<");

   type Data_Array is array (Positive range <>) of Positive;
   Data : Data_Array := (
                                    100,
                            50,                150,
                        25,     75,       125,      175,
                      15, 35, 65, 85,  115, 135,  165, 185);

   ----------------------------------------------------------------------------
   procedure Display_Keys is
      procedure Display_One_Key (Element : in out Element_Rec) is
      begin
         Ada.Integer_Text_IO.Put (Item  => Element.Key,
                                  Width => 4);
      end Display_One_Key;
   begin
      Ada.Text_IO.New_Line;
      My_Tree.InOrder_Traverse (Process => Display_One_Key'Access);
      Ada.Text_IO.New_Line (2);
   end Display_Keys;

   ----------------------------------------------------------------------------
   procedure Display_Element (Element : in out Element_Rec) is
   begin
      Ada.Integer_Text_IO.Put (Item  => Element.Key,
                               Width => 4);
      Ada.Text_IO.Put_Line (' ' & Element.Data);
   end Display_Element;


   ----------------------------------------------------------------------------
   procedure Display_Keys_And_Data is
   begin
      Ada.Text_IO.New_Line;
      My_Tree.InOrder_Traverse (Process => Display_Element'Access);
      Ada.Text_IO.New_Line (2);
   end Display_Keys_And_Data;

   ----------------------------------------------------------------------------
   procedure Initial_Test is
   begin
      Ada.Text_IO.Put ("The initial tree is");
      if not My_Tree.Empty then
         Ada.Text_IO.Put (" not");
      end if;
      Ada.Text_IO.Put_Line (" empty");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Current is");
      if not My_Tree.Current_Defined then
         Ada.Text_IO.Put (" not");
      end if;
      Ada.Text_IO.Put_Line (" defined");
      Ada.Text_IO.New_Line;
      Display_Keys_And_Data;
   end Initial_Test;

   ----------------------------------------------------------------------------
   procedure Delete_Test is
      My_Data : Character := 'A';
   begin
      My_Tree.Clear;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Deleting top to bottom");
      Ada.Text_IO.Put_Line ("----------------------");
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => My_Data));
         My_Data := Character'Succ (My_Data);
      end loop;

      Ada.Text_IO.Put_Line ("The keys in the original tree");
      Display_Keys;

      for Count in Data'Range loop
         My_Tree.Find (Key => Data (Count));
         My_Tree.Delete (Key => Data (Count));
         Ada.Text_IO.Put_Line ("The keys after deleting" & Integer'Image (Data (Count)));
         Display_Keys;
         Ada.Text_IO.New_Line;
         if My_Tree.Current_Defined then
            Ada.Text_IO.Put_Line ("Failure in delete, Current should not be defined");
         end if;
      end loop;

      if not My_Tree.Empty then
         Ada.Text_IO.Put_Line ("Failure in delete, Tree should be empty");
      end if;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Deleting in order");
      Ada.Text_IO.Put_Line ("-----------------");
      My_Data := 'a';
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => My_Data));
         My_Data := Character'Succ (My_Data);
      end loop;

      Ada.Text_IO.Put_Line ("The keys in the original tree");
      Display_Keys;

      Data := (15, 25, 35, 50, 65, 75, 85, 100, 115, 125, 135, 150, 165, 175, 185);
      My_Tree.Find (Key => Data (14));
      for Count in Data'Range loop
         My_Tree.Delete (Key => Data (Count));
         Ada.Text_IO.Put_Line ("The keys after deleting" & Integer'Image (Data (Count)));
         Display_Keys;
         Ada.Text_IO.New_Line;
         if My_Tree.Current_Defined then
            Ada.Text_IO.Put_Line ("Failure in delete, Current should not be defined");
         end if;
      end loop;

      if not My_Tree.Empty then
         Ada.Text_IO.Put_Line ("Failure in delete");
      end if;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Deleting in reverse order");
      Ada.Text_IO.Put_Line ("-------------------------");
      Data := (100, 50, 150, 25, 75, 125, 175, 15, 35, 65, 85, 115, 135, 165, 185);
      My_Data := 'a';
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => My_Data));
         My_Data := Character'Succ (My_Data);
      end loop;

      Ada.Text_IO.Put_Line ("The keys in the original tree");
      Display_Keys;

      Data := (15, 25, 35, 50, 65, 75, 85, 100, 115, 125, 135, 150, 165, 175, 185);
      for Count in reverse Data'Range loop
         My_Tree.Delete (Key => Data (Count));
         Ada.Text_IO.Put_Line ("The keys after deleting" & Integer'Image (Data (Count)));
         Display_Keys;
         Ada.Text_IO.New_Line;
      end loop;

      if not My_Tree.Empty then
         Ada.Text_IO.Put_Line ("Failure in delete, Tree should be empty");
      end if;

   end Delete_Test;



begin
   Ada.Text_IO.Set_Line_Length (To => 80);

   Ada.Text_IO.Put_Line ("Intial tree characteristics");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Initial_Test;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Testing Delete");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Delete_Test;
   Ada.Text_IO.New_Line (3);

   My_Tree.Close;
   Ada.Text_IO.Put_Line ("Normal termination of program Test_File_Tree2");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
end Test_File_Tree2;