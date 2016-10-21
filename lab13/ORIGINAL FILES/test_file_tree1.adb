with File_Tree;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Test_File_Tree1 is

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
   Data : constant Data_Array := (
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
      Ada.Text_IO.Put_Line ("The keys in the tree are");
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
      Ada.Text_IO.Put_Line ("The records in the tree are");
      My_Tree.InOrder_Traverse (Process => Display_Element'Access);
      Ada.Text_IO.New_Line (2);
   end Display_Keys_And_Data;


   ----------------------------------------------------------------------------
   procedure Check_Current (Key : in Positive) is
      Element : Element_Rec;
   begin
      if My_Tree.Current_Defined then
         My_Tree.Retrieve (Element);
         if Key /= Element.Key then
            Ada.Text_IO.Put_Line ("ERROR Current element is not correct");
         end if;
      else
         Ada.Text_IO.Put_Line ("ERROR Current is not defined");
      end if;
   end Check_Current;


   ----------------------------------------------------------------------------
   procedure Initial_Test is
   -- Display whether or not initial tree is empty and
   -- whether or not a current element is defined
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
   procedure Test_One is
   -- Tests Height and Inorder Traversal
   -- Assumes that the Clear and Insert operations work correctly
   begin
      My_Tree.Clear;
      Display_Keys;
      if My_Tree.Height = 0 then
         Ada.Text_IO.Put_Line ("Height works for empty tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for empty tree");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");
      My_Tree.Insert (Item => (Key => 100,  Data => 'A'));
      Check_Current (100);
      Display_Keys;
      if My_Tree.Height = 1 then
         Ada.Text_IO.Put_Line ("Height works for tree with one node");
      else
         Ada.Text_IO.Put_Line ("Height fails for tree with one node");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");
      My_Tree.Clear;
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => 'A'));
         Check_Current (Data (Count));
      end loop;
      Display_Keys;
      if My_Tree.Height = 4 then
         Ada.Text_IO.Put_Line ("Height works for bushy tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for bushy tree");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");
      My_Tree.Clear;
      for Count in 1 .. 100 loop
         My_Tree.Insert (Item => (Key => Count,  Data => 'A'));
         Check_Current (Count);
      end loop;
      Display_Keys;
      if My_Tree.Height = 100 then
         Ada.Text_IO.Put_Line ("Height works for right stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for right stick");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");
      My_Tree.Clear;
      for Count in reverse 1 .. 100 loop
         My_Tree.Insert (Item => (Key => Count,  Data => 'A'));
         Check_Current (Count);
      end loop;
      Display_Keys;
      if My_Tree.Height = 100 then
         Ada.Text_IO.Put_Line ("Height works for left stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for left stick");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");

      Ada.Text_IO.New_Line;

   end Test_One;


   ----------------------------------------------------------------------------
   procedure Test_Two is
   -- Test Find, Retrieve, and Modify
   -- Assumes that Clear and Insert work
      My_Data    : Character := 'A';
      My_Element : Element_Rec;
   begin
      My_Tree.Clear;
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => 'A'));
      end loop;
      Ada.Text_IO.Put_Line ("Original Data");
      Ada.Text_IO.Put_Line ("-----------------------");
      Display_Keys_And_Data;
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("Modified Data");
      Ada.Text_IO.Put_Line ("-----------------------");
      for Count in Data'Range loop
         My_Tree.Find (Key => Data (Count));
         My_Data := Character'Succ (My_Data);
         My_Tree.Modify (Element => (Key => Data (Count), Data => My_Data));
         My_Tree.Retrieve (Element => My_Element);
         Display_Element (My_Element);
      end loop;
      Ada.Text_IO.New_Line;
      Display_Keys_And_Data;
   end Test_Two;

   ----------------------------------------------------------------------------
   procedure Test_Three is
   -- Test exceptions
   -- Assumes that Clear, Find, and Insert work
      My_Element : Element_Rec;
   begin
      My_Tree.Clear;
      begin
         My_Tree.Retrieve (My_Element);
         Ada.Text_IO.Put_Line ("Unsuccessfull raising of Current_Undefined");
      exception
         when My_Tree.CURRENT_UNDEFINED =>
            Ada.Text_IO.Put_Line ("Successfull raising of Current_Undefined");
         when others =>
            Ada.Text_IO.Put_Line ("Unknown exception raised in Test_Three");
      end;

      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => 'A'));
      end loop;
      My_Tree.Find (Key => 333);
      begin
         My_Tree.Retrieve (My_Element);
         Ada.Text_IO.Put_Line ("Unsuccessfull raising of Current_Undefined");
      exception
         when My_Tree.CURRENT_UNDEFINED =>
            Ada.Text_IO.Put_Line ("Successfull raising of Current_Undefined");
         when others =>
            Ada.Text_IO.Put_Line ("Unknown exception raised in Test_Three");
      end;

      My_Tree.Insert (Item => (Key => 333,  Data => 'A'));
      begin
         My_Tree.Insert (Item => (Key => 333,  Data => 'B'));
         Ada.Text_IO.Put_Line ("Unsuccessfull raising of Duplicate_Key");
      exception
         when My_Tree.DUPLICATE_KEY =>
            Ada.Text_IO.Put_Line ("Successfull raising of Duplicate_Key");
         when others =>
            Ada.Text_IO.Put_Line ("Unknown exception raised in Test_Three");
      end;

      My_Tree.Find (Key => 333);
      begin
         My_Tree.Modify (Element => (Key => 777,  Data => 'B'));
         Ada.Text_IO.Put_Line ("Unsuccessfull raising of Key_Error");
      exception
         when My_Tree.KEY_ERROR =>
            Ada.Text_IO.Put_Line ("Successfull raising of Key_Error");
         when others =>
            Ada.Text_IO.Put_Line ("Unknown exception raised in Test_Three");
      end;

      begin
         My_Tree.Delete (Key => 777);
         Ada.Text_IO.Put_Line ("Unsuccessfull raising of Key_Error");
      exception
         when My_Tree.KEY_ERROR =>
            Ada.Text_IO.Put_Line ("Successfull raising of Key_Error");
         when others =>
            Ada.Text_IO.Put_Line ("Unknown exception raised in Test_Three");
      end;

   end Test_Three;

   ----------------------------------------------------------------------------
   procedure Setup_For_Next_Test_Run is
      My_Data : Character := 'a';
   begin
      My_Tree.Clear;
      for Count in Data'Range loop
         My_Tree.Insert (Item => (Key => Data (Count),  Data => My_Data));
         My_Data := Character'Succ (My_Data);
      end loop;
      My_Tree.Close;
   end Setup_For_Next_Test_Run;


begin
   Ada.Text_IO.Set_Line_Length (To => 80);

   Ada.Text_IO.Put_Line ("Intial tree characteristics");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Initial_Test;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Testing Insert, Clear, Height, and Inorder Traversal");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Test_One;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Testing Find, Retrieve, and Modify");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Test_Two;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Testing exceptions");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.New_Line;
   Test_Three;
   Ada.Text_IO.New_Line (3);


   Ada.Text_IO.Put_Line ("Set up for next test run and close the tree");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Setup_For_Next_Test_Run;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Normal termination of program Test_File_Tree1");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
end Test_File_Tree1;