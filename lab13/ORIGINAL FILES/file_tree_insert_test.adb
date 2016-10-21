with File_Tree;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
procedure File_Tree_Insert_Test is

-- A program to test several operations in the file based binary search tree

   type Element_Rec is
      record
         Key  : Positive;
         Data : Character;
      end record;


   package My_Tree is new File_Tree (



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
   procedure Build_Bushy is
      type Key_Array is array (Positive range <>) of Positive;
      Keys : constant Key_Array := (
                                       100,
                               50,                150,
                           25,     75,       125,      175,
                         15, 35, 65, 85,  115, 135,  165, 185);

      subtype Lowercase is Character range 'a' .. 'z';
      package Random_Character is new Ada.Numerics.Discrete_Random (Lowercase);
      My_Generator : Random_Character.Generator;

   begin
      My_Tree.Clear;
      Random_Character.Reset (Gen       => My_Generator,
                              Initiator => 50614);
      My_Tree.Clear;
      for Count in Keys'Range loop
         My_Tree.Insert (Item => (Key  => Keys (Count),
                                  Data => Random_Character.Random (My_Generator)));
      end loop;
   end Build_Bushy;


   ----------------------------------------------------------------------------
   procedure Test_Insert_And_Height is
   -- Assumes that the Clear operation works correctly

   begin

      My_Tree.Clear;
      if My_Tree.Height = 0 then
         Ada.Text_IO.Put_Line ("Height works for empty tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for empty tree");
      end if;
      Ada.Text_IO.Put_Line ("-----------------------");


      My_Tree.Insert (Item => (Key => 100,  Data => 'A'));
      Check_Current (100);
      if My_Tree.Height = 1 then
         Ada.Text_IO.Put_Line ("Height works for tree with one node");
      else
         Ada.Text_IO.Put_Line ("Height fails for tree with one node");
      end if;
      Display_Keys_And_Data;
      Ada.Text_IO.Put_Line ("-----------------------");

      My_Tree.Clear;
      for Count in 1 .. 15 loop
         My_Tree.Insert (Item => (Key => Count,  Data => 'A'));
         Check_Current (Count);
      end loop;
      if My_Tree.Height = 15 then
         Ada.Text_IO.Put_Line ("Height works for right stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for right stick");
      end if;
      Display_Keys_And_Data;
      Ada.Text_IO.Put_Line ("-----------------------");

      My_Tree.Clear;
      for Count in reverse 1 .. 17 loop
         My_Tree.Insert (Item => (Key => Count,  Data => 'A'));
         Check_Current (Count);
      end loop;
      if My_Tree.Height = 17 then
         Ada.Text_IO.Put_Line ("Height works for left stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for left stick");
      end if;
      Display_Keys_And_Data;
      Ada.Text_IO.Put_Line ("-----------------------");

      Build_Bushy;
      if My_Tree.Height = 4 then
         Ada.Text_IO.Put_Line ("Height works for bushy tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for bushy tree");
      end if;
      Display_Keys_And_Data;
      Ada.Text_IO.Put_Line ("-----------------------");


      Ada.Text_IO.New_Line;

   end Test_Insert_And_Height;


begin

   Ada.Text_IO.Put_Line ("Press Return to test Inorder Travesal");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.Skip_Line;
   Ada.Text_IO.Put_Line ("Bushy Tree");
   Display_Keys_And_Data;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Empty Tree");
   My_Tree.Clear;
   Display_Keys_And_Data;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Press Return to test Insert and Height");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   Ada.Text_IO.Skip_Line;
   Test_Insert_And_Height;
   Ada.Text_IO.New_Line (3);

   Ada.Text_IO.Put_Line ("Normal termination. Saving the tree for another run.");
   Ada.Text_IO.Put_Line ("----------------------------------------------------");
   My_Tree.Close;
end File_Tree_Insert_Test;