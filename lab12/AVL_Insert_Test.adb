with AVL_Tree.Check;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
procedure AVL_Insert_Test is

   -- Instantiate an AVL tree of natural numbers

   function Identity (Item : in Natural) return Natural is
   begin
      return Item;
   end Identity;

   package Natural_Tree is new AVL_Tree (Element_Type => Natural,
                                         Key_Type     => Natural,
                                         Key_Of       => Identity,
                                         "="          => "=",
                                         "<"          => "<");

   -- Process procedure for traversals
   procedure Put_Info (Item : in out Natural) is
   begin
      Put (Item  => Item,
           Width => 3);
   end Put_Info;

   package Natural_Tree_Check is new Natural_Tree.Check (Put => Put_Info);
   use Natural_Tree_Check;

   -- For test data
   type Natural_Array is array (Positive range <>) of Natural;


   ----------------------------------------------------------------------------
   procedure Test_Single_Left is
   -- Test of the single left rotation
      Tree : Natural_Tree.Tree_Type;
      Data : constant Natural_Array := (10, 15, 20, 25, 30);
   begin
      for Index in Data'Range loop
         Put ("Inserting ");
         Put (Item => Data (Index),
              Width => 1);
         New_Line;
         Tree.Insert (Data (Index));

         Put ("Size   is ");
         Put (Item  => Tree.Size,
              Width => 1);
         New_Line;

         Put ("Height is ");
         Put (Item  => Tree.Height,
              Width => 1);
         New_Line;

         Put ("Preorder  traversal is ");
         Tree.Traverse (Order => Natural_Tree.Preorder,
                        Process => Put_Info'Access);
         New_Line;

         Put ("Postorder traversal is ");
         Tree.Traverse (Order => Natural_Tree.Postorder,
                        Process => Put_Info'Access);
         New_Line;

         Put ("Inorder   traversal is ");
         Tree.Traverse (Order => Natural_Tree.Inorder,
                        Process => Put_Info'Access);
         New_Line;
         Check_Validity (Tree);
         New_Line (2);

      end loop;
   end Test_Single_Left;

   ----------------------------------------------------------------------------
   procedure Test_Single_Right is
   -- Test of the single left rotation
      Tree : Natural_Tree.Tree_Type;
      Data : constant Natural_Array := (40, 35, 30, 25, 20);
   begin
      for Index in Data'Range loop
         Put ("Inserting ");
         Put (Item => Data (Index),
              Width => 1);
         New_Line;
         Tree.Insert (Data (Index));

         Put ("Size   is ");
         Put (Item  => Tree.Size,
              Width => 1);
         New_Line;

         Put ("Height is ");
         Put (Item  => Tree.Height,
              Width => 1);
         New_Line;

         Put ("Preorder  traversal is ");
         Tree.Traverse (Order => Natural_Tree.Preorder,
                        Process => Put_Info'Access);
         New_Line;

         Put ("Postorder traversal is ");
         Tree.Traverse (Order => Natural_Tree.Postorder,
                        Process => Put_Info'Access);
         New_Line;

         Put ("Inorder   traversal is ");
         Tree.Traverse (Order => Natural_Tree.Inorder,
                        Process => Put_Info'Access);
         New_Line;
         Check_Validity (Tree);
         New_Line (2);

      end loop;
   end Test_Single_Right;

   ----------------------------------------------------------------------------
   procedure Test_Double_Left_Right is
   -- Test of the double left right rotation
      Tree   : Natural_Tree.Tree_Type;
      Data_1 : constant Natural_Array := (50, 30, 60, 20, 40);
   begin
      Put ("Inserting ");
      for Index in Data_1'Range loop
         Put (Item => Data_1 (Index),
              Width => 3);
         Tree.Insert (Data_1 (Index));
      end loop;
      New_Line;

      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);
      New_Line;
      Check_Validity (Tree);
      New_Line (2);

      Put_Line ("Inserting 35");
      Tree.Insert (35);
      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);
      New_Line;
      Check_Validity (Tree);
      New_Line (2);


      Tree.Clear;   -- Second test

      Put ("Inserting ");
      for Index in Data_1'Range loop
         Put (Item => Data_1 (Index),
              Width => 3);
         Tree.Insert (Data_1 (Index));
      end loop;
      New_Line;

      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);

      New_Line;
      Check_Validity (Tree);
      New_Line (2);

      Put_Line ("Inserting 45");
      Tree.Insert (45);
      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);

      New_Line;
      Check_Validity (Tree);
      New_Line (2);

   end Test_Double_Left_Right;

   ----------------------------------------------------------------------------
   procedure Test_Double_Right_Left is
   -- Test of the double right left rotation
      Tree   : Natural_Tree.Tree_Type;
      Data_1 : constant Natural_Array := (50, 30, 70, 60, 80);
   begin
      Put ("Inserting ");
      for Index in Data_1'Range loop
         Put (Item => Data_1 (Index),
              Width => 3);
         Tree.Insert (Data_1 (Index));
      end loop;
      New_Line;

      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);
      New_Line;
      Check_Validity (Tree);
      New_Line (2);

      Put_Line ("Inserting 65");
      Tree.Insert (65);
      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);
      New_Line;
      Check_Validity (Tree);
      New_Line (2);

      --------------------------------------------
      Tree.Clear;   -- Second test

      Put ("Inserting ");
      for Index in Data_1'Range loop
         Put (Item => Data_1 (Index),
              Width => 3);
         Tree.Insert (Data_1 (Index));
      end loop;
      New_Line;

      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);

      New_Line;
      Check_Validity (Tree);
      New_Line (2);

      Put_Line ("Inserting 55");
      Tree.Insert (55);
      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;

      Put ("Preorder  traversal is ");
      Tree.Traverse (Order => Natural_Tree.Preorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Postorder traversal is ");
      Tree.Traverse (Order => Natural_Tree.Postorder,
                     Process => Put_Info'Access);
      New_Line;

      Put ("Inorder   traversal is ");
      Tree.Traverse (Order => Natural_Tree.Inorder,
                     Process => Put_Info'Access);

      New_Line;
      Check_Validity (Tree);
      New_Line (2);

   end Test_Double_Right_Left;

   ----------------------------------------------------------------------------
   procedure Test_Big_Random_Tree is
   -- Testing randomly built big tree

      subtype Index_Range is Integer range 1 .. 20_000;
      package Random_Index is new Ada.Numerics.Discrete_Random (Index_Range);

      Generator : Random_Index.Generator;
      Tree : Natural_Tree.Tree_Type;
      Elements : array (Index_Range) of Index_Range;
      Swap_Index : Index_Range;
      Temp       : Index_Range;

   begin
      Random_Index.Reset (Gen => Generator, Initiator => 50614);
      for Index in Elements'Range loop
         Elements (Index) := Index;
      end loop;
      -- Shuffle the array five times
      for Count in 1 .. 5 loop
         for Index in Elements'Range loop
            Swap_Index := Random_Index.Random (Generator);
            Temp := Elements (Index);
            Elements (Index) := Elements (Swap_Index);
            Elements (Swap_Index) := Temp;
         end loop;
      end loop;

      Put_Line ("Inserting" & Positive'Image (Index_Range'Last) &
                " keys in random order");
      -- Insert the elements into the tree
      for Index in Elements'Range loop
         Tree.Insert (Elements (Index));
      end loop;
      Put ("Size   is ");
      Put (Item  => Tree.Size,
           Width => 1);
      New_Line;

      Put ("Height is ");
      Put (Item  => Tree.Height,
           Width => 1);
      New_Line;
      Check_Validity (Tree);
      New_Line (2);

   end Test_Big_Random_Tree;

begin
   Put_Line ("Press Enter to test two single right rotations");
   Put_Line ("----------------------------------------------");
   Skip_Line;
   Test_Single_Right;
   New_Line (2);

   Put_Line ("Press Enter to test two single left rotations");
   Put_Line ("---------------------------------------------");
   Skip_Line;
   Test_Single_Left;
   New_Line (2);

   Put_Line ("Press Enter to test two double right left rotations");
   Put_Line ("---------------------------------------------------");
   Skip_Line;
   Test_Double_Right_Left;
   New_Line (2);

   Put_Line ("Press Enter to test two double left right rotations");
   Put_Line ("---------------------------------------------------");
   Skip_Line;
   Test_Double_Left_Right;
   New_Line (2);

   Put_Line ("Press Enter to test a big random tree");
   Put_Line ("-------------------------------------");
   Skip_Line;
   Test_Big_Random_Tree;
   New_Line (2);

   Put_Line ("All Done");
   Put_Line ("--------");

end AVL_Insert_Test;