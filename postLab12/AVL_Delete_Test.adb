with AVL_Tree.Check;
with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
procedure AVL_Delete_Test is

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
   procedure Delete_Leaves is
   -- Test of deletions of leaves
      Tree : Natural_Tree.Tree_Type;
      Data : constant Natural_Array := (50, 80, 30, 40, 70, 10, 90);
   begin
      Put ("Inserting ");
      for Index in Data'Range loop
         Put (Item => Data (Index),
              Width => 3);
         Tree.Insert (Data (Index));
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

      for Index in reverse Data'Range loop
         Put ("Deleting");
         Put (Item => Data (Index),
              Width => 3);
         New_Line;
         Tree.Delete (Data (Index));

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
   end Delete_Leaves;

   ----------------------------------------------------------------------------
   procedure Delete_With_One is
   -- Test deletion of nodes with single child
      Tree        : Natural_Tree.Tree_Type;
      Data        : constant Natural_Array := (50, 30, 80, 10, 90);
      Delete_Data : constant Natural_Array := (30, 80);
   begin
      Put ("Inserting ");
      for Index in Data'Range loop
         Put (Item => Data (Index),
              Width => 3);
         Tree.Insert (Data (Index));
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

      for Index in  Delete_Data'Range loop
         Put ("Deleting");
         Put (Item => Delete_Data (Index),
              Width => 3);
         New_Line;
         Tree.Delete (Delete_Data (Index));

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
   end Delete_With_One;

   ----------------------------------------------------------------------------
   procedure Delete_With_Two is
   -- Test deletion of nodes with two children
      Tree   : Natural_Tree.Tree_Type;
      Data_1 : constant Natural_Array := (70, 50, 80, 30, 60, 90, 20, 40);
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

      Put_Line ("Deleting 50");
      Tree.Delete (50);

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


   end Delete_With_Two;

   ----------------------------------------------------------------------------
   procedure Put_Stats (Tree : in Natural_Tree.Tree_Type) is
   begin
      Put ("Tree size and height ");
      Put (Tree.Size);
      Put (Tree.Height);
      New_Line;
   end Put_Stats;

   ----------------------------------------------------------------------------
   procedure Big_Random_Tree is
   -- Testing randomly built big trees
      subtype Index_Range is Integer range 1 .. 20_000;
      package Random_Index is new Ada.Numerics.Discrete_Random (Index_Range);
      Generator : Random_Index.Generator;
      Tree : Natural_Tree.Tree_Type;
      Elements : array (Index_Range) of Positive;
      Swap_Index : Positive;
      Temp       : Positive;
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
      Put_Stats (Tree);
      Check_Validity (Tree);
      New_Line;
      Put_Line ("Deleting all keys in same ""random"" order");
      -- Delete the key from the tree into the tree
      for Index in Elements'Range loop
         Tree.Delete (Key => Elements (Index));
         if Index rem 2000 = 0 then
            Put_Stats (Tree);
         end if;
      end loop;
      Check_Validity (Tree);
      New_Line (2);
      Tree.Clear;


      Put_Line ("Inserting" & Positive'Image (Index_Range'Last) &
                " keys in random order");
      -- Insert the elements into the tree
      for Index in Elements'Range loop
         Tree.Insert (Elements (Index));
      end loop;
      Check_Validity (Tree);
      New_Line;
      Put_Line ("Deleting all keys in order");
      -- Delete the key from the tree into the tree
      for Index in Elements'Range loop
         Tree.Delete (Key => Index);
         if Index rem 2000 = 0 then
            Put_Stats (Tree);
         end if;
      end loop;
      Check_Validity (Tree);
      New_Line (2);
      Tree.Clear;


      Put_Line ("Inserting" & Positive'Image (Index_Range'Last) &
                " keys in random order");
      -- Insert the elements into the tree
      for Index in Elements'Range loop
         Tree.Insert (Elements (Index));
      end loop;
      Check_Validity (Tree);
      New_Line;
      Put_Line ("Deleting all keys in reverse order");
      -- Delete the key from the tree into the tree
      for Index in reverse Elements'Range loop
         if Index rem 2000 = 0 then
            Put_Stats (Tree);
         end if;
         Tree.Delete (Key => Index);
      end loop;
      Put_Stats (Tree);
      Check_Validity (Tree);
      New_Line;

   end Big_Random_Tree;


begin
   Put_Line ("Testing deletes of leaves");
   Put_Line ("-------------------------");
   New_Line;
   Delete_Leaves;
   New_Line (2);

   Put_Line ("Testing deletes of nodes with one child");
   Put_Line ("---------------------------------------");
   New_Line;
   Delete_With_One;
   New_Line (2);

   Put_Line ("Testing deletes of nodes with two children");
   Put_Line ("------------------------------------------");
   New_Line;
   Delete_With_Two;
   New_Line (2);

   Put_Line ("Testing deletes in a 'big' random tree");
   Put_Line ("--------------------------------------");
   New_Line;
   Big_Random_Tree;
   New_Line (2);


   Put_Line ("All Done");
   Put_Line ("--------");

end AVL_Delete_Test;