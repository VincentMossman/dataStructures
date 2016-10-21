with Binary_Search_Tree;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Test_New_Ops is

-- A program to test the new operations added to the Binary Search Tree Class
-- in Post Lab #14

   type Element_Rec is
      record
         Key  : Positive;
         Data : Character;
      end record;

   function Key_Of (Element : in Element_Rec) return Positive is
   begin
      return Element.Key;
   end Key_Of;

   package My_Tree is new Binary_Search_Tree (Element_Type => Element_Rec,
                                              Key_Type     => Positive,
                                              Key_Of       => Key_Of,
                                              "="          => "=",
                                              "<"          => "<");

   type Data_Array is array (Positive range <>) of Positive;
   Data : constant Data_Array := (
                                    100,
                            50,                150,
                        25,     75,       125,      175,
                      15, 35, 65, 85,  115, 135,  165, 185);


   ----------------------------------------------------------------------------
   procedure Test_Same_Shape is
   -- Assumes that the Clear and Insert operations works correctly

      A_Tree : My_Tree.Tree_Type;
      B_Tree : My_Tree.Tree_Type;

   begin
      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for empty trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for empty trees");
      end if;
      ------------------
      My_Tree.Insert (Tree => A_Tree,
                      Item => (Key => 100, Data => 'A'));
      My_Tree.Insert (Tree => B_Tree,
                      Item => (Key => 152, Data => 'B'));
      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for trees with a single equal element");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for trees with a single equal element");
      end if;

      ------------------
      My_Tree.Clear (A_Tree);
      My_Tree.Clear (B_Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => A_Tree,
                         Item => (Key => Data (Count),     Data => 'A'));
         My_Tree.Insert (Tree => B_Tree,
                         Item => (Key => Data (Count) + 1, Data => 'B'));
      end loop;
      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for same shape bushy trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for same shape bushy trees");
      end if;
      ------------------
      My_Tree.Insert (Tree => B_Tree,
                      Item => (Key => 120, Data => 'B'));
      if not My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for different shaped bushy trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for different shaped bushy trees");
      end if;
      ------------------
      My_Tree.Insert (Tree => A_Tree,
                      Item => (Key => 120, Data => 'A'));
      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for same shape bushy trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for same shape bushy trees");
      end if;
      ------------------
      My_Tree.Insert (Tree => A_Tree,
                      Item => (Key => 60, Data => 'A'));
      if not My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for different shaped bushy trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for different shaped bushy trees");
      end if;
      ------------------
      My_Tree.Insert (Tree => B_Tree,
                      Item => (Key => 60, Data => 'B'));
      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for same shape bushy trees");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for same shape bushy trees");
      end if;
      ------------------
      My_Tree.Clear (A_Tree);
      My_Tree.Clear (B_Tree);
      for Count in 20 .. 100 loop
         My_Tree.Insert (Tree => A_Tree,
                         Item => (Key => 2 * Count, Data => 'A'));
         My_Tree.Insert (Tree => B_Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;

      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for same shape right sticks");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for same shape right sticks");
      end if;
      ------------------
      My_Tree.Clear (A_Tree);
      My_Tree.Clear (B_Tree);
      for Count in reverse 20 .. 100 loop
         My_Tree.Insert (Tree => A_Tree,
                         Item => (Key => 2 * Count, Data => 'A'));
         My_Tree.Insert (Tree => B_Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;

      if My_Tree.Same_Shape (A_Tree, B_Tree) then
         Ada.Text_IO.Put_Line ("Same_Shape works for same shape left sticks");
      else
         Ada.Text_IO.Put_Line ("Same_Shape fails for same shape left sticks");
      end if;

      Ada.Text_IO.New_Line;

   end Test_Same_Shape;

   ----------------------------------------------------------------------------
   procedure Test_Height is
   -- Assumes that the Clear and Insert operations works correctly

      Tree : My_Tree.Tree_Type;
   begin

      if My_Tree.Height (Tree) = 0 then
         Ada.Text_IO.Put_Line ("Height works for empty tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for empty tree");
      end if;
      ------------------
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 100,  Data => 'A'));
      if My_Tree.Height (Tree) = 1 then
         Ada.Text_IO.Put_Line ("Height works for tree with one node");
      else
         Ada.Text_IO.Put_Line ("Height fails for tree with one node");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Data (Count),  Data => 'A'));
      end loop;
      if My_Tree.Height (Tree) = 4 then
         Ada.Text_IO.Put_Line ("Height works for bushy tree");
      else
         Ada.Text_IO.Put_Line ("Height fails for bushy tree");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in 1 .. 100 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Count,  Data => 'A'));
      end loop;
      if My_Tree.Height (Tree) = 100 then
         Ada.Text_IO.Put_Line ("Height works for right stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for right stick");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in reverse 1 .. 100 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Count,  Data => 'A'));
      end loop;
      if My_Tree.Height (Tree) = 100 then
         Ada.Text_IO.Put_Line ("Height works for left stick");
      else
         Ada.Text_IO.Put_Line ("Height fails for left stick");
      end if;

      Ada.Text_IO.New_Line;

   end Test_Height;

   ----------------------------------------------------------------------------
   procedure Test_Leaf_Count is
   -- Assumes that the Clear and Insert operations works correctly

      Tree : My_Tree.Tree_Type;
   begin

      if My_Tree.Leaf_Count (Tree) = 0 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for empty tree");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for empty tree");
      end if;
      ------------------
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 100,  Data => 'A'));
      if My_Tree.Leaf_Count (Tree) = 1 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for tree with one node");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for tree with one node");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Data (Count),  Data => 'A'));
      end loop;
      if My_Tree.Leaf_Count (Tree) = 8 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for bushy tree");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for bushy tree");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in 1 .. 10 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Count,  Data => 'A'));
      end loop;
      if My_Tree.Leaf_Count (Tree) = 1 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for right stick");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for right stick");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      for Count in reverse 1 .. 10 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Count,  Data => 'A'));
      end loop;
      if My_Tree.Leaf_Count (Tree) = 1 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for left stick");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for left stick");
      end if;
      ------------------
      for Count in 11 .. 19 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Count,  Data => 'A'));
      end loop;
      if My_Tree.Leaf_Count (Tree) = 2 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for V stick");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for V stick");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 15,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 10,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 5,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 7,  Data => 'A'));
      if My_Tree.Leaf_Count (Tree) = 1 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for single child root");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for single child root");
      end if;
      ------------------
      My_Tree.Clear (Tree);
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 50,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 60,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 70,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 65,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 75,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 62,  Data => 'A'));
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 80,  Data => 'A'));
      if My_Tree.Leaf_Count (Tree) = 2 then
         Ada.Text_IO.Put_Line ("Leaf_Count works for bird claw tree");
      else
         Ada.Text_IO.Put_Line ("Leaf_Count fails for bird claw tree");
      end if;

      Ada.Text_IO.New_Line;

   end Test_Leaf_Count;


   ----------------------------------------------------------------------------
   procedure Test_Copy is
   -- Assumes that the Clear, Insert, and Same_Shape, and Height operations
   -- work correctly

      A_Tree : My_Tree.Tree_Type;
      B_Tree : My_Tree.Tree_Type;

   begin

      My_Tree.Copy (Source      => A_Tree,
                    Destination => B_Tree);
      if My_Tree.Same_Shape (A_Tree, B_Tree)                       and
         My_Tree.Height (A_Tree)     = My_Tree.Height (B_Tree)     and
         My_Tree.Leaf_Count (A_Tree) = My_Tree.Leaf_Count (B_Tree) then

         Ada.Text_IO.Put_Line ("Copy works for empty tree to empty tree");
      else
         Ada.Text_IO.Put_Line ("Copy fails for empty tree to empty tree");
      end if;
      ------------------
      for Count in Data'Range loop
         My_Tree.Insert (Tree => A_Tree,
                         Item => (Key => Data (Count), Data => 'A'));
      end loop;
      for Count in 10 .. 250 loop
         My_Tree.Insert (Tree => B_Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;
      My_Tree.Copy (Source => A_Tree, Destination => B_Tree);
      if My_Tree.Same_Shape (A_Tree, B_Tree)                       and
         My_Tree.Height (A_Tree)     = My_Tree.Height (B_Tree)     and
         My_Tree.Leaf_Count (A_Tree) = My_Tree.Leaf_Count (B_Tree) then

         Ada.Text_IO.Put_Line ("Copy works for nonempty tree to nonempty tree");
      else
         Ada.Text_IO.Put_Line ("Copy fails for nonempty tree to nonempty tree");
      end if;
      ------------------
      My_Tree.Insert (Tree => B_Tree,
                      Item => (Key => 12345, Data => 'B'));
      if My_Tree.Same_Shape (A_Tree, B_Tree)                       and
         My_Tree.Height (A_Tree)     = My_Tree.Height (B_Tree)     and
         My_Tree.Leaf_Count (A_Tree) = My_Tree.Leaf_Count (B_Tree) then
         Ada.Text_IO.Put_Line ("Copy fails for nonempty tree to nonempty tree. " &
                               "Trees appear to be aliases not copies!");
      else
         Ada.Text_IO.Put_Line ("Copy works for nonempty tree to nonempty tree");
      end if;
      ------------------
      My_Tree.Clear (A_Tree);
      My_Tree.Clear (B_Tree);
      for Count in reverse 10 .. 25 loop
         My_Tree.Insert (Tree => B_Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;
      My_Tree.Copy (Source => A_Tree, Destination => B_Tree);
      if My_Tree.Same_Shape (A_Tree, B_Tree)                       and
         My_Tree.Height (A_Tree)     = My_Tree.Height (B_Tree)     and
         My_Tree.Leaf_Count (A_Tree) = My_Tree.Leaf_Count (B_Tree) then

         Ada.Text_IO.Put_Line ("Copy works for empty tree to nonempty tree");
      else
         Ada.Text_IO.Put_Line ("Copy fails for empty tree to nonempty tree");
      end if;
      ------------------
      A_Tree.Clear;
      B_Tree.Clear;
      for Count in Data'Range loop
         A_Tree.Insert ((Key => Data (Count), Data => 'A'));
      end loop;
      A_Tree.Find (75);
      My_Tree.Copy (Source => A_Tree, Destination => B_Tree);
      B_Tree.Find (75);
      if My_Tree.Same_Shape (A_Tree, B_Tree)                       and
         My_Tree.Height (A_Tree)     = My_Tree.Height (B_Tree)     and
         My_Tree.Leaf_Count (A_Tree) = My_Tree.Leaf_Count (B_Tree) and
         B_Tree.Current_Defined                                    then

         Ada.Text_IO.Put_Line ("Copy works for bushy tree with current element to empty tree");
      else
         Ada.Text_IO.Put_Line ("Copy fails for bushy tree with current element to empty tree");
      end if;

      Ada.Text_IO.New_Line;

   end Test_Copy;

   ----------------------------------------------------------------------------
   procedure Test_Ancestors is
   -- Assumes that Insert, Clear, and Traverse work correctly

      ---------------------------------------
      procedure Display_One_Num (Element : in Element_Rec) is
      begin
         Ada.Integer_Text_IO.Put (Item  => Element.Key,
                                  Width => 4);
      end Display_One_Num;

      Tree : My_Tree.Tree_Type;
   begin
      Ada.Text_IO.Put_Line ("Ancestors of an empty tree (list should be empty)");
      My_Tree.Ancestors (Tree => Tree,                         Key  => 25,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of tree with just a root (list should be empty)");
      My_Tree.Insert (Tree => Tree,
                      Item => (Key => 100,  Data => 'A'));
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 100,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of tree with just a root (list should contain 100)");
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 50,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of tree with just a root (list should contain 100)");
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 150,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of right stick (list should contain evens 28 thru 20)");
      My_Tree.Clear (Tree);
      for Count in 10 .. 15 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;

      My_Tree.Ancestors (Tree => Tree,
                         Key  => 2 * 15,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------
      Ada.Text_IO.Put_Line ("Ancestors of right stick (list should contain evens 28 thru 20)");
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 2 * 14 - 1,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of left stick (list should contain evens 22 thru 30)");
      My_Tree.Clear (Tree);
      for Count in  reverse 10 .. 15 loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => 2 * Count, Data => 'B'));
      end loop;

      My_Tree.Ancestors (Tree => Tree,
                         Key  => 2 * 10,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);

      ------------------
      Ada.Text_IO.Put_Line ("Ancestors of right stick (list should contain evens 22 thru 30)");
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 2 * 11 + 1,
                         Process => Display_One_Num'Access);
      Ada.Text_IO.New_Line (2);


      ------------------

      Ada.Text_IO.Put_Line ("Ancestors of a bushy tree (list should 125 150, 100)");
      My_Tree.Clear (Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Data (Count), Data => 'A'));
      end loop;
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 115,
                         Process => Display_One_Num'Access);

      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Ancestors of a bushy tree (list should 115, 125 150, 100)");
      My_Tree.Clear (Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Data (Count), Data => 'A'));
      end loop;
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 117,
                         Process => Display_One_Num'Access);

      Ada.Text_IO.New_Line (2);

      Ada.Text_IO.Put_Line ("Ancestors of a bushy tree (list should 65, 75 50, 100)");
      My_Tree.Clear (Tree);
      for Count in Data'Range loop
         My_Tree.Insert (Tree => Tree,
                         Item => (Key => Data (Count), Data => 'A'));
      end loop;
      My_Tree.Ancestors (Tree => Tree,
                         Key  => 67,
                         Process => Display_One_Num'Access);

      Ada.Text_IO.New_Line (2);
   end Test_Ancestors;


begin
   Test_Height;
   Ada.Text_IO.Put_Line ("------------------------------------------------------");
   Test_Same_Shape;
   Ada.Text_IO.Put_Line ("------------------------------------------------------");
   Test_Leaf_Count;
   Ada.Text_IO.Put_Line ("------------------------------------------------------");
   Test_Copy;
   Ada.Text_IO.Put_Line ("------------------------------------------------------");
   Test_Ancestors;
   Ada.Text_IO.Put_Line ("------------------------------------------------------");
   Ada.Text_IO.Put_Line ("All done");
end Test_New_Ops;