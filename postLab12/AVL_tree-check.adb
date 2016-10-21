with Ada.Text_IO;
package body AVL_Tree.Check is


   function Balance_OK (Root : in  Node_Ptr) return Boolean is

   -- This function analyzes the balance of all nodes in Tree.
   -- It returns True if no balance problems are found and False if
   -- a problem is found.

   -- This function displays an error message if it finds a node that
   -- 1)  Has subtree heights that differ by more than one
   -- 2)  Has the incorrect value of Balance for the subtree heights

      ---------------------------------
      function Height (Root : in Node_Ptr) return Natural is
      begin
         if Root = null then
            return 0;
         else return 1 + Natural'Max (Height (Root.all.Left),
                                      Height (Root.all.Right));
         end if;
      end Height;

      ---------------------------------
      Left_Height  : Natural;
      Right_Height : Natural;
   begin
      if Root = null then
         return True;
      else
         Left_Height  := Height (Root.all.Left);
         Right_Height := Height (Root.all.Right);
         if abs (Left_Height - Right_Height) > 1 then
            Ada.Text_IO.Put_Line ("ERROR: The following node's subtrees violate the AVL height condition");
            Put (Root.all.Info);
            return False;
         else
            if (Left_Height > Right_Height and Root.all.Balance /= Left_Heavy)  or
               (Left_Height < Right_Height and Root.all.Balance /= Right_Heavy) or
               (Left_Height = Right_Height and Root.all.Balance /= In_Balance)  then
               Ada.Text_IO.Put_Line ("ERROR: The following node has an incorrect balance factor");
               Put (Root.all.Info);
               return False;
            else
               return Balance_OK (Root.all.Left) and then
                      Balance_OK (Root.all.Right);
            end if;
         end if;
      end if;
   end Balance_OK;

   -------------------------------------------------------------------------------------
   function Balance_OK (Tree : in Tree_Type) return Boolean is
   begin
      return Balance_OK (Tree.Root);  -- Call recursive function
   end Balance_OK;


   --------------------------------------------------------------------------------------
   function Order_OK (Tree : in Tree_Type) return Boolean is

   -- This function analyzes the order of all nodes in Tree.
   -- It returns True if no order problems are found and False if
   -- a problem is found.

   -- This function displays an error message if it finds a node that
   -- 1)  Has left subtree with a key >= to the key of the node
   -- 2)  Has a right subtree with a key <= to the key of the node

      type Element_Array is array (Positive range <>) of Element_Type;
      Elements : Element_Array (1 .. Tree.Size);
      Index    : Natural := 0;

      ---------------------------------------------
      procedure Traverse (Root : in Node_Ptr) is
      -- Do an inorder traversal adding each node to the array Elements
      begin
         if Root /= null then
            Traverse (Root.all.Left);
            Index := Index + 1;
            Elements (Index) := Root.all.Info;
            Traverse (Root.all.Right);
         end if;
      end Traverse;

   -------------------
   begin
      Traverse (Tree.Root);
      for Index in Elements'First .. Elements'Last - 1 loop
         if not (Key_Of (Elements (Index)) < Key_Of (Elements (Index + 1))) then
            Ada.Text_IO.Put_Line ("ERROR: The nodes in the AVL tree are out of order");
            return False;
         end if;
      end loop;
      return True;
   end Order_OK;

   --------------------------------------------------------------------------------------
   procedure Check_Validity (Tree : in Tree_Type) is
   begin
      if Balance_OK (Tree) and Order_OK (Tree) then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("AVL tree has valid order and balance factors");
         Ada.Text_IO.New_Line;
      end if;
   end Check_Validity;

end AVL_Tree.Check;
