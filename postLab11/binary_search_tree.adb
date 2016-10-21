with Ada.Unchecked_Deallocation;
package body Binary_Search_Tree is

   -- Completed by   Vincent T. Mossman
   -- Date           April 17, 2014

   -- Instantiate procedure for recycling node memory
   procedure Free is new Ada.Unchecked_Deallocation (Object => Node_Type,
                                                     Name   => Node_Ptr);
   -- New Operations for the Postlab

   -----------------------------------------------------------------------------
   function Height (Tree : in Tree_Type) return Natural is
      function Height (Root : in Node_Ptr) return Natural is
      begin
         if Root = null then
            return 0;
         else
            return 1 + Natural'Max (Height (Root => Root.all.Left),
                                    Height (Root => Root.all.Right));
         end if;
      end Height;
   begin
      return Height (Root => Tree.Root);
   end Height;

   ------------------------------------------------------------------------------
   procedure Ancestors
             (Tree    : in Tree_Type;
              Key     : in Key_Type;
              Process : not null access procedure (Element : in Element_Type)) is
      procedure Ancestors (Root : in Node_Ptr) is
      begin
         if Root = null or else Key_Of (Root.all.Info) = Key then
            null;
         else
            if Key < Key_Of (Root.all.Info) then
               Ancestors (Root => Root.all.Left);
            else
               Ancestors (Root => Root.all.Right);
            end if;
            Process (Element => Root.all.Info);
         end if;
      end Ancestors;
   begin
      Ancestors (Root => Tree.Root);
   end Ancestors;

   -----------------------------------------------------------------------------
   function Leaf_Count (Tree : in Tree_Type) return Natural is
      function Leaf_Count (Root : in Node_Ptr) return Natural is
      begin
         if Root = null then
            return 0;
         elsif Root.all.Left = null and Root.all.Right = null then
            return 1;
         else
            return Leaf_Count (Root => Root.all.Left) +
                   Leaf_Count (Root => Root.all.Right);
         end if;
      end Leaf_Count;
   begin
      return Leaf_Count (Tree.Root);
   end Leaf_Count;

   -----------------------------------------------------------------------------
   procedure Copy (Source      : in  Tree_Type;
                   Destination : out Tree_Type) is
      function Copy (Root : in Node_Ptr) return Node_Ptr is
      begin
         if Root = null then
            return null;
         else
            return new Node_Type'(Info  => Root.all.Info,
                                  Left  => Copy (Root => Root.all.Left),
                                  Right => Copy (Root => Root.all.Right));
         end if;
      end Copy;
   begin
      Destination.Root    := Copy (Source.Root);
      Destination.Current := null;
   end Copy;

   -----------------------------------------------------------------------------
   function Same_Shape (Tree_1 : in Tree_Type;
                        Tree_2 : in Tree_Type) return Boolean is
      function Same_Shape (Root_1 : in Node_Ptr;
                           Root_2 : in Node_Ptr) return Boolean is
      begin
         if Root_1 = null and Root_2 = null then
            return True;
         elsif (Root_1 = null and Root_2 /= null) or
               (Root_1 /= null and Root_2 = null) then
            return False;
         else
            return Same_Shape (Root_1 => Root_1.all.Left,
                               Root_2 => Root_2.all.Left) and
                   Same_Shape (Root_1 => Root_1.all.Right,
                               Root_2 => Root_2.all.Right);
         end if;
      end Same_Shape;
   begin
      return Same_Shape (Root_1 => Tree_1.Root,
                         Root_2 => Tree_2.Root);
   end Same_Shape;

   ----------------------------------------------------------------------------
   procedure Clear (Tree : in out Tree_Type) is
      procedure Recursive_Clear (Root : in out Node_Ptr) is
      begin
         if Root /= null then
            Recursive_Clear (Root.all.Left);
            Recursive_Clear (Root.all.Right);
            Free (Root);
         end if;
      end Recursive_Clear;
   begin
      Recursive_Clear (Tree.Root);  -- Recycle all nodes in Tree
      Tree.Current := null;         -- Current now undefined
   end Clear;

   ----------------------------------------------------------------------------
   procedure Insert (Tree : in out Tree_Type;
                     Item : in     Element_Type) is
      procedure Recursive_Insert (Root : in out Node_Ptr;
                                  Key  : in     Key_Type) is
      begin
         if Root = null then
            -- Base case, inserting into an empty tree
            Root := new Node_Type'(Info => Item, Left => null, Right => null);
            Tree.Current := Root;  -- Inserted element is the current element
         elsif Key = Key_Of (Root.all.Info) then
            -- Base case, key already in tree
            raise DUPLICATE_KEY;
         elsif Key < Key_Of (Root.all.Info) then
            -- General case:  Insert into left subtree
            Recursive_Insert (Root => Root.all.Left, Key => Key);
         else
            -- General case:  Insert into right subtree
            Recursive_Insert (Root => Root.all.Right, Key => Key);
         end if;
      end Recursive_Insert;
   begin
      Recursive_Insert (Root => Tree.Root, Key => Key_Of (Item));
   end Insert;

   -----------------------------------------------------------------------------
   procedure Find_And_Unlink_Max (Root    : in out Node_Ptr;
                                  Max_Ptr :    out Node_Ptr) is
   -- Purpose        : Finds and unlinks the node with the maximum key
   --                  from a tree whose Root is given
   -- Preconditions  : Root is not null
   -- Postconditions : Max_Ptr designates the node containing the
   --                     largest key in the tree rooted at Root
   --                  The node designated by Max_Ptr is unlinked
   --                     from the tree rooted at Root
   begin
      if Root.all.Right = null then     -- Is there a right child?
         -- Base case, root contains the maximum key in Tree
         Max_Ptr := Root;               -- Return pointer to it
         Root    := Root.all.Left;      -- Unlink it from Tree
      else
         -- General case, keep looking in the right subtree
         Find_And_Unlink_Max (Root => Root.all.Right, Max_Ptr => Max_Ptr);
      end if;
   end Find_And_Unlink_Max;

   -----------------------------------------------------------------------------
   procedure Delete_Root (Root : in out Node_Ptr) is
   -- Purpose       : Delete the root node from a tree
   -- Preconditions : Root is not null
   -- Postconditions: The node designated by Root is deleted.
   --                 Tree remains a binary search tree

      To_Recycle : Node_Ptr;  -- For recycling nodes
      Pred_Ptr   : Node_Ptr;  -- Designates the root's logical predecessor
   begin
      if Root.all.Left = null and Root.all.Right = null then
         -- Root node has no children
         Free (Root);                        -- Entire tree now empty
      elsif Root.all.Left = null then
         -- Root node has only a right child
         To_Recycle := Root;                 -- Save for later deallocation
         Root := Root.all.Right;             -- Unlink the root node
         Free (To_Recycle);                  -- Deallocate former root node
      elsif Root.all.Right = null  then
         -- Root node has a left child
         To_Recycle := Root;                 -- Save for later deallocation
         Root := Root.all.Left;              -- Unlink the root node
         Free (To_Recycle);                  -- Deallocate former root node
      else  -- Root node has two children
         -- Find and unlink the logical predecessor
         Find_And_Unlink_Max (Root    => Root.all.Left,
                              Max_Ptr => Pred_Ptr);
         Root.all.Info := Pred_Ptr.all.Info; -- Copy Info from predecessor
         Free (Pred_Ptr);                    -- Deallocate predecessor
      end if;
   end Delete_Root;

   ----------------------------------------------------------------------------
   procedure Delete (Tree : in out Tree_Type;
                     Key  : in      Key_Type) is
      procedure Recursive_Delete (Root : in out Node_Ptr;
                                  Key  : in     Key_Type) is
      begin
         if Root = null then                                    -- Base case
            raise KEY_ERROR;
         elsif Key = Key_Of (Root.all.Info) then                -- Base case
            Delete_Root (Root);
         elsif Key < Key_Of (Root.all.Info) then                -- General case
            Recursive_Delete (Root => Root.all.Left,  Key => Key);
         else                                                   -- General case
            Recursive_Delete (Root => Root.all.Right, Key => Key);
         end if;
      end Recursive_Delete;
   begin
      if Tree.Current /= null and then Key = Key_Of (Tree.Current.all.Info) then
         Tree.Current := null;
      end if;
      Recursive_Delete (Root => Tree.Root, Key => Key);
   end Delete;

   ----------------------------------------------------------------------------
   procedure Modify (Tree    : in out Tree_Type;
                     Element : in     Element_Type) is
   begin
      if Tree.Current = null then
         raise CURRENT_UNDEFINED;
      end if;
      Tree.Current.all.Info := Element;
   end Modify;

   ----------------------------------------------------------------------------
   procedure Find (Tree : in out Tree_Type;
                   Key  : in      Key_Type) is
      function Recursive_Find (Root : in Node_Ptr;
                               Key  : in Key_Type) return Node_Ptr is
      begin
         if Root = null or else Key = Key_Of (Root.all.Info) then
            return Root;  -- Two base cases (not found and found in root)
         elsif Key < Key_Of (Root.all.Info) then
            -- General case (search Left subtree)
            return Recursive_Find (Root => Root.all.Left, Key => Key);
         else
            -- General case (search Right subtree)
            return Recursive_Find (Root => Root.all.Right, Key => Key);
         end if;
      end Recursive_Find;
   begin
      Tree.Current := Recursive_Find (Tree.Root, Key);
   end Find;

   ----------------------------------------------------------------------------
   procedure Balance (Tree : in out Tree_Type) is
      type Element_Array is array (Positive range <>) of Element_Type;
      -- Declare an array the exact size of the tree
      Sorted_Elements : Element_Array (1 .. Tree.Size);
      Index : Natural := 0;

      --------------------------------------------------------
      procedure Store_One_Element (Element : in out Element_Type) is
      begin
         Index := Index + 1;
         Sorted_Elements (Index) := Element;
      end Store_One_Element;

      --------------------------------------------------------
      function Array_To_Tree (Elements : in Element_Array) return Node_Ptr is
         Middle : Positive;  -- The middle element in the array Elements
      begin
         if Elements'Length = 0 then
            return null;     -- Base case, no elements is an empty tree
         else
            Middle := (Elements'First + Elements'Last) / 2;
            -- Return a tree with the middle element as the root, the elements
            -- before the middle as the left subtree, and the elements after
            -- the middle as the right subtree
            return new Node_Type'
                 (Info => Elements (Middle),
                  Left  => Array_To_Tree (Elements (Elements'First .. Middle - 1)),
                  Right => Array_To_Tree (Elements (Middle + 1 .. Elements'Last)));
         end if;
      end Array_To_Tree;

   begin -- Balance
      -- Copy the Tree into a sorted array of elements
      Tree.Traverse (Order   => Inorder,
                     Process => Store_One_Element'Access);
      Tree.Clear;
      Tree.Root    := Array_To_Tree (Sorted_Elements);
      Tree.Current := null;
   end Balance;

   ----------------------------------------------------------------------------
   function Current_Defined (Tree : in Tree_Type) return Boolean is
   begin
      return Tree.Current /= null;
   end Current_Defined;

   ----------------------------------------------------------------------------
   function Empty (Tree : in Tree_Type) return Boolean is
   begin
      return Tree.Root = null;
   end Empty;

   ----------------------------------------------------------------------------
   function Full (Tree : in Tree_Type) return Boolean is
   pragma Warnings (Off, Tree);
   begin
      return False;
   end Full;

   ----------------------------------------------------------------------------
   function Size (Tree : in Tree_Type) return Natural is
      function Recursive_Size (Root : in Node_Ptr) return Natural is
      begin
         if Root = null then
            return 0;
         else
            return 1 + Recursive_Size (Root.all.Left)
                     + Recursive_Size (Root.all.Right);
         end if;
      end Recursive_Size;
   begin
      return Recursive_Size (Tree.Root);
   end Size;

   ----------------------------------------------------------------------------
   procedure Retrieve (Tree    : in     Tree_Type;
                       Element :    out Element_Type) is
   begin
      if Tree.Current = null then
         raise CURRENT_UNDEFINED;
      end if;
      Element := Tree.Current.all.Info;
   end Retrieve;

   ----------------------------------------------------------------------------
   procedure Traverse
       (Tree    : in out Tree_Type;
        Order   : in Traversal_Order;
        Process : not null access procedure (Element : in out Element_Type)) is

      ----------------------------------------
      procedure Reverse_Inorder (Root : in Node_Ptr) is
      begin
         if Root /= null then
            Reverse_Inorder (Root => Root.all.Right); -- Traverse Right subtree
            Process (Root.all.Info);                  -- Process the root
            Reverse_Inorder (Root => Root.all.Left);  -- Traverse Left subtree
         end if;
      end Reverse_Inorder;


      ----------------------------------------
      procedure Inorder (Root : in Node_Ptr) is
      begin
         if Root /= null then
            Inorder (Root => Root.all.Left);      -- Traverse Left subtree
            Process (Root.all.Info);              -- Process the root
            Inorder (Root => Root.all.Right);     -- Traverse Right subtree
         end if;
      end Inorder;

      ----------------------------------------
      procedure Preorder (Root : in Node_Ptr) is
      begin
         if Root /= null then
            Process (Root.all.Info);              -- Process the root
            Preorder (Root => Root.all.Left);     -- Traverse Left subtree
            Preorder (Root => Root.all.Right);    -- Traverse Right subtree
         end if;
      end Preorder;

      ----------------------------------------
      procedure Postorder (Root : in Node_Ptr) is
      begin
         if Root /= null then
            Postorder (Root => Root.all.Left);    -- Traverse Left subtree
            Postorder (Root => Root.all.Right);   -- Traverse Right subtree
            Process (Root.all.Info);              -- Process the root
         end if;
      end Postorder;

   begin
      case Order is
         when Inorder   => Inorder (Tree.Root);
         when Preorder  => Preorder (Tree.Root);
         when Postorder => Postorder (Tree.Root);
         when Reverse_Inorder   => Reverse_Inorder (Tree.Root);
      end case;
   end Traverse;

end Binary_Search_Tree;