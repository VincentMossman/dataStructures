with Ada.Unchecked_Deallocation;
with Ada.Text_IO;
package body AVL_Tree is

   -- Completed by   Vincent T. Mossman
   -- Date           May 1, 2014

   -- Instantiate procedure for recycling node memory
   procedure Free is new Ada.Unchecked_Deallocation (Object => Node_Type,
                                                     Name   => Node_Ptr);

   -- The four possible AVL rotations
   ----------------------------------

   ----------------------------------------------------------------------------
   procedure Single_Right_Rotation (Root : in out Node_Ptr) is
   -- Performs an AVL single right rotation
   -- Preconditions  : The root node violates the AVL height condition
   --                     (The height of its left subtree is 2 greater
   --                      than the height of its right subtree)
   --                  The left child of Root is Left_Heavy or In_Balance
   -- Postconditions : Root designates a valid AVL tree
   --                  If the new Root is In_Balance the
   --                     the height of the tree was reduced by 1
   --                  Else
   --                     the height of the tree remains the same

      Left_Child : Node_Ptr;  -- Pointer to the left child of Root
   begin
      Left_Child := Root.all.Left;

      -- GNAT assertion pragmas allow us to test preconditions and halt the
      -- program when a precondition is not met
      pragma Assert ((Root.all.Balance       = Left_Heavy) and
                     (Left_Child.all.Balance = Left_Heavy or
                      Left_Child.all.Balance = In_Balance),
                     "Single_Right_Rotation precondition violated");

      -- Set the original root's left pointer to its left child's right subtree
      Root.all.Left := Left_Child.all.Right;
      -- Set the original left child's right pointer to the original root
      Left_Child.all.Right := Root;

      -- Set the balances
      if Left_Child.all.Balance = In_Balance then
         Left_Child.all.Balance := Right_Heavy;
         Root.all.Balance       := Left_Heavy;
      else
         Left_Child.all.Balance := In_Balance;
         Root.all.Balance       := In_Balance;
      end if;

      -- Make the left child the new root
      Root := Left_Child;
   end Single_Right_Rotation;

   ----------------------------------------------------------------------------
   procedure Single_Left_Rotation (Root : in out Node_Ptr) is
   -- Performs an AVL single left rotation
   -- Preconditions  : The root node violates the AVL height condition
   --                     (The height of its right subtree is 2 greater
   --                      than the height of its left subtree)
   --                  The right child of Root is Right_Heavy
   -- Postconditions : Root designates a valid AVL tree
   --                  The height of the tree is reduced by 1

      Right_Child : Node_Ptr;  -- Pointer to the right child of Root
   begin
      Right_Child := Root.all.Right;

      -- GNAT assertion pragmas allow us to test preconditions and halt the
      -- program when a precondition is not met
      pragma Assert ((Root.all.Balance        = Right_Heavy) and
                     (Right_Child.all.Balance = Right_Heavy or
                      Right_Child.all.Balance = In_Balance),
                     "Single_Left_Rotation precondition violated");

      -- Set the original root's right pointer to its right child's left subtree
      Root.all.Right := Right_Child.all.Left;
      -- Set the original right child's left pointer to the original root
      Right_Child.all.Left := Root;

      -- Set the balances
      if Right_Child.all.Balance = In_Balance then
         Right_Child.all.Balance := Left_Heavy;
         Root.all.Balance        := Right_Heavy;
      else
         Right_Child.all.Balance := In_Balance;
         Root.all.Balance        := In_Balance;
      end if;

      -- Make the right child the new root
      Root := Right_Child;
   end Single_Left_Rotation;

   ----------------------------------------------------------------------------
   procedure Double_Left_Right_Rotation (Root : in out Node_Ptr) is
   -- Performs an AVL double left right rotation
   -- Preconditions  : The root node violates the AVL height condition
   --                     (The height of its left subtree is 2 greater
   --                      than the height of its right subtree)
   --                  The left child of Root is Right_Heavy
   -- Postconditions : Root designates a valid AVL tree
   --                  If the Right_Grandchild is In_Balance then
   --                     The height of the tree is not changed
   --                  Else
   --                     The height of the tree is reduced by 1

      Left_Child       : Node_Ptr;  -- Pointer to the right child of Root
      Right_Grandchild : Node_Ptr;  -- Pointer to the left child of Right_Child
   begin
      Left_Child       := Root.all.Left;
      Right_Grandchild := Left_Child.all.Right;

      -- GNAT assertion pragmas allow us to test preconditions and halt the
      -- program when a precondition is not met
      pragma Assert (Root.all.Balance       = Left_Heavy and
                     Left_Child.all.Balance = Right_Heavy,
                     "Double_Left_Right_Rotation precondition violated");

      -- Do a left rotation about the left child
      Left_Child.all.Right      := Right_Grandchild.all.Left;
      Right_Grandchild.all.Left := Left_Child;
      Root.all.Left             := Right_Grandchild;

      -- Do a right rotation about the root
      Root.all.Left := Right_Grandchild.all.Right;
      Right_Grandchild.all.Right := Root;

      -- Reset balances
      case Right_Grandchild.all.Balance is

         when In_Balance =>
            Left_Child.all.Balance       := In_Balance;
            Right_Grandchild.all.Balance := In_Balance;
            Root.all.Balance             := In_Balance;

         when Right_Heavy =>
            Left_Child.all.Balance       := Left_Heavy;
            Right_Grandchild.all.Balance := In_Balance;
            Root.all.Balance             := In_Balance;

         when Left_Heavy =>
            Left_Child.all.Balance       := In_Balance;
            Right_Grandchild.all.Balance := In_Balance;
            Root.all.Balance             := Right_Heavy;
      end case;

      -- Set the new Root
      Root := Right_Grandchild;
   end Double_Left_Right_Rotation;

   ----------------------------------------------------------------------------
   ----------------------------------------------------------------------------
   procedure Double_Right_Left_Rotation (Root : in out Node_Ptr) is
   -- Performs an AVL double right left rotation
   -- Preconditions  : The root node violates the AVL height condition
   --                     (The height of its right subtree is 2 greater
   --                      than the height of its left subtree)
   --                  The right child of Root is Left_Heavy
   -- Postconditions : Root designates a valid AVL tree
   --                  The height of the tree is reduced by 1

      Right_Child     : Node_Ptr;  -- Pointer to the right child of Root
      Left_Grandchild : Node_Ptr;  -- Pointer to the left child of Right_Child
   begin
      Right_Child     := Root.all.Right;
      Left_Grandchild := Right_Child.all.Left;

      -- GNAT assertion pragmas allow us to test preconditions and halt the
      -- program when a precondition is not met
      pragma Assert (Root.all.Balance        = Right_Heavy and
                     Right_Child.all.Balance = Left_Heavy,
                     "Double_Right_Left_Rotation precondition violated");


      -- Do a right rotation about the right child
      Right_Child.all.Left      := Left_Grandchild.all.Right;
      Left_Grandchild.all.Right := Right_Child;
      Root.all.Right            := Left_Grandchild;

      -- Do a left rotation about the root
      Root.all.Right := Left_Grandchild.all.Left;
      Left_Grandchild.all.Left := Root;

      -- Reset balances
      case Left_Grandchild.all.Balance is

         when In_Balance =>
            Right_Child.all.Balance     := In_Balance;
            Left_Grandchild.all.Balance := In_Balance;
            Root.all.Balance            := In_Balance;

         when Left_Heavy =>
            Right_Child.all.Balance     := Right_Heavy;
            Left_Grandchild.all.Balance := In_Balance;
            Root.all.Balance            := In_Balance;

         when Right_Heavy =>
            Right_Child.all.Balance     := In_Balance;
            Left_Grandchild.all.Balance := In_Balance;
            Root.all.Balance            := Left_Heavy;
      end case;

      -- Set the new Root
      Root := Left_Grandchild;
   end Double_Right_Left_Rotation;



   -- Public Operations defined in the package specification
   ---------------------------------------------------------

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

      procedure Recursive_Insert (Root             : in out Node_Ptr;
                                  Key              : in     Key_Type;
                                  Height_Increased :    out Boolean) is
      begin
         if Root = null then
            -- Base case, inserting into an empty tree
            Root := new Node_Type'(Info    => Item,
                                   Balance => In_Balance,
                                   Left    => null,
                                   Right   => null);
            Tree.Current := Root;  -- Inserted element is the current element
            -- Adding a node to an empty tree increases its height
            Height_Increased := True;

         elsif Key = Key_Of (Root.all.Info) then
            -- Base case, key already in tree
            raise DUPLICATE_KEY;

         elsif Key < Key_Of (Root.all.Info) then
            -- General case:  Insert into left subtree
            Recursive_Insert (Root             => Root.all.Left,
                              Key              => Key,
                              Height_Increased => Height_Increased);

            -- If the insert increased the height of the left subtree,
            -- we may need to rebalance the tree around this node
            if Height_Increased then
               case Root.all.Balance is
                  when Left_Heavy =>
                     -- Adding the new node changed this node's balance so
                     -- that it now violates the AVL condition

                     -- Determine what kind of rotation to make
                     if Root.all.Left.all.Balance = Left_Heavy then
                        -- Root and left child are both Left_Heavy
                        Single_Right_Rotation (Root);
                     else
                        -- Root is Left_Heavy and left child is Right_Heavy
                        Double_Left_Right_Rotation (Root);
                     end if;
                     Height_Increased := False;
                  when In_Balance =>
                     -- Adding the new node changed this node's balance
                     -- but it still satisfies AVL condition.
                     -- Height_Increased remains True.
                     Root.all.Balance := Left_Heavy;
                  when Right_Heavy =>
                     -- Adding the new node, balanced this node
                     Root.all.Balance := In_Balance;
                     Height_Increased := False;
               end case;
            end if;

         else
            -- General case:  Insert into right subtree
            Recursive_Insert (Root             => Root.all.Right,
                              Key              => Key,
                              Height_Increased => Height_Increased);

            -- If the insert increased the height of the right subtree,
            -- we may need to rebalance the tree around this node
            if Height_Increased then
               case Root.all.Balance is
                  when Left_Heavy =>
                     -- Adding the new node, balanced this node
                     Root.all.Balance := In_Balance;
                     Height_Increased := False;
                  when In_Balance =>
                     -- Adding the new node changed this node's balance
                     -- but it still satisfies AVL condition.
                     -- Height_Increased remains True.
                     Root.all.Balance := Right_Heavy;
                  when Right_Heavy =>
                     -- Adding the new node changed this node's balance so
                     -- that it now violates the AVL condition

                     -- Determine what kind of rotation to make
                     if Root.all.Right.all.Balance = Right_Heavy then
                        -- Root and right child are both Right_Heavy
                        Single_Left_Rotation (Root);
                     else
                        -- Root is Right_Heavy and right child is Left_Heavy
                        Double_Right_Left_Rotation (Root);
                     end if;
                     Height_Increased := False;
               end case;
            end if;
         end if;
      end Recursive_Insert;

   ------------------------------
      Increased_Height : Boolean; -- Did recursive insert increase the tree's height?
   begin
      Recursive_Insert (Root             => Tree.Root,
                        Key              => Key_Of (Item),
                        Height_Increased => Increased_Height);
   end Insert;

   -----------------------------------------------------------------------------
   procedure Find_And_Unlink_Max (Root          : in out Node_Ptr;
                                  Max_Ptr       :    out Node_Ptr;
                                  Height_Reduce :    out Boolean) is
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
         Height_Reduce := True;
      else
         -- General case, keep looking in the right subtree
         Find_And_Unlink_Max (Root => Root.all.Right, Max_Ptr => Max_Ptr, Height_Reduce => Height_Reduce);
         -- Rebalance tree, if necessary
         if Height_Reduce then
            case Root.all.Balance is
               when Left_Heavy =>
                  if Root.all.Left.all.Balance = Left_Heavy then
                     Single_Right_Rotation (Root);
                  elsif Root.all.Left.all.Balance = In_Balance then
                     Single_Right_Rotation (Root);
                     Height_Reduce := False;
                  else
                     Double_Left_Right_Rotation (Root);
                  end if;
               when In_Balance =>
                  Root.all.Balance := Left_Heavy;
                  Height_Reduce := False;
               when Right_Heavy =>
                  Root.all.Balance := In_Balance;
            end case;
         end if;
      end if;
   end Find_And_Unlink_Max;

   -----------------------------------------------------------------------------
   procedure Delete_Root (Root          : in out Node_Ptr;
                          Height_Reduce :    out Boolean) is
   -- Purpose       : Delete the root node from a tree
   -- Preconditions : Root is not null
   -- Postconditions: The node designated by Root is deleted.
   --                 Tree remains a binary search tree

      To_Recycle    : Node_Ptr;  -- For recycling nodes
      Pred_Ptr      : Node_Ptr;  -- Designates the root's logical predecessor
   begin
      if Root.all.Left = null and Root.all.Right = null then
         -- Root node has no children
         Free (Root);                        -- Entire tree now empty
         Height_Reduce := True;
      elsif Root.all.Left = null then
         -- Root node has only a right child
         To_Recycle := Root;                 -- Save for later deallocation
         Root := Root.all.Right;             -- Unlink the root node
         Free (To_Recycle);                  -- Deallocate former root node
         Height_Reduce := True;
      elsif Root.all.Right = null  then
         -- Root node has a left child
         To_Recycle := Root;                 -- Save for later deallocation
         Root := Root.all.Left;              -- Unlink the root node
         Free (To_Recycle);                  -- Deallocate former root node
         Height_Reduce := True;
      else  -- Root node has two children
         -- Find and unlink the logical predecessor
         Find_And_Unlink_Max (Root    => Root.all.Left,
                              Max_Ptr => Pred_Ptr,
                              Height_Reduce => Height_Reduce);
         Root.all.Info := Pred_Ptr.all.Info; -- Copy Info from predecessor
         Free (Pred_Ptr);                    -- Deallocate predecessor
         -- Rebalance tree, if necessary
         if Height_Reduce then
            case Root.all.Balance is
               when Left_Heavy =>
                  Root.all.Balance := In_Balance;
               when In_Balance =>
                  Root.all.Balance := Right_Heavy;
                  Height_Reduce := False;
               when Right_Heavy =>
                  if Root.all.Right.all.Balance = Right_Heavy then
                     Single_Left_Rotation (Root);
                  elsif Root.all.Right.all.Balance = In_Balance then
                     Single_Left_Rotation (Root);
                     Height_Reduce := False;
                  else
                     Double_Right_Left_Rotation (Root);
                  end if;
            end case;
         end if;
      end if;
   end Delete_Root;

   ----------------------------------------------------------------------------
   procedure Delete (Tree : in out Tree_Type;
                     Key  : in      Key_Type) is
      procedure Recursive_Delete (Root          : in out Node_Ptr;
                                  Key           : in     Key_Type;
                                  Height_Reduce :    out Boolean) is
      begin
         if Root = null then                                    -- Base case
            raise KEY_ERROR;
         elsif Key = Key_Of (Root.all.Info) then                -- Base case
            Delete_Root (Root => Root,
                         Height_Reduce => Height_Reduce);
         elsif Key < Key_Of (Root.all.Info) then                -- General case
            Recursive_Delete (Root => Root.all.Left,  Key => Key, Height_Reduce => Height_Reduce);
            -- Rebalance tree, if necessary
            if Height_Reduce then
               case Root.all.Balance is
                  when Left_Heavy =>
                     Root.all.Balance := In_Balance;
                  when In_Balance =>
                     Root.all.Balance := Right_Heavy;
                     Height_Reduce := False;
                  when Right_Heavy =>
                     if Root.all.Right.all.Balance = Right_Heavy then
                        Single_Left_Rotation (Root);
                     elsif Root.all.Right.all.Balance = In_Balance then
                        Single_Left_Rotation (Root);
                        Height_Reduce := False;
                     else
                        Double_Right_Left_Rotation (Root);
                     end if;
               end case;
            end if;
         else                                                   -- General case
            Recursive_Delete (Root => Root.all.Right, Key => Key, Height_Reduce => Height_Reduce);
            if Height_Reduce then
               case Root.all.Balance is
                  when Left_Heavy =>
                     if Root.all.Left.all.Balance = Left_Heavy then
                        Single_Right_Rotation (Root);
                     elsif Root.all.Left.all.Balance = In_Balance then
                        Single_Right_Rotation (Root);
                        Height_Reduce := False;
                     else
                        Double_Left_Right_Rotation (Root);
                     end if;
                  when In_Balance =>
                     Root.all.Balance := Left_Heavy;
                     Height_Reduce := False;
                  when Right_Heavy =>
                     Root.all.Balance := In_Balance;
               end case;
            end if;
         end if;
      end Recursive_Delete;

      Height_Reduce : Boolean;

   begin
      Tree.Current := null;  -- The current element is no longer defined
      Recursive_Delete (Root => Tree.Root, Key => Key, Height_Reduce => Height_Reduce);
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
   function Height (Tree : in Tree_Type) return Natural is
      function Recursive_Height (Root : in Node_Ptr) return Natural is
      begin
         if Root = null then
            return 0;
         else return 1 + Natural'Max (Recursive_Height (Root.all.Left),
                                      Recursive_Height (Root.all.Right));
         end if;
      end Recursive_Height;
   begin
      return Recursive_Height (Root => Tree.Root);
   end Height;

   ----------------------------------------------------------------------------
   procedure Traverse
       (Tree    : in out Tree_Type;
        Order   : in Traversal_Order;
        Process : not null access procedure (Element : in out Element_Type)) is


      ----------------------------------------
      procedure Inorder (Root : in Node_Ptr) is
      begin
         if Root /= null then
            Inorder (Root => Root.all.Left);      -- Traverse Left subtree
            Process (Root.all.Info);              -- Process the root
            case Root.all.Balance is
               when Left_Heavy =>
                  Ada.Text_IO.Put ('/');
               when In_Balance =>
                  Ada.Text_IO.Put ('=');
               when Right_Heavy =>
                  Ada.Text_IO.Put ('\');
            end case;
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
      end case;
   end Traverse;

end AVL_Tree;