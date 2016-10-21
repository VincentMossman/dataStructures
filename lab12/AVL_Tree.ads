with Ada.Finalization;
generic

   type Element_Type is private;       -- The type of element in the list
   type Key_Type is limited private;   -- The type of key in the element

   -- The user must supply a function that returns the Key of an Element
   with function Key_Of (Element : in Element_Type) return Key_Type;
   -- The user must supply functions for comparing Keys
   with function "=" (Left : in Key_Type; Right : in Key_Type) return Boolean;
   with function "<" (Left : in Key_Type; Right : in Key_Type) return Boolean;

package AVL_Tree is

-- This package implements an AVL tree.
-- Each element in the tree is identified by a unique key.

-- The tree class includes the notion of a current element in the tree.
-- The retrieve and modify operations are performed on the current node.

   type Tree_Type is new Ada.Finalization.Limited_Controlled with private;

   type Traversal_Order is (Inorder, Preorder, Postorder);

   DUPLICATE_KEY     : exception;
   KEY_ERROR         : exception;
   OVERFLOW          : exception;
   CURRENT_UNDEFINED : exception;

   -- Transformers

   ----------------------------------------------------------------------------
   procedure Clear (Tree : in out Tree_Type);
   -- Purpose        : Removes all elements from Tree.
   -- Preconditions  : None
   -- Postconditions : Tree is empty.

   ----------------------------------------------------------------------------
   procedure Insert (Tree : in out Tree_Type;
                     Item : in     Element_Type);
   -- Purpose        : Adds Item to Tree.
   -- Preconditions  : None
   -- Postconditions : Tree = original Tree + Item
   --                  Item becomes the current element
   -- Exceptions     : OVERFLOW       If there is no room for Item.
   --                  DUPLICATE_KEY  If an element already exists in the tree
   --                                 with the same key as Item.
   --                                 Tree is unchanged in both cases.

   -----------------------------------------------------------------------------
   procedure Delete (Tree : in out Tree_Type;
                     Key  : in      Key_Type);
   -- Purpose        : Deletes the element with the given Key from Tree
   -- Preconditions  : None
   -- Postconditions : Tree = original Tree with the Key element removed
   --                  The current element is not defined.
   -- Exceptions     : KEY_ERROR  If Key is not found in Tree.
   --                             Tree is unchanged.

   -----------------------------------------------------------------------------
   procedure Modify (Tree    : in out Tree_Type;
                     Element : in     Element_Type);
   -- Purpose        : Replace current element with Element.
   -- Preconditions  : None
   -- Postconditions : The current element is replaced by Element
   -- Exceptions     : CURRENT_UNDEFINED  If the current element is not
   --                                     defined.  Tree is unchanged.
   --                  KEY_ERROR     If Element has a different key than the
   --                                current element.  Tree and current
   --                                element are unchanged.

   ----------------------------------------------------------------------------
   procedure Find (Tree : in out Tree_Type;
                   Key  : in      Key_Type);
   -- Purpose        : Searches Tree for Key
   -- Preconditions  : None
   -- Postconditions : If there is an element with Key in Tree, that element
   --                  becomes the current element for the Tree.  If the Key
   --                  is not found in Tree, the current element is undefined.


   -- Observers

   ----------------------------------------------------------------------------
   function Current_Defined (Tree : in Tree_Type) return Boolean;
   -- Purpose        : Determines whether or not a current element is defined
   -- Preconditions  : None
   -- Postconditions : Returns True is a current element is defined, otherwise
   --                  returns False.

   ----------------------------------------------------------------------------
   function Empty (Tree : in Tree_Type) return Boolean;
   -- Purpose        : Determines whether Tree is empty.
   -- Preconditions  : None
   -- Postconditions : Empty = (Tree is empty)

   ----------------------------------------------------------------------------
   function Full (Tree : in Tree_Type) return Boolean;
   -- Purpose        : Determines whether Tree is full.
   -- Preconditions  : None
   -- Postconditions : Full = (Tree is full)

   ----------------------------------------------------------------------------
   function Size (Tree : in Tree_Type) return Natural;
   -- Purpose        : Determines the number of elements in Tree.
   -- Preconditions  : None
   -- Postconditions : Size = number of elements in Tree

   -----------------------------------------------------------------------------
   procedure Retrieve (Tree    : in     Tree_Type;
                       Element :    out Element_Type);
   -- Purpose        : Get an element from Tree
   -- Preconditions  : None
   -- Postconditions : Element is a copy of the current element of Tree.
   -- Exceptions     : CURRENT_UNDEFINED  If the current element is not
   --                                     defined.

   ----------------------------------------------------------------------------
   function Height (Tree : in Tree_Type) return Natural;
   -- Purpose        : Determine the height of Tree
   -- Preconditions  : None
   -- Postconditions : Height = number of levels in tree

   -- Iterator

   -----------------------------------------------------------------------------
   procedure Traverse
         (Tree    : in out Tree_Type;
          Order   : in     Traversal_Order;
          Process : not null access procedure (Element : in out Element_Type));
   -- Purpose        : Process all the elements in Tree in the given Order
   -- Preconditions  : Procedure Process does not change the key of an element
   -- Postconditions : Every element in Tree is passed to a call of
   --                  procedure Process. Elements processed in the given Order

private
   type Balance_Type is (Left_Heavy, In_Balance, Right_Heavy);

   type Node_Type;                       -- Incomplete type declaration
   type Node_Ptr is access Node_Type;    -- Access to a node
   type Node_Type is                     -- Complete type declaration
      record
         Info    : Element_Type;    -- One element
         Balance : Balance_Type;    -- Relative heights of subtrees
         Left    : Node_Ptr;        -- Link to left child
         Right   : Node_Ptr;        -- Link to right child
      end record;

   type Tree_Type is new Ada.Finalization.Limited_Controlled with
      record
         Root    : Node_Ptr;  -- Designates first node in the tree
         Current : Node_Ptr;  -- Designates the current node in the tree
      end record;

   overriding procedure Finalize (Tree : in out Tree_Type) renames Clear;

end AVL_Tree;
