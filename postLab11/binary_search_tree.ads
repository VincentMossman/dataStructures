with Ada.Finalization;
generic

   type Element_Type is private;       -- The type of element in the list
   type Key_Type is limited private;   -- The type of key in the element

   -- The user must supply a function that returns the Key of an Element
   with function Key_Of (Element : in Element_Type) return Key_Type;
   -- The user must supply functions for comparing Keys
   with function "=" (Left : in Key_Type; Right : in Key_Type) return Boolean;
   with function "<" (Left : in Key_Type; Right : in Key_Type) return Boolean;

package Binary_Search_Tree is

-- This package implements a binary search tree.
-- Each element in the tree is identified by a unique key.

-- The tree class includes the notion of a current element in the tree.
-- The retrieve and modify operations are performed on the current node.

   type Tree_Type is new Ada.Finalization.Limited_Controlled with private;

   type Traversal_Order is (Inorder, Preorder, Postorder, Reverse_Inorder);

   DUPLICATE_KEY     : exception;
   KEY_ERROR         : exception;
   OVERFLOW          : exception;
   CURRENT_UNDEFINED : exception;

   -- New Operations for the Postlab

   -----------------------------------------------------------------------------
   function Height (Tree : in Tree_Type) return Natural;
   -- Returns the number of levels in a tree
   -- Preconditions  : none
   -- Postconditions : The number of levels in Tree is returned

   -----------------------------------------------------------------------------
   procedure Ancestors
             (Tree  : in Tree_Type;
              Key   : in Key_Type;
              Process : not null access procedure (Element : in Element_Type));
   -- Process the ancestors (youngest to oldest) of the node with the given Key
   -- Preconditions  : None
   -- Postconditions : Process is called for each ancestor
   --                  Each child is processed before its parent
   --                  The node with Key is not processed (just its ancestors)
   --                  If Key is not in Tree, the ancestors processed are those
   --                     that would be processed if the Key had just been added

   -----------------------------------------------------------------------------
   function Leaf_Count (Tree : in Tree_Type) return Natural;
   -- Returns the number of nodes in a tree that have no children
   -- Preconditions  : none
   -- Postconditions : The number of nodes with no children is returned

   -----------------------------------------------------------------------------
   procedure Copy (Source      : in  Tree_Type;
                   Destination : out Tree_Type);
   -- Make an exact duplicate of a tree.
   -- Preconditions   : Source /= Destination
   -- Postconditions  : Returns an exact duplicate of Tree

   -----------------------------------------------------------------------------
   function Same_Shape (Tree_1 : in Tree_Type;
                        Tree_2 : in Tree_Type) return Boolean;
   -- Returns True is Tree_1 and Tree_2 have the same shape.  Nodes do NOT
   -- have to contain the same values.  We are only concerned with the shape
   -- of the two trees.
   --
   -- Preconditions  : none
   --
   -- Postconditins  : Returns True if Tree_1 and Tree_2 have the same shape



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
   --                  If the element removed was the current element, the
   --                  current element is not defined.
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

   -----------------------------------------------------------------------------
   procedure Balance (Tree : in out Tree_Type);
   -- Purpose        : Restructures Tree for optimum performance
   -- Preconditions  : None
   -- Postconditions : Tree is a minimum height tree
   --                  The current element is not defined

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

   type Node_Type;                       -- Incomplete type declaration
   type Node_Ptr is access Node_Type;    -- Access to a node
   type Node_Type is                     -- Complete type declaration
      record
         Info  : Element_Type;    -- One element
         Left  : Node_Ptr;        -- Link to left child
         Right : Node_Ptr;        -- Link to right child
      end record;

   type Tree_Type is new Ada.Finalization.Limited_Controlled with
      record
         Root    : Node_Ptr;  -- Designates first node in the tree
         Current : Node_Ptr;  -- Designates the current node in the tree
      end record;

   overriding procedure Finalize (Tree : in out Tree_Type) renames Clear;

end Binary_Search_Tree;
