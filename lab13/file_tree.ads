generic

   Tree_File_Name   : String;  -- Name of file used for tree storage
   Header_File_Name : String;  -- Name of file used for bookkeeping information

   type Element_Type is private;       -- The type of element in the list
   type Key_Type is limited private;   -- The type of key in the element

   -- The user must supply a function that returns the Key of an Element
   with function Key_Of (Element : in Element_Type) return Key_Type;
   -- The user must supply functions for comparing Keys
   with function "=" (Left : in Key_Type; Right : in Key_Type) return Boolean;
   with function "<" (Left : in Key_Type; Right : in Key_Type) return Boolean;

package File_Tree is

-- This package implements a binary search tree ADO. The tree is physically stored
-- in two files.  The user supplies the filenames for these two files.
--
-- Each element in the tree is identified by a unique key.

-- The tree includes the notion of a current element.
-- The retrieve and modify operations are performed on the current node.


   DUPLICATE_KEY     : exception;
   KEY_ERROR         : exception;
   OVERFLOW          : exception;
   CURRENT_UNDEFINED : exception;

   -- Transformers

   ----------------------------------------------------------------------------
   procedure Clear;
   -- Purpose        : Removes all elements from Tree.
   -- Preconditions  : None
   -- Postconditions : Tree is empty.

   ----------------------------------------------------------------------------
   procedure Insert (Item : in Element_Type);
   -- Purpose        : Adds Item to Tree.
   -- Preconditions  : None
   -- Postconditions : Tree = original Tree + Item
   --                  Item becomes the current element
   -- Exceptions     : OVERFLOW       If there is no room for Item.
   --                  DUPLICATE_KEY  If an element already exists in the tree
   --                                 with the same key as Item.
   --                                 Tree is unchanged in both cases.

   -----------------------------------------------------------------------------
   procedure Delete (Key : in  Key_Type);
   -- Purpose        : Deletes the element with the given Key from Tree
   -- Preconditions  : None
   -- Postconditions : Tree = original Tree with the Key element removed
   --                  The current element is not defined.
   -- Exceptions     : KEY_ERROR  If Key is not found in Tree.
   --                             Tree is unchanged.

   -----------------------------------------------------------------------------
   procedure Modify (Element : in Element_Type);
   -- Purpose        : Replace current element with Element.
   -- Preconditions  : None
   -- Postconditions : The current element is replaced by Element
   -- Exceptions     : CURRENT_UNDEFINED  If the current element is not
   --                                     defined.  Tree is unchanged.
   --                  KEY_ERROR     If Element has a different key than the
   --                                current element.  Tree and current
   --                                element are unchanged.

   ----------------------------------------------------------------------------
   procedure Find (Key : in  Key_Type);
   -- Purpose        : Searches Tree for Key
   -- Preconditions  : None
   -- Postconditions : If there is an element with Key in Tree, that element
   --                  becomes the current element for the Tree.  If the Key
   --                  is not found in Tree, the current element is undefined.

   ----------------------------------------------------------------------------
   procedure Close;
   -- Purpose        : Saves tree header information in a file
   --                  Must be called before ending the program or the tree will be lost
   -- Preconditions  : Close was not previously called
   -- Postconditions : Both tree files are closed



   -- Observers

   ----------------------------------------------------------------------------
   function Current_Defined return Boolean;
   -- Purpose        : Determines whether or not a current element is defined
   -- Preconditions  : None
   -- Postconditions : Returns True is a current element is defined, otherwise
   --                  returns False.

   ----------------------------------------------------------------------------
   function Empty return Boolean;
   -- Purpose        : Determines whether Tree is empty.
   -- Preconditions  : None
   -- Postconditions : Empty = (Tree is empty)

   -----------------------------------------------------------------------------
   procedure Retrieve (Element : out Element_Type);
   -- Purpose        : Get an element from Tree
   -- Preconditions  : None
   -- Postconditions : Element is a copy of the current element of Tree.
   -- Exceptions     : CURRENT_UNDEFINED  If the current element is not
   --                                     defined.

   ----------------------------------------------------------------------------
   function Height return Natural;
   -- Purpose        : Determine the height of Tree
   -- Preconditions  : None
   -- Postconditions : Height = number of levels in tree

   -- Iterator

   -----------------------------------------------------------------------------
   procedure InOrder_Traverse (Process : not null access procedure (Element : in out Element_Type));
   -- Purpose        : Process all the elements in Tree in Inorder
   -- Preconditions  : Procedure Process does not change the key of an element
   -- Postconditions : Every element in Tree is passed to a call of
   --                  procedure Process. Elements processed in Inorder



end File_Tree;
