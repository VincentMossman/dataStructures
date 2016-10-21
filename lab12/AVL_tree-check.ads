generic
   with procedure Put (Item : in out Element_Type);
package AVL_Tree.Check is

-- This child package provides a means for checking the validity of an AVL tree
-- It is helpful tool for implementing the AVL tree


   procedure Check_Validity (Tree : in Tree_Type);
   -- Checks whether Tree is a valid AVL tree
   --
   -- Preconditions  : None
   --
   -- Postconditions : If Tree is a valid AVL tree, displays confirmation message
   --                  Otherwise, displays why Tree is not a valid AVL tree

end AVL_Tree.Check;

