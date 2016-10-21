generic

   type Element_Type is (<>);

package Discrete_Set is

-- This package implements an ADT for a set of discrete elements

   -- The Set Class
   type Set_Type is private;

   -- A set with no elements
   Empty_Set : constant Set_Type;

   -- A set with all possible elements
   Universal_Set : constant Set_Type;

   -- A type used to build sets from arrays of elements (added for InLab exercise)
   type Element_Array is array (Positive range <>) of Element_Type;


   ----------------------------------------------------------------------
   function Is_Member (Element : in Element_Type;
                       Set     : in Set_Type) return Boolean;
   -- This function determines whether Element is a member of Set

   ----------------------------------------------------------------------
   function "+" (Left : in Set_Type; Right : in Set_Type)
                                                         return Set_Type;
   -- Returns the union of the two sets
   -- (elements that are in either or both sets)

   ----------------------------------------------------------------------
   function "+" (Left : in Set_Type; Right : in Element_Type)
                                                         return Set_Type;
   -- Adds an element to a set
   -- (the set is not changed if the element is already a member)

   ----------------------------------------------------------------------
   function "*" (Left : in Set_Type; Right : in Set_Type)
                                                         return Set_Type;
   -- Returns the intersection of the two sets
   -- (elements occurring in both sets)

   ----------------------------------------------------------------------
   function "-"  (Left : in Set_Type; Right : in Set_Type)
                                                         return Set_Type;
   -- Returns the difference of the two sets
   -- (elements in Left and not in Right)

   ----------------------------------------------------------------------
   function "-" (Left : in Set_Type; Right : in Element_Type)
                                                         return Set_Type;
   -- Removes an element from the set
   -- (the set is not changed if the element is not a member)


   ----------------------------------------------------------------------
   --                       Relational Set Operators
   --                The equality operators (= and /=) are
   --               available for the private type Set_Type
   ----------------------------------------------------------------------

   function "<=" (Left : in Set_Type; Right : in Set_Type) return Boolean;
   -- Returns True if Left is a subset of Right

   function "<" (Left : in Set_Type; Right : in Set_Type) return Boolean;
   -- Returns True if Left is a proper subset of Right

   function ">=" (Left : in Set_Type; Right : in Set_Type) return Boolean;
   -- Returns True if Right is a subset of Left

   function ">" (Left : in Set_Type; Right : in Set_Type) return Boolean;
   -- Returns True if Right is a proper subset of Left


-------------------------------------------------------------------------------
-- Additional operations for Inlab exercise
-------------------------------------------------------------------------------
   function "+" (Left : in Set_Type; Right : in Element_Array) return Set_Type;
   -- Returns the set obtained by adding the values in the array Right 
   -- to the set Left

   function "-" (Left : in Set_Type; Right : in Element_Array) return Set_Type;
   -- Returns the set obtained by removing the values in the array Right 
   -- from the set Left

private

   type Set_Type is array (Element_Type) of Boolean;

      -- Completion of deferred constants
   Empty_Set     : constant Set_Type := (Element_Type => False);
   Universal_Set : constant Set_Type := (Element_Type => True);

end Discrete_Set;
