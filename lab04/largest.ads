generic
   type Index_Type   is (<>);
   type Element_Type is private;
   type Array_Type   is array (Index_Type range <>) of Element_Type;
   with function ">" (Left : in Element_Type; Right : in Element_Type) return Boolean;
function Largest (Values : in Array_Type) return Element_Type;
-- Find the largest value in the array Values
--
-- Preconditions  : Values'Length > 0    (There is at least one value in the array)
--
-- Postconditions : The value returned is the largest value in the array Values 