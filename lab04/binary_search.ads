generic
   type Index_Type   is (<>);
   type Element_Type is limited private;
   type Array_Type   is array (Index_Type range <>) of Element_Type;
   with function "<" (Left  : in Element_Type; 
                      Right : in Element_Type) return Boolean;
   with function "=" (Left  : in Element_Type; 
                      Right : in Element_Type) return Boolean;

procedure Binary_Search (List     : in  Array_Type;
                         Value    : in  Element_Type;
                         Found    : out Boolean;
                         Location : out Index_Type);
-- Purpose        :  Search List for Value
-- Preconditions  :  List is in ascending order
-- Postconditions :  Value is in List and Found and List(Location) = Value   
--                     or
--                   Value is not in List and not Found 
