procedure Binary_Search (List     : in  Array_Type;
                         Value    : in  Element_Type;
                         Found    : out Boolean;
                         Location : out Index_Type) is
   First_Index : Index_Type; -- First index in current search area
   Last_Index  : Index_Type; -- Last index in current search area
   Midpoint    : Index_Type; -- Index of search area's midpoint
begin
   -- Is the list empty?
   if List'Length = 0 then
      Found := False;
   -- Is the value outside of the range of indices?
   elsif Value < List (List'First)  or  List (List'Last) < Value then
      Found := False;

   else -- Do the search

      Found       := False;
      First_Index := List'First;
      Last_Index  := List'Last;

      -- Search until element found or the current search area is empty.
      -- Current search area is List (First_Index..Last_Index)
      Search_Loop :
      loop
         exit Search_Loop when First_Index > Last_Index  or  Found;
         -- Find middle element in the current search area.
         Midpoint := Index_Type'Val ((Index_Type'Pos (First_Index) +
                                      Index_Type'Pos (Last_Index)) / 2);
         -- Compare Value to middle element in search area.
         if List (Midpoint) = Value then
            Found := True;
         elsif Value < List (Midpoint) then
            Last_Index := Index_Type'Pred (Midpoint);   -- Search 1st half
         else
            First_Index := Index_Type'Succ (Midpoint);  -- Search 2nd half
         end if;
      end loop Search_Loop;
      Location := Midpoint;
   end if;
end Binary_Search;