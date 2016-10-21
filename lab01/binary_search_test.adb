with Ada.Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;
procedure Binary_Search_Test is

   type Integer_Array is array (Positive range <>) of Integer;

   subtype List_Type is Integer_Array (1 .. 10);

   ----------------------------------------------------------------------------
   procedure Binary_Search (List     : in  Integer_Array;
                            Value    : in  Integer;
                            Location : out Natural) is
   -- Purpose        : Searches List for Value. Location is the index of Value
   --                  in List if found.  Otherwise returns zero for Location
   -- Preconditions  : List is sorted in ascending order
   -- Postconditions : If Value is in list
   --                     List(Location) = Value
   --                  Else
   --                     Location = 0

      Found       : Boolean;  -- Has Value been found yet?
      Midpoint    : Natural;  -- Index of search area's midpoint
      First_Index : Natural;  -- First index in current search area
      Last_Index  : Natural;  -- Last index in current search area

   begin
      Found       := False;
      First_Index := List'First;
      Last_Index  := List'Last;

      -- Search until element found or the current search area is empty.
      -- Each iteration, check one element in List
      -- Current search area is List (First_Index..Last_Index)
      loop
         exit when First_Index > Last_Index  or  Found;
         -- Find middle element in the current search area.
         Midpoint := (First_Index + Last_Index) / 2;
         -- Compare Value to middle element in search area.
         if List (Midpoint) = Value then
            Found := True;
         elsif Value < List (Midpoint) then
            Last_Index := Midpoint - 1;   -- Search area now 1st half
         else
            First_Index := Midpoint + 1;  -- Search area now 2nd half
         end if;
      end loop;
      -- Set value of Location.
      if Found then
         Location := Midpoint;
      else
         Location := 0;
      end if;
   end Binary_Search;

   ----------------------------------------------------------------------------
   procedure Get_List (List  : out List_Type;
                       Count : out Natural) is
   -- Ask tester for list of values to put in search array.
   begin
      Put ("How many elements do you want in your list? (0-10)  ");
      Get (Count);
      New_Line;
      if Count > 0 then
         Put_Line ("Enter the elements in order.");
         for Index in 1 .. Count loop
            Get (List (Index));
         end loop;
      end if;
   end Get_List;

-------------------------------------------------------------------------------
   Num_Elements : Natural;       -- number of elements to search
   List         : List_Type;     -- list of elements to search
   Search_Value : Integer;       -- value to search for in List
   Location     : Natural;       -- index of Value in List

begin
   Get_List (List => List, Count => Num_Elements);
   New_Line;
   Put ("For what value do you want to search?  ");
   Get (Search_Value);
   New_Line;
   Binary_Search (List     => List (1 .. Num_Elements),
                  Value    => Search_Value,
                  Location => Location);
   if Location /= 0 then
      Put ("Search value found at location ");
      Put (Item => Location, Width => 2);
   else
      Put ("Location = 0  (Search value not found in list)");
   end if;
   New_Line (2);
   Put_Line ("Normal program termination");
end Binary_Search_Test;
