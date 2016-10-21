package body Discrete_Set is

   ----------------------------------------------------------------------------
   function Is_Member (Set     : in Set_Type;
                       Element : in Element_Type) return Boolean is
   begin
      return Set (Element);
   end Is_Member;

   ----------------------------------------------------------------------------
   function "+" (Left : in Set_Type;  Right : in Set_Type) return Set_Type is
   begin
      return  Left or Right;
   end "+";

   ----------------------------------------------------------------------------
   function "+" (Left  : in Set_Type;
                 Right : in Element_Type) return Set_Type is
      Result : Set_Type;
   begin
      Result := Left;
      Result (Right) := True;   -- Add the new element to the set
      return Result;
   end "+";

   ----------------------------------------------------------------------------
   function "+" (Left  : in Element_Type;
                 Right : in Set_Type) return Set_Type is
      Result : Set_Type;
   begin
      Result := Right;
      Result (Left) := True;   -- Add the new element to the set
      return Result;
   end "+";

   ----------------------------------------------------------------------------
   function "*" (Left : in Set_Type;  Right : in Set_Type) return Set_Type is
   begin
      return  Left and Right;
   end "*";

   ----------------------------------------------------------------------------
   function "-" (Left : in Set_Type;  Right : in Set_Type) return Set_Type is
   begin
      return Left and not Right;
   end "-";

   ----------------------------------------------------------------------------
   function "-" (Left  : in Set_Type;
                 Right : in Element_Type) return Set_Type is
      Result : Set_Type;
   begin
      Result := Left;
      Result (Right) := False;   -- Remove the element from the set
      return Result;
   end "-";

   ----------------------------------------------------------------------------
   function "<=" (Left : in Set_Type; Right : in Set_Type) return Boolean is
      Is_A_Subset : Boolean;
   begin
      Is_A_Subset := (Left - Right) = Empty_Set;
      return Is_A_Subset;
   end "<=";

   ----------------------------------------------------------------------------
   function "<" (Left : in Set_Type; Right : in Set_Type) return Boolean is
      Result : Boolean;
   begin
      if Left = Right then  -- If the sets are equal, not a proper subset
         Result := False;
      else
         Result := Left <= Right;  -- If not equal, test for subset
      end if;                      -- using the function above
      return Result;
   end "<";

   ----------------------------------------------------------------------------
   function ">=" (Left : in Set_Type; Right : in Set_Type) return Boolean is
   begin
      -- Reverse the order of the parameters and use the
      -- operation we've already written
      return  Right <= Left;
   end ">=";

   ----------------------------------------------------------------------------
   function ">" (Left : in Set_Type; Right : in Set_Type) return Boolean is
   begin
      -- Reverse the order of the parameters and use the
      -- operation we've already written
      return  Right < Left;
   end ">";

end Discrete_Set;
