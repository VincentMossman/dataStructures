package body Square_Path is

-- Compute the number of paths between diagonally opposite corners in a
-- square grid.  Two different implementation are provided along with
-- mechanisms to count the number of calls to the 'work horse' internal
-- recursive functions.

-- Written  by Walter Beck and modified by John McCormick



   -- Global-Package variables for recursive call counts
   Count_1 : Big_Natural.Big_Natural;  -- Counter for Path_Algorithm_1
   Count_2 : Big_Natural.Big_Natural;  -- Counter for Path_Algorithm_2

   ----------------------------------------------------------------------------
   function Path_Algorithm_1 (Dimension : in Positive) return Big_Natural.Big_Natural is

      Result : Big_Natural.Big_Natural;    -- The number of distinct paths

      -------------------------------------------------------------------------
      function Num_Paths (Start_Row    : Positive;
                          Start_Column : Positive) return Big_Natural.Big_Natural is

      -- Calculates the number of distinct paths in a rectangular grid from the
      -- given starting position to Row 1, Column 1.

         Result : Big_Natural.Big_Natural;  -- The number of distinct paths
      begin
         -- Increment the number of times this function has been called
         Count_1 := Count_1 + To_Big_Natural (1);

         if Start_Row = 1 or Start_Column = 1 then
            Result := To_Big_Natural (1);
         else
            Result := Num_Paths (Start_Row - 1, Start_Column) +
                      Num_Paths (Start_Row, Start_Column - 1);
         end if;

         return Result;
      end Num_Paths;
      -------------------------------------------------------------------------

   begin -- Path_Algorithm_1
      Count_1 := To_Big_Natural (0);  -- Initialize number of times function Num_Paths is called
      Result := Num_Paths (Start_Row    => Dimension,
                           Start_Column => Dimension);
      return Result;
   end Path_Algorithm_1;



   ----------------------------------------------------------------------------
   function Path_Algorithm_2 (Dimension : in Positive) return Big_Natural.Big_Natural is

      -- An array to save previously calculated path counts
      type Save_Type is array (1 .. Dimension, 1 .. Dimension) of Big_Natural.Big_Natural;

      Save_Area : Save_Type;       -- Previously calculated path counts
      pragma Warnings (Off, Save_Area);
      Result    : Big_Natural.Big_Natural;         -- The number of distinct paths

      -------------------------------------------------------------------------
      function Num_Paths (Start_Row    : Positive;
                          Start_Column : Positive) return Big_Natural.Big_Natural is

      -- Calculates the number of distinct paths in a rectangular grid from the
      -- given starting position to Row 1, Column 1.  Stores previously calculated
      -- counts in the local-global array Save_Area

         Result : Big_Natural.Big_Natural;      -- The number of distinct paths
      begin
         -- Increment the number of times this function has been called
         Count_2 := Count_2 + 1;

         if Save_Area (Start_Row, Start_Column) /= 0 then
            Result := Save_Area (Start_Row, Start_Column);
         else
            if Start_Row = 1 or Start_Column = 1 then
               Result := To_Big_Natural (1);
            else
               Result := Num_Paths (Start_Row - 1, Start_Column) +
                         Num_Paths (Start_Row, Start_Column - 1);
            end if;
            Save_Area (Start_Row, Start_Column) := Result;
         end if;

         return Result;
      end Num_Paths;
      -------------------------------------------------------------------------
   begin
      Count_2   := To_Big_Natural (0);
      Save_Area := (others => (others => To_Big_Natural (0)));
      Result := Num_Paths (Start_Row    => Dimension,
                           Start_Column => Dimension);
      return Result;
   end Path_Algorithm_2;


   ----------------------------------------------------------------------------
   function Count_Algorithm_1 return Big_Natural.Big_Natural is
   begin
      return Count_1;
   end Count_Algorithm_1;

   ----------------------------------------------------------------------------
   function Count_Algorithm_2 return Big_Natural.Big_Natural is
   begin
      return Count_2;
   end Count_Algorithm_2;

end Square_Path;