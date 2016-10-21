with Big_Natural;    use Big_Natural;
with Big_Natural.IO; use Big_Natural.IO;
package Square_Path is

-- Compute the number of paths between diagonally opposite corners in a
-- square grid.  Two different implementation are provided along with
-- mechanisms to count the number of calls to the 'work horse' internal
-- recursive functions.

-- Written  by Walter Beck     March 1998
-- Modified by John McCormick  April 1999


   function Path_Algorithm_1 (Dimension : Positive) return Big_Natural.Big_Natural;
   -- Preconditions  :   None
   --
   -- Postconditions :   Returns the number of paths between opposite corners
   --                    in a square with the given dimension.

   function Path_Algorithm_2 (Dimension : Positive) return Big_Natural.Big_Natural;
   -- Preconditions  :   None
   --
   -- Postconditions :   Returns the number of paths between opposite corners
   --                    in a square with the given Dimension.

   function Count_Algorithm_1 return Big_Natural.Big_Natural;
   -- Preconditions  :   Path_Algorithm_1 called before this function
   --
   -- Postconditions :   Returns the number of recursive calls to compute the
   --                    number of recursive calls by Path_Algorithm_1

   function Count_Algorithm_2 return Big_Natural.Big_Natural;
   -- Preconditions  :   Path_Algorithm_2 called before this function
   --
   -- Postconditions :   Returns the number of recursive calls to compute the
   --                    number of recursive calls by Path_Algorithm_2

end Square_Path;