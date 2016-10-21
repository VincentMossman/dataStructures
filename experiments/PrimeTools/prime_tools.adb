with Ada.Numerics.Discrete_Random;

package body Prime_Tools is

   -- Index generation for random number generation
   type    Random_Range     is new Positive range 1 .. 1000;
   package Random_Generator is new Ada.Numerics.Discrete_Random (Random_Range);
   use     Random_Generator;
   Random_Index : Random_Generator.Generator;

   --------------
   -- Is_Prime --
   --------------

   function Is_Prime (Item : in Positive) return Boolean is

      -- Variables
      Divisor : Natural;

   begin

      -- Initialize variable
      Divisor := Item / 2;

      Check_Prime_Loop :
      loop
         exit when Divisor < 2 or else Item rem Divisor = 0;

         Divisor := Divisor - 1;
      end loop Check_Prime_Loop;

      return Divisor < 2 and Item /= 1;

   end Is_Prime;

   --------------
   -- Is_Prime --
   --------------

   procedure Is_Prime (Item  : in     Positive;
                       Prime :    out Boolean;
                       N     :    out Natural) is

   begin

      Prime := False;
      N := 0;

   end Is_Prime;

   --------------------
   -- Generate_Prime --
   --------------------

   function Generate_Prime (N : in Positive) return Positive is

      -- Variable
      Prime : Positive;

   begin

      -- Initialize variable
      Prime := 1;

      Generate_Prime_Loop :
      for I in 1 .. N loop

         loop

            Prime := Prime + 1;
            exit when Is_Prime (Item => Prime);

         end loop;

      end loop Generate_Prime_Loop;

      return Prime;

   end Generate_Prime;

   --------------------
   -- Generate_Prime --
   --------------------

   procedure Generate_Prime (Item :    out Positive;
                             N    : in     Positive) is

   begin

      Item := Generate_Prime (N => N);

   end Generate_Prime;

   --------------------
   -- Generate_Prime --
   --------------------

   function Generate_Prime return Positive is

   begin

      return Generate_Prime (N => Positive (Random (Random_Index)));

   end Generate_Prime;

   --------------------
   -- Generate_Prime --
   --------------------

   procedure Generate_Prime (Item : out Positive) is

   begin

      Item := Generate_Prime (N => Positive (Random (Random_Index)));

   end Generate_Prime;

   ----------------
   -- Prime_List --
   ----------------

   procedure Fill_Prime_List (Item : out Prime_Array) is

   begin

      Fill_List_Loop :
      for Index in Item'Range loop
         Item (Index) := Generate_Prime (N => Index);
      end loop Fill_List_Loop;

   end Fill_Prime_List;

   -----------------------
   -- Random_Prime_List --
   -----------------------

   procedure Fill_Random_Prime_List (Item : out Prime_Array) is

   begin

      Fill_Random_List_Loop :
      for Index in Item'Range loop
         Item (Index) := Generate_Prime;
      end loop Fill_Random_List_Loop;

   end Fill_Random_Prime_List;

end Prime_Tools;
