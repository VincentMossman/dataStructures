with Ada.Sequential_IO;
package Inventory_Definitions is

-- This definition package provides types for inventory records

   type Part_Number_Range is range 1 .. 1000;

   type Part_Rec is
      record
         Number   : Part_Number_Range;
         Quantity : Natural := 0;
      end record;

   package Part_IO is new Ada.Sequential_IO (Element_Type => Part_Rec);

end Inventory_Definitions;
         