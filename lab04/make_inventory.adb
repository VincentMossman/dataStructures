with Inventory_Definitions;
with Discrete_Set;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Make_Inventory is

-- This program creates an inventory file for In Lab #4

   package Inventory_Set is new Discrete_Set (Inventory_Definitions.Part_Number_Range);
   use type Inventory_Set.Set_Type;

   package Random_Part_Num is new Ada.Numerics.Discrete_Random (Inventory_Definitions.Part_Number_Range);
   package Random_Quantity is new Ada.Numerics.Discrete_Random (Natural);

   -- Random generators
   Part_Gen     : Random_Part_Num.Generator;
   Quantity_Gen : Random_Quantity.Generator;

   -- The inventory file created by this program
   Inventory_File : Inventory_Definitions.Part_IO.File_Type;

   -- A set of part numbers used (so we don't duplicate part numbers)
   Used : Inventory_Set.Set_Type := Inventory_Set.Empty_Set;

   Num  : Positive;                        -- Number of parts wanted
   Part : Inventory_Definitions.Part_Rec;  -- Information for one part

begin
   Ada.Text_IO.Put_Line ("How many parts would you like to create?");
   Ada.Integer_Text_IO.Get (Num);

   -- Reset the generators with a fixed seed for reproducible results
   Random_Part_Num.Reset (Part_Gen, 50613);
   Random_Quantity.Reset (Quantity_Gen, 12901);

   -- Prepare the data file
   Inventory_Definitions.Part_IO.Create (File => Inventory_File,
                                         Name => "Inventory.Dat");

   -- Create Num inventory records with random part numbers and random quantities
   -- Each iteration, generate one part record
   for Count in 1 .. Num loop
      -- Get a part number that has not yet been used
      -- Each iteration, try one part number
      loop
         Part.Number := Random_Part_Num.Random (Part_Gen);
         exit when not Inventory_Set.Is_Member (Used, Part.Number);
      end loop;

      -- Add the part number to the used set
      Used := Used + Part.Number;
      -- Generate a random quantity for the part
      Part.Quantity := Random_Quantity.Random (Quantity_Gen) rem 10_000;
      -- Put the new part into the inventory file
      Inventory_Definitions.Part_IO.Write (File => Inventory_File,
                                           Item => Part);
   end loop;
   Inventory_Definitions.Part_IO.Close (Inventory_File);
   Ada.Text_IO.Put_Line ("All done creating the part file");

end Make_Inventory;