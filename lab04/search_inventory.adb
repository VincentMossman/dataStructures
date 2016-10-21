with Inventory_Definitions;
use type Inventory_Definitions.Part_Number_Range;
with Binary_Search;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Generic_Array_Sort;
procedure Search_Inventory is

-- This program searches an inventory for the quantities of desired parts

   type Inventory_Array    is array (Integer range <>) of Inventory_Definitions.Part_Rec;
   subtype Inventory_Range is Integer range 1 .. 500;
   subtype Small_Inventory is Inventory_Array (Inventory_Range);

   ----------------------------------------------------------------------------
   function "<" (Left  : in Inventory_Definitions.Part_Rec;
                 Right : in Inventory_Definitions.Part_Rec) return Boolean is
   -- Returns True if Left's part number is less than Right's part number.
   -- Otherwise, returns False.
   begin
      return Left.Number < Right.Number;
   end "<";

   ----------------------------------------------------------------------------
   function "=" (Left  : in Inventory_Definitions.Part_Rec;
                 Right : in Inventory_Definitions.Part_Rec) return Boolean is
   -- Returns True if Left's part number is equal to the Right's part number.
   -- Otherwise, returns False.
   begin
      return Left.Number = Right.Number;
   end "=";


   ----------------------------------------------------------------------------
   procedure Inventory_Sort is new Ada.Containers.Generic_Array_Sort
                            (Index_Type   => Integer,
                             Element_Type => Inventory_Definitions.Part_Rec,
                             Array_Type   => Inventory_Array,
                             "<"          => Search_Inventory."<");

   ----------------------------------------------------------------------------
   procedure Inventory_Search is new Binary_Search (Index_Type   => Integer,
                                                    Element_Type => Inventory_Definitions.Part_Rec,
                                                    Array_Type   => Inventory_Array,
                                                    "<"          => "<",
                                                    "="          => "=");

   ----------------------------------------------------------------------------
   procedure Get_Inventory (Inventory : out Inventory_Array;
                            Size      : out Natural) is
   -- Gets the inventory from a file
   -- Preconditions  : The file Inventory.Dat exists
   -- Postconditions : Inventory contains the contents of Inventory.Dat
   --                  Size is the number of parts in the array inventory
      Inventory_File : Inventory_Definitions.Part_IO.File_Type;
   begin
      -- Prepare the data file
      Inventory_Definitions.Part_IO.Open (File => Inventory_File,
                                          Mode => Inventory_Definitions.Part_IO.In_File,
                                          Name => "Inventory.Dat");
      -- Get all of the parts from the file and put them into the array
      -- Each iteration, get one part
      Size := 0;
      loop
         exit when Inventory_Definitions.Part_IO.End_Of_File (Inventory_File);
         Size := Size + 1;
         Inventory_Definitions.Part_IO.Read (File => Inventory_File,
                                             Item => Inventory (Size));
      end loop;
      -- Close the inventory file
      Inventory_Definitions.Part_IO.Close (Inventory_File);
   end Get_Inventory;

   ----------------------------------------------------------------------------
   procedure Display_Inventory (Inventory : in Inventory_Array) is
   -- Display the contents of Inventory
   -- Preconditions  : none
   -- Postconditions : Inventory is displayed, one part per line
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line (" Part     Quantity");
      Ada.Text_IO.New_Line;
      for Index in Inventory'Range loop
         Ada.Integer_Text_IO.Put (Item  => Integer (Inventory (Index).Number),
                                  Width => 5);
         Ada.Integer_Text_IO.Put (Item  => Inventory (Index).Quantity,
                                  Width => 10);
         Ada.Text_IO.New_Line;
      end loop;
   end Display_Inventory;


-------------------------------------------------------------------------------

   Inventory : Small_Inventory;  -- An array of all the inventory parts
   Num       : Natural;          -- The number of parts in Inventory
   Part_Num  : Natural;          -- A part number for which to search
   Part      : Inventory_Definitions.Part_Rec;  -- One inventory part

   Found     : Boolean;   -- Values for
   Index     : Natural;   -- search results

begin
   Ada.Text_IO.Put_Line ("Loading inventory information into memory");
   Ada.Text_IO.New_Line;
   Get_Inventory (Inventory => Inventory,
                  Size      => Num);
   Ada.Text_IO.Put_Line ("The inventory before sorting");
   Display_Inventory (Inventory (1 .. Num));
   Ada.Text_IO.New_Line (2);

   -- Sort the inventory array by part number
   Inventory_Sort (Container => Inventory (1 .. Num));

   Ada.Text_IO.Put_Line ("The inventory after sorting");
   Display_Inventory (Inventory (1 .. Num));
   Ada.Text_IO.New_Line (2);

   -- Make multiple searches of the inventory
   -- Each iteration, make one search
   loop
      Ada.Text_IO.Put_Line ("Enter a part number (zero to exit)");
      Ada.Integer_Text_IO.Get (Part_Num);
      exit when Part_Num = 0;

      -- Cast the integer entered to a part number and assign to the record field
      Part.Number := Inventory_Definitions.Part_Number_Range (Part_Num);

      Inventory_Search (List     => Inventory (1 .. Num),
                        Value    => Part,
                        Found    => Found,
                        Location => Index);
      if Found then
         Ada.Integer_Text_IO.Put (Item  => Integer (Inventory (Index).Number),
                                  Width => 5);
         Ada.Integer_Text_IO.Put (Item  => Inventory (Index).Quantity,
                                  Width => 10);
      else
         Ada.Text_IO.Put_Line ("Part number not found in Inventory");
      end if;
      Ada.Text_IO.New_Line (2);

   end loop;

   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("All done");

end Search_Inventory;