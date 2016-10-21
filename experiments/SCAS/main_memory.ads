package Main_Memory is

-- Types for data representation
type Bin             is range 0 .. 1;
type Data_Range      is range 0 .. 65_535;
type Data_Slot_Range is range 0 .. 2 ** 16 - 1;
type Address_Range   is range 0 .. 2 ** 8 - 1;
type Chip_Range      is range 0 .. 8;
--------
type Data_Slot is array (Data_Slot_Range) of Bin;
type Address   is array (Address_Range)  of Data_Slot;
type Chip      is
   record
      Active : Boolean;
      Memory : Address;
   end record;
type Memory_Array is array (Chip_Range) of Chip;

-- Types for data retrieval/storage
type Location is
   record
      Chip      : Chip_Range;
      Address   : Address_Range;
      Data_Slot : Data_Slot_Range;
   end record;

-- Functions

procedure Initialize_Memory_Array (Chips : in Chip_Range);
-- Activates array of Main Memory chips for use in simulator
-- Preconditions  : None
-- Postconditions : Specified number of chips are initialized and ready for use

procedure Store (Data : in Data_Range;
                 To   : in Location);
-- Stores Data in Location
-- Preconditions  : None
-- Postconditions : Data is stored in location Location

end Main_Memory;