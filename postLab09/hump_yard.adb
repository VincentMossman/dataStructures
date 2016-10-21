with Unbounded_Queue;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;

procedure Hump_Yard is

-- This program prints manifests (lists of cars) for trains
-- leaving the Rouses Point hump yard.

   Number_Incoming_Cars : constant := 200;  -- Number of incoming cars

   type Car_Class_Type   is (Box, Flat, Hopper, Tank);
   type Destination_Type is (Austin, Waterloo, Plattsburgh);
   subtype Car_ID_Type   is Positive range 10000 .. 99999;

   -- Input Output packages
   package Class_IO is new Ada.Text_IO.Enumeration_IO (Enum => Car_Class_Type);
   package City_IO  is new Ada.Text_IO.Enumeration_IO (Enum => Destination_Type);

   -- Random generators
   package Pick_Car  is new Ada.Numerics.Discrete_Random
                            (Result_Subtype => Car_Class_Type);
   package Pick_City is new Ada.Numerics.Discrete_Random
                            (Result_Subtype => Destination_Type);
   package Pick_ID   is new Ada.Numerics.Discrete_Random
                            (Result_Subtype => Car_ID_Type);

   -- Type for railroad cars
   type Car_Rec is
      record
         ID          : Car_ID_Type;
         Class       : Car_Class_Type;
         Destination : Destination_Type;
      end record;

   -- Types for track sections in the rail yard
   package Car_Queue is new Unbounded_Queue (Element_Type => Car_Rec);
   Section_Size : constant := 40;           -- Maximum cars per section
   subtype Track_Section is Car_Queue.Queue_Type (Max_Size => Section_Size);

   -- Type for the yard (a collection of track sections)
   type Yard_Array is array (Destination_Type) of Track_Section;

   --------------------------------------------------------------------------------
      procedure Print_Manifest (Destination : in     Destination_Type;
                                Track       : in out Track_Section)    is
      -- This procedure displays information on the cars on the given track.

      -- Preconditions  : None
      --
      -- Postconditions : The information for the cars on the given track is displayed.
      --                  Track is empty.

         Car : Car_Rec;   -- Information on one car

      begin
         -- Display headings
         Ada.Text_IO.Put ("Manifest for ");
         Ada.Integer_Text_IO.Put (Item  => Car_Queue.Size (Track),
                                  Width => 1);
         Ada.Text_IO.Put (" car train going to ");
         City_IO.Put (Destination);
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put ("Car ID");
         Ada.Text_IO.Set_Col (To => 10);
         Ada.Text_IO.Put ("Car Class");
         Ada.Text_IO.New_Line (2);

         Display_Loop :  -- Display all the cars in the manifest
         loop            -- Each iteration, the information for one car is displayed
            exit Display_Loop when Car_Queue.Empty (Track);

            -- Assertion:  The queue Track is not empty

            Car_Queue.Dequeue (Queue => Track,  Item => Car);
            Ada.Integer_Text_IO.Put (Item => Car.ID,  Width => 5);
            Ada.Text_IO.Set_Col (To => 10);
            Class_IO.Put (Car.Class);
            Ada.Text_IO.New_Line;
         end loop Display_Loop;
         Ada.Text_IO.New_Page;
      end Print_Manifest;

-------------------------------------------------------------------------------
   Car_Generator  : Pick_Car.Generator;     -- Random generator of car classes
   City_Generator : Pick_City.Generator;    -- Random generator of cities
   ID_Generator   : Pick_ID.Generator;      -- Random generator of ID numbers

   Yard           : Yard_Array;             -- The hump yard
   Car            : Car_Rec;                -- A single railroad car

begin -- Hump Yard
   -- Assertion:  No queue is full at this point.

   -- Initialize generators so all runs are reproducible.
   Pick_ID.Reset   (Gen       => ID_Generator,
                    Initiator => 1234567);
   Pick_Car.Reset  (Gen       => Car_Generator,
                    Initiator => 989761);
   Pick_City.Reset (Gen       => City_Generator,
                    Initiator => 12901);

   -- Process all of the incoming cars.
   -- Each iteration, process one incoming car.
   for I in 1 .. Number_Incoming_Cars loop

      -- Create an incoming car using pseudorandom generators
      Car.ID          := Pick_ID.Random (ID_Generator);
      Car.Class       := Pick_Car.Random (Car_Generator);
      Car.Destination := Pick_City.Random (City_Generator);

      -- Add the car to the appropriate track section
      Car_Queue.Enqueue (Queue => Yard (Car.Destination),
                         Item  => Car);

      -- If the track section is full, print the manifest
      if Car_Queue.Full (Yard (Car.Destination)) then
         Print_Manifest (Destination => Car.Destination,
                         Track       => Yard (Car.Destination));
      end if;

      -- Assertion:  No queue is full.

   end loop;

   -- Display manifests for all trains (from non-empty track sections)
   for Destination in Destination_Type loop
      if not Car_Queue.Empty (Yard (Destination)) then
         Print_Manifest (Destination => Destination,
                         Track       => Yard (Destination));
      end if;
   end loop;


-- This program should not produce exceptions.  If any occur, it is likely
-- due to an error in the Queue package implementation.
exception
   when Car_Queue.OVERFLOW =>
      Ada.Text_IO.Put_Line ("Unexpected OVERFLOW occurred at unknown point.");
   when Car_Queue.UNDERFLOW =>
      Ada.Text_IO.Put_Line ("Unexpected UNDERFLOW occurred at unknown point.");
   when The_Error : others =>
      Ada.Text_IO.Put_Line ("Unexpected " & Ada.Exceptions.Exception_Name (The_Error)
                           & " at unknown point.");
end Hump_Yard;