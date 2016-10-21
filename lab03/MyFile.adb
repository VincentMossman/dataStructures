   -- file:    InLab08.adb
   -- Author:  W E Beck
   -- Abstract:   Practice using the Set ADT: Sets of hobbies
   -- Modified by: J W McCormick

with Ada.Text_IO;
with Discrete_Set;

procedure Inlab03 is

   type Hobby_Type is (Skiing, Skating, Tennis, Baseball, Soccer, Football,
                       Basketball, Hearts, Old_Maid, Five_Hundred, Bridge,
                       Dancing, Music, Photography, Painting, Gardening,
                       Mountain_Climbing, Hiking, Fishing, Hunting, Sewing,
                       Knitting, Reading, Net_Surfing, Woodworking);

   package  Hobby_Set is new Discrete_Set (Element_Type => Hobby_Type);
   use type Hobby_Set.Set_Type;

   package Hobby_IO is new Ada.Text_IO.Enumeration_IO (Enum => Hobby_Type);

   ----------------------------------------------------------------------------
   procedure Add_A_Range (Hobbies : in out Hobby_Set.Set_Type;
                          First   : in     Hobby_Type;
                          Last    : in     Hobby_Type) is
   -- Add the hobbies in the range First to Last to the set Hobbies
   --
   -- Preconditions  : none
   --
   -- Postconditions : All hobbies between First and Last, inclusive are a
   --                  subset of Hobbies
   begin
      for Index in First .. Last loop
         Hobbies := Hobbies + Index;
      end loop;
   end Add_A_Range;


   ----------------------------------------------------------------------------
   procedure Remove_A_Range (Hobbies : in out Hobby_Set.Set_Type;
                             First   : in     Hobby_Type;
                             Last    : in     Hobby_Type) is
   -- Remove all hobbies in a given range from the set Hobbies
   --
   -- Preconditions  : none
   --
   -- Postconditions : No hobbies in the range First .. Last are members of Hobbies
   begin
      for Index in First .. Last loop
         Hobbies := Hobbies - Index;
      end loop;
   end Remove_A_Range;


   ----------------------------------------------------------------------------
   procedure Put (Item        : in Hobby_Set.Set_Type;
                  Line_Length : in Positive) is
   -- Output a set of hobbies to the screen
   --
   -- Preconditions  : none
   --
   -- Postconditions : All hobbies in Item are displayed on lines containing
   --                  no more than Line_Length characters.

      Saved_Line_Length : Ada.Text_IO.Count;

   begin
      -- Get and save the current value of Text_IO's line length
      Saved_Line_Length := Ada.Text_IO.Line_Length;

      -- Change the value of Text_IO's line length.  Note the type conversion of
      -- a Positive value to a Count value
      Ada.Text_IO.Set_Line_Length (To => Ada.Text_IO.Count (Line_Length));

      -- Display all of the hobbies in the set of hobbies
      -- Each iteration, process one potential hobby
      for Hobby in Hobby_Type loop
         if Hobby_Set.Is_Member (Set => Item, Element => Hobby) then
            Hobby_IO.Put (Item => Hobby);
            Ada.Text_IO.Put (Item => ' ');
         end if;
      end loop;

      -- Restore the original value of Text_IO's line length
      Ada.Text_IO.Set_Line_Length (To => Saved_Line_Length);
   end Put;

-------------------------------------------------------------------------------
   Sports             : Hobby_Set.Set_Type;  -- Holds hobbies which are sports
   Cards              : Hobby_Set.Set_Type;  -- Holds hobbies which are card games
   Outdoor_Activities : Hobby_Set.Set_Type;  -- Holds hobbies which are normally done outdoors
   Other_Hobbies      : Hobby_Set.Set_Type;  -- None of the above
   Some_Hobbies       : Hobby_Set.Set_Type;


begin
   -- Initialize the sets to empty
   Sports             := Hobby_Set.Empty_Set;
   Cards              := Hobby_Set.Empty_Set;
   Outdoor_Activities := Hobby_Set.Empty_Set;
   Other_Hobbies      := Hobby_Set.Empty_Set;
   Some_Hobbies       := Hobby_Set.Empty_Set;

   -- Display the universal set of hobbies
   Ada.Text_IO.Put_Line ("The universal set of Hobbies");
   Put (Item        => Hobby_Set.Universal_Set,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Put some hobbies in Some_Hobbies
   Add_A_Range (Hobbies => Some_Hobbies,   -- First call to Add_A_Range
                Last    => Old_Maid,
                First   => Soccer);

   -- Add some more hobbies to Some_Hobbies
   Some_Hobbies := Some_Hobbies + Woodworking;
   Add_A_Range (Hobbies => Some_Hobbies,   -- Second call to Add_A_Range
                Last    => Mountain_Climbing,
                First   => Photography);


   Ada.Text_IO.Put_Line ("Some_Hobbies:");
   Put (Item        => Some_Hobbies,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Remove some hobbies from Some_Hobbies
   Some_Hobbies := Some_Hobbies - Hiking;
   Remove_A_Range (Hobbies => Some_Hobbies,
                   First   => Football,
                   Last    => Hearts);

   Ada.Text_IO.Put_Line ("With some removed:");
   Put (Item        => Some_Hobbies,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Put some hobbies in Sports
   Add_A_Range (Hobbies => Sports,
                Last    => Basketball,
                First   => Skiing);

   Ada.Text_IO.Put_Line ("Sports:");
   Put (Item        => Sports,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Put some hobbies in Cards
   Add_A_Range (Hobbies => Cards,
                Last    => Bridge,
                First   => Hearts);

   Ada.Text_IO.Put_Line ("Cards:");
   Put (Item        => Cards,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Put some hobbies in Outdoor_Activities
   Add_A_Range (Hobbies => Outdoor_Activities,
                Last    => Hunting,
                First   => Gardening);

   Ada.Text_IO.Put_Line ("Outdoor Activities:");
   Put (Item        => Outdoor_Activities,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);

   -- Put some hobbies in Other_Activities
   Other_Hobbies := Hobby_Set.Universal_Set - Sports - Cards - Outdoor_Activities;

   Ada.Text_IO.Put_Line ("Other Hobbies:");
   Put (Item        => Other_Hobbies,
        Line_Length => 70);
   Ada.Text_IO.New_Line (2);


end Inlab03;