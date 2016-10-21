with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Largest;
procedure Gambling is

   subtype Die_Range  is Integer range 1 .. 6;
   subtype Dice_Range is Integer range 2 .. 12;

   type    Natural_Array   is array (Positive range <>) of Natural;
   subtype Frequency_Array is Natural_Array (Dice_Range);

   package  Random_Die     is new  Ada.Numerics.Discrete_Random (Result_Subtype => Die_Range);

   function Largest_Natural is new Largest (Index_Type   => Positive,
                                            Element_Type => Natural,
                                            Array_Type   => Natural_Array,
                                            ">"          => ">");

   ----------------------------------------------------------------------------
   procedure Display_Chars (Char : in Character;
                            Num  : in Natural) is
   -- Display Num copies of Char
   -- Preconditions  : none
   -- Postconditions : Num copies of Char are displayed to standard output
   begin
      for Count in 1 .. Num loop
         Ada.Text_IO.Put (Char);
      end loop;
   end Display_Chars;


   ----------------------------------------------------------------------------
   procedure Display_Histogram (List : in Frequency_Array;
                                Max  : in Positive) is
   -- Display a histogram of the data in List
   -- Preconditions  : none
   -- Postconditions : A histogram of the data in List is displayed to standard output

      Biggest    : Natural;    -- The largest value in List
      Star_Value : Positive;   -- The value of one star in the histogram
      Num_Stars  : Natural;    -- The number of stars on one line
   begin
      -- Determine the value of one star in the histogram
      Biggest    := Largest_Natural (List);  -- Maximum value in List
      Star_Value := Biggest / Max;
      -- Correct the division so that that we take the ceiling rather than average
      if Biggest rem Max /= 0 then
         Star_Value := Star_Value + 1;
      end if;

      Ada.Text_IO.Put ("Here is the histogram with one star equal to ");
      Ada.Integer_Text_IO.Put (Item  => Star_Value,
                               Width => 1);
      Ada.Text_IO.Put_Line (" rolls");
      Ada.Text_IO.New_Line;

      for Dice_Value in List'Range loop
         -- Display the value of the dice
         Ada.Integer_Text_IO.Put (Item  => Dice_Value,
                                  Width => 2);
         Ada.Text_IO.Put (' ');

         -- Determine the number of stars to display for this line
         Num_Stars := List (Dice_Value) / Star_Value;
         -- Round up if necessary
         if List (Dice_Value) rem Star_Value > Star_Value / 2 then
            Num_Stars := Num_Stars + 1;
         end if;

         -- Display the line of stars
         Display_Chars (Char => '*',
                        Num  => Num_Stars);
         Ada.Text_IO.New_Line;
      end loop;
   end Display_Histogram;



   Frequency     : Frequency_Array;       -- Counts of the possible dice values
   Sample_Size   : Natural;               -- How many times to roll the dice
   Die_Generator : Random_Die.Generator;  -- The random die value generator
   Dice          : Dice_Range;            -- The value of one roll of a pair of dice

begin
   -- Intialize the frequency counts and random generator
   Frequency := (Dice_Range => 0);
   Random_Die.Reset (Gen       => Die_Generator);

   Ada.Text_IO.Put_Line ("How many times would you like to roll the dice?");
   Ada.Integer_Text_IO.Get (Sample_Size);

   -- Roll the dice Sample_Size times,
   -- Each iteration, roll the dice once
   for Count in 1 .. Sample_Size loop
      Dice := Random_Die.Random (Die_Generator) + Random_Die.Random (Die_Generator);
      Frequency (Dice) := Frequency (Dice) + 1;
   end loop;

   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("Here are the frequencies");
   Ada.Text_IO.New_Line;

   -- Display the frequencies
   for Value in Dice_Range loop
      Ada.Integer_Text_IO.Put (Item => Value, Width => 3);
      Ada.Integer_Text_IO.Put (Item => Frequency (Value), Width => 8);
      Ada.Text_IO.New_Line;
   end loop;

   -- Display a histogram of the frequencies
   Ada.Text_IO.New_Line (2);
   Display_Histogram (List => Frequency,
                      Max  => 40);
end Gambling;