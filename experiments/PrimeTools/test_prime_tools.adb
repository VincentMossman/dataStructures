with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Prime_Tools; use Prime_Tools;

procedure Test_Prime_Tools is

   Lower_Bound : Integer;
   Upper_Bound : Integer;
   Occurance   : Integer;

begin

   Ada.Text_IO.Put_Line ("Let X be Prime");
   Ada.Text_IO.Put_Line ("X = 4K + 3, for some positive integer K");
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Enter Lower K Bound: ");
   Ada.Integer_Text_IO.Get (Lower_Bound);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put ("Enter Upper K Bound: ");
   Ada.Integer_Text_IO.Get (Upper_Bound);
   Ada.Text_IO.New_Line (Spacing => 3);
   Ada.Text_IO.Put_Line ("Results:");
   Ada.Text_IO.New_Line;

   Occurance := 0;

   for K in Lower_Bound .. Upper_Bound loop
      if Is_Prime (4 * K + 3) then
         Occurance := Occurance + 1;
         Ada.Text_IO.Put ("Occurance ");
         Ada.Integer_Text_IO.Put (Occurance, 0);
         Ada.Text_IO.Put (":  K = ");
         Ada.Integer_Text_IO.Put (K, 0);
         Ada.Text_IO.Put ("  X = ");
         Ada.Integer_Text_IO.Put (4 * K + 3, 0);
         Ada.Text_IO.New_Line;
      end if;
   end loop;

end Test_Prime_Tools;