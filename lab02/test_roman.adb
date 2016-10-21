with Roman;
use type Roman.Number;  -- Allows us to use the operators without qualification
with Roman.Number_IO;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Test_Roman is


   A, B, C : Roman.Number;

begin
   Ada.Text_IO.Put_Line ("Enter two additive Roman Numbers");
   Roman.Number_IO.Get (A);
   Roman.Number_IO.Get (B);
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("Their Arabic values are");
   Ada.Integer_Text_IO.Put (Roman.Value_Of (A));
   Ada.Integer_Text_IO.Put (Roman.Value_Of (B));
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("Their sum is ");
   begin
      C := A + B;
      Roman.Number_IO.Put (Item          => C,
                           Width         => 10,
                           Justification => Roman.Number_IO.Left);
      Ada.Text_IO.Put (' ');
      Ada.Integer_Text_IO.Put (Roman.Value_Of (C));
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("larger than 25 numerals");
   end;
   Ada.Text_IO.New_Line (2);

   if A /= B then
      if A > B then
         C := A - B;
      else
         C := B - A;
      end if;
      Ada.Text_IO.Put_Line ("Their difference is ");
      Roman.Number_IO.Put (Item          => C,
                           Width         => 10,
                           Justification => Roman.Number_IO.Right);
      Ada.Text_IO.Put (' ');
      Ada.Integer_Text_IO.Put (Roman.Value_Of (C));
      Ada.Text_IO.New_Line;
   else
      Ada.Text_IO.Put_Line ("The numbers are equal, can't subtract!");
   end if;

   Ada.Text_IO.New_Line (Spacing => 2);

   if A >= B then
      Ada.Text_IO.Put_Line ("True");
   else
      Ada.Text_IO.Put_Line ("False");
   end if;

end Test_Roman;