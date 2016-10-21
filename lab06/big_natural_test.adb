with Big_Natural;
with Big_Natural.IO; use Big_Natural;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
procedure Big_Natural_Test is

   use type Ada.Text_IO.Positive_Count;

   Number_Column : constant Ada.Text_IO.Positive_Count := 30;
   Label_Column  : constant Ada.Text_IO.Positive_Count := Number_Column + 16;

   package Boolean_IO is new Ada.Text_IO.Enumeration_IO (Boolean);


   procedure Test_Ops (A : in Big_Natural.Big_Natural;
                       B : in Big_Natural.Big_Natural) is
      C : Big_Natural.Big_Natural;
   begin
      Ada.Text_IO.Put ("A is");
      Ada.Text_IO.Set_Col (To => Number_Column);
      Big_Natural.IO.Put (Item => A, Width => 25);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("B is");
      Ada.Text_IO.Set_Col (To => Number_Column);
      Big_Natural.IO.Put (Item => B, Width => 25);
      Ada.Text_IO.New_Line (2);

      -- Relational operator tests

      Ada.Text_IO.Put ("A = B");
      Ada.Text_IO.Set_Col (To => Label_Column + 4);
      Boolean_IO.Put (Item => A = B);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("A <= B");
      Ada.Text_IO.Set_Col (To => Label_Column + 4);
      Boolean_IO.Put (Item => A <= B);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("A >= B");
      Ada.Text_IO.Set_Col (To => Label_Column + 4);
      Boolean_IO.Put (Item => A >= B);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("A < B");
      Ada.Text_IO.Set_Col (To => Label_Column + 4);
      Boolean_IO.Put (Item => A < B);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("A > B");
      Ada.Text_IO.Set_Col (To => Label_Column + 4);
      Boolean_IO.Put (Item => A > B);
      Ada.Text_IO.New_Line (2);

      -- Arithmetic operator tests

      C := A + B;
      Ada.Text_IO.Put ("A + B is");
      Ada.Text_IO.Set_Col (To => Number_Column);
      Big_Natural.IO.Put (Item => C, Width => 25);
      Ada.Text_IO.New_Line;

      begin
         Ada.Text_IO.Put ("A - B is");
         C := A - B;
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
      exception
         when CONSTRAINT_ERROR =>
            Ada.Text_IO.Set_Col (To => Label_Column);
            Ada.Text_IO.Put ("Negative");
      end;
      Ada.Text_IO.New_Line;

      begin
         Ada.Text_IO.Put ("B - A is");
         C := B - A;
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
      exception
         when CONSTRAINT_ERROR =>
            Ada.Text_IO.Set_Col (To => Label_Column);
            Ada.Text_IO.Put ("Negative");
      end;
      Ada.Text_IO.New_Line;

      C := A * B;
      Ada.Text_IO.Put ("A * B is");
      Ada.Text_IO.Set_Col (To => Number_Column);
      Big_Natural.IO.Put (Item => C, Width => 25);
      Ada.Text_IO.New_Line;

      begin
         Ada.Text_IO.Put ("A / B is");
         C := A / B;
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
      exception
         when CONSTRAINT_ERROR =>
            Ada.Text_IO.Set_Col (To => Label_Column);
            Ada.Text_IO.Put ("Division by zero");
      end;
      Ada.Text_IO.New_Line;
      begin
         Ada.Text_IO.Put ("B / A is");
         C := B / A;
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
      exception
         when CONSTRAINT_ERROR =>
            Ada.Text_IO.Set_Col (To => Label_Column);
            Ada.Text_IO.Put ("Division by zero");
      end;
      Ada.Text_IO.New_Line (2);


      -- Shift tests

      for Shift in 0 .. 3 loop
         Ada.Text_IO.Put ("A shifted left");
         Ada.Integer_Text_IO.Put (Item => Shift, Width => 2);
         Ada.Text_IO.Put (" is");
         C := Shift_Left (Number => A, By => Shift);
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.New_Line;

      for Shift in 0 .. 3 loop
         Ada.Text_IO.Put ("A shifted right");
         Ada.Integer_Text_IO.Put (Item => Shift, Width => 2);
         Ada.Text_IO.Put (" is");
         C := Shift_Right (Number => A, By => Shift);
         Ada.Text_IO.Set_Col (To => Number_Column);
         Big_Natural.IO.Put (Item => C, Width => 25);
         Ada.Text_IO.New_Line;
      end loop;

   end Test_Ops;

-----------------------------------------------------------------------------------
   X, Y, Z : Big_Natural.Big_Natural;
   Zero    : Big_Natural.Big_Natural;

begin
   Ada.Text_IO.Put_Line ("Tests without input");
   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.Put_Line ("                     Natural              Big Natural");
   Ada.Text_IO.Put_Line ("                     -------              -----------");

   Ada.Text_IO.Put ("54321 is        ");
   X := To_Big_Natural (54321);
   Ada.Integer_Text_IO.Put (Item => To_Natural (X), Width => 12);
   Big_Natural.IO.Put (Item => X, Width => 25);
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put ("12345 is        ");
   Y := To_Big_Natural (12345);
   Ada.Integer_Text_IO.Put (Item => To_Natural (Y), Width => 12);
   Big_Natural.IO.Put (Item => Y, Width => 25);
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put ("Natural'Last is ");
   Z := To_Big_Natural (Natural'Last);
   Ada.Integer_Text_IO.Put (Item => To_Natural (Z), Width => 12);
   Big_Natural.IO.Put (Item => Z, Width => 25);
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put ("Zero is         ");
   Zero := To_Big_Natural (0);
   Ada.Integer_Text_IO.Put (Item => To_Natural (Zero), Width => 12);
   Big_Natural.IO.Put (Item => Zero, Width => 25);
   Ada.Text_IO.New_Line (2);

   Ada.Text_IO.Put_Line ("Done with To_Big_Natural tests");
   Ada.Text_IO.New_Line;

   Ada.Text_IO.Put_Line ("Checking for expected overflow when converting a big natural to natural");
   Ada.Text_IO.Put_Line ("This test requires that the ""+"" operator be completed");
   Ada.Text_IO.New_Line;
   Z := X + Y + Z;  -- This addition results in a number too large to convert to Natural
   begin
      Ada.Integer_Text_IO.Put (Item => Big_Natural.To_Natural (Z), Width => 12);
   exception
      when Constraint_Error =>
         Ada.Text_IO.Put_Line ("Successfully detected a Value too large to convert to Natural");
   end;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Done with To_Natural test constraint error test");
   Ada.Text_IO.New_Line (2);

   Test_Ops (X, Y);
   Ada.Text_IO.New_Line (2);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("Done with operator tests without input");
   Ada.Text_IO.New_Line (2);


   -- Get data from keyboard
   -- Each iteration, test one pair of numbers
   loop
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("-------------------------------------------------------------------------");
      Ada.Text_IO.Put_Line ("Enter two natural numbers.");
      Big_Natural.IO.Get (Item => X);
      Big_Natural.IO.Get (Item => Y);
      Test_Ops (X, Y);
      Ada.Text_IO.New_Line;
      exit when X = Zero and Y = Zero;
   end loop;

end Big_Natural_Test;