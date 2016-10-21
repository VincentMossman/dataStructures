with Integrate;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
procedure Test_Integrate is

-- Completed by   Vincent T. Mossman
-- Date           April 11, 2014

-- A program to test the generic function Integrate

   type High_Precision is digits 15;
   package Precise_IO  is new Ada.Text_IO.Float_IO (Num => High_Precision);

   package Elementary_Functions is new
           Ada.Numerics.Generic_Elementary_Functions (High_Precision);
   use Elementary_Functions;

   function Integration is new Integrate (High_Precision);

   ----------------------------------------------------------------------------
   function One (X : in High_Precision) return High_Precision is
   -- The following pragma instructs the compiler not
   -- to issue a warning for not using the parameter X
   pragma Warnings (Off, X);
   begin
      return 1.0;
   end One;

   ----------------------------------------------------------------------------
   function Two (X : in High_Precision) return High_Precision is
   begin
      return X;
   end Two;

   ----------------------------------------------------------------------------
   function Three (X : in High_Precision) return High_Precision is
   begin
      return Exp (-X ** 2.0);
   end Three;

   ----------------------------------------------------------------------------
   function Four (X : in High_Precision) return High_Precision is
   begin
      return ((1.0 / 3.0) - X) ** (1.0 / 3.0);
   end Four;

   ----------------------------------------------------------------------------
   function Five (X : in High_Precision) return High_Precision is
   begin
      return 4.0 / (1.0 + X ** 2.0);
   end Five;

   ----------------------------------------------------------------------------
   function Six (X : in High_Precision) return High_Precision is
   begin
      return Sin (X) - 0.5 * Cos (X);
   end Six;

-------------------------------------------------------------------------------
   A       : High_Precision;  -- Left bound of integration
   B       : High_Precision;  -- Right bound of intergration
   Epsilon : High_Precision;  -- Acceptable error

begin  -- Test_Integrate

   Input_Loop :  -- Calculate all of the areas requested
   loop          -- Each iteration, calculate one area
      Ada.Text_IO.Put_Line ("Enter A and B   (B < A to terminate program)");
      Precise_IO.Get (A);
      Precise_IO.Get (B);

      exit Input_Loop when B < A;

      Ada.Text_IO.Put_Line ("Enter Epsilon");
      Precise_IO.Get (Epsilon);
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put ("For A = ");
      Precise_IO.Put (Item => A, Fore => 1, Aft => 4, Exp => 0);
      Ada.Text_IO.Put (", B = ");
      Precise_IO.Put (Item => B, Fore => 1, Aft => 4, Exp => 0);
      Ada.Text_IO.Put (", and Epsilon = ");
      Precise_IO.Put (Item => Epsilon, Fore => 1,
                      Aft  => High_Precision'Digits / 2,
                      Exp  => 0);
      Ada.Text_IO.New_Line (2);

      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f(1) is");

      Exception_Block_One :
      begin

         Precise_IO.Put (Item => Integration (F       => One'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f(1) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_One;

      Ada.Text_IO.New_Line (2);
      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f(x) is");

      Exception_Block_Two :
      begin

         Precise_IO.Put (Item => Integration (F       => Two'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f(x) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_Two;

      Ada.Text_IO.New_Line (2);
      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f(e to the -x squared power) is");

      Exception_Block_Three :
      begin

         Precise_IO.Put (Item => Integration (F       => Three'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f(e to the -x squared power) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_Three;

      Ada.Text_IO.New_Line (2);
      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f((1/3 - x) to the 1/3 power) is");

      Exception_Block_Four :
      begin

         Precise_IO.Put (Item => Integration (F       => Four'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f((1/3 - x) to the 1/3 power) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_Four;

      Ada.Text_IO.New_Line (2);
      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f(4/(1 + x squared)) is");

      Exception_Block_Five :
      begin

         Precise_IO.Put (Item => Integration (F       => Five'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f(4/(1 + x squared)) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_Five;

      Ada.Text_IO.New_Line (2);
      ---------------------------------------------------------------
      Ada.Text_IO.Put_Line ("  Result for f(sin(x) - 0.5 cos(x)) is");

      Exception_Block_Six :
      begin

         Precise_IO.Put (Item => Integration (F       => Six'Access,
                                              A       => A,
                                              B       => B,
                                              Epsilon => Epsilon),
                         Fore => 3,
                         Aft  => High_Precision'Digits,
                         Exp  => 4);

      exception
         when STORAGE_ERROR =>
            Ada.Text_IO.Put_Line ("STORAGE_ERROR raised during f(sin(x) - 0.5 cos(x)) integration");
         when The_Error : others =>
            Ada.Text_IO.Put_Line ("Unexpected exception " &
                                  Ada.Exceptions.Exception_Name (The_Error));
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (The_Error));

      end Exception_Block_Six;

      Ada.Text_IO.New_Line (2);

   end loop Input_Loop;
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line ("All Done");
end Test_Integrate;
