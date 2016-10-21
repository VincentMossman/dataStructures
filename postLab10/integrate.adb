-- Written by   Vincent T. Mossman
-- Date         April 11, 2014

function Integrate (F       : not null access function (X : in Real) return Real;
                    A       : in  Real;
                    B       : in  Real;
                    Epsilon : in  Real) return Real is

   -- Variables
   Total_Area     : Real;   -- Total estimated area
   Left_Area      : Real;   -- Estimated area of left half of curve
   Right_Area     : Real;   -- Estimated area of right half of curve
   Error_Estimate : Real;   -- Error estimate

-------------------------------------------------------------------------------------

   function Integrate (A       : in Real;
                       B       : in Real;
                       Epsilon : in Real) return Real is

   begin   -- Integrate

      -- Calculate Total_Area
      Total_Area := 0.5 * (F (A) + F (B)) * (B - A);

      -- Calculate Left_Area and Right_Area
      Left_Area  := 0.5 * (F (A) + F ((A + B) / 2.0)) * ((A + B) / 2.0 - A);
      Right_Area := 0.5 * (F ((A + B) / 2.0) + F (B)) * (B - (A + B) / 2.0);

      -- Calculate Error_Estimate
      Error_Estimate := (Total_Area - (Left_Area + Right_Area)) / 3.0;

      if not (abs Error_Estimate < Epsilon) then
         return Integrate (A       => A,
                           B       => (A + B) / 2.0,
                           Epsilon => Epsilon / 2.0) +
                Integrate (A       => (A + B) / 2.0,
                           B       => B,
                           Epsilon => Epsilon / 2.0);
      else
         return Left_Area + Right_Area;
      end if;

   end Integrate;

-------------------------------------------------------------------------------------

begin   -- Integrate

   return Integrate (A       => A,
                     B       => B,
                     Epsilon => Epsilon);

end Integrate;

