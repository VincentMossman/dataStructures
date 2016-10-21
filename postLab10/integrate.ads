-- Integrate is a generic subprogram for integrating a mathematical
-- function.  It uses an adaptive trapezoidal rule method for integration.
--
-- It is adaptive in that the number of trapezoids used in a
-- particular region of the function is determined dynamically
-- to meet the given error criteria.

generic
   -- The floating point type used in the calculations
   type Real is digits <>;
function Integrate (F : not null access function (X : in Real) return Real;
                    A       : in  Real;
                    B       : in  Real;
                    Epsilon : in  Real) return Real;

-- Determine an approximation of the integral of the function F in the
-- interval A to B
--
-- Parameters  A        - The left endpoint of integration interval
--             B        - The right endpoint of the integration interval
--             Epsilon  - The maximum allowed absolute value of the error
--
-- Preconditions  : None
--
-- Postconditions : An approximation of the definate integral is returned
--
-- Exceptions     : STORAGE_ERROR raised if there is not enough memory to approximate
--                                the integral to the given error tolerance
--                                   