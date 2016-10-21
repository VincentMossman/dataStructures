package Prime_Tools is

-- Type for lists of prime numbers
type Prime_Array is array (Positive range <>) of Positive;

-----------------------------------------------------------------------
---------------------------- Primary Tools ----------------------------
-----------------------------------------------------------------------

   function Is_Prime (Item : in Positive) return Boolean;
   -- Function determines whether or not Item is a prime number
   -- Preconditions  : Item must be a positive integer
   -- Postconditions : If Item is prime
   --                     returns TRUE
   --                  else
   --                     returns FALSE

-----------------------------------------------------------------------

   procedure Is_Prime (Item  : in Positive;
                       Prime : out Boolean;
                       N     : out Natural);
   -- Procedure determines whether or not Item is a prime number
   -- Preconditions  : Item must be a positive integer
   -- Postconditions : If Item is prime
   --                     returns TRUE, N is set to location of prime
   --                  else
   --                     returns FALSE, N is set to 0
-----------------------------------------------------------------------
---------------------- Prime Number Generation ------------------------
-----------------------------------------------------------------------

   function Generate_Prime (N : in Positive) return Positive;
   -- Function calculates Nth prime number
   -- Preconditions  : N must be positive integer
   -- Postconditions : Returns Nth prime number

-----------------------------------------------------------------------

   procedure Generate_Prime (Item :    out Positive;
                             N    : in     Positive);
   -- Procedure calculates Nth prime number
   -- Preconditions  : N must be positive integer
   -- Postconditions : Copies Nth prime number to Item

-----------------------------------------------------------------------

   function Generate_Prime return Positive;
   -- Function generates randomly generated prime number
   -- Preconditions  : None
   -- Postconditions : Returns randomly generated prime number

-----------------------------------------------------------------------

   procedure Generate_Prime (Item : out Positive);
   -- Procedure generates randomly generated prime number
   -- Preconditions  : None
   -- Postconditions : Copies randomly generated prime number into Item

-----------------------------------------------------------------------
------------------ List of Prime Number Generation --------------------
-----------------------------------------------------------------------

   procedure Fill_Prime_List (Item : out Prime_Array);
   -- Procedure generates a list of consecutive prime numbers
   -- Preconditions  : None
   -- Postconditions : Copies list of consecutive prime numbers
   --                   (starting with 2) into Item

-----------------------------------------------------------------------

   procedure Fill_Random_Prime_List (Item : out Prime_Array);
   -- Procedure generates a list of randomly generated prime numbers
   -- Preconditions  : None
   -- Postconditions : Copies list of randomly generated prime numbers
   --                   into Item

end Prime_Tools;