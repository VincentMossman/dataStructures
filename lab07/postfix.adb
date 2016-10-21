with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Unbounded;    use Ada.Strings; use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO; use Ada.Text_IO.Unbounded_IO;
with Stack;
procedure Postfix is

-- This program gets a postfix expression, evaluates it, and displays
-- the result. An error message is displayed for invalid expressions.
--
-- Assumption  All operands and operators are separated with at least one blank
--
-- Limitation  This program will handle no more than 50 operands in a row

   Operand_Bound : constant := 50; -- Maximum number of saved operands

   INVALID_EXPRESSION : exception;

   package Integer_Stack is new Stack (Element_Type => Integer);

   ----------------------------------------------------------------------------
   procedure Take_Item (Source : in out Unbounded_String;
                        Item   :    out Unbounded_String) is
   -- Purpose        : Find and remove the first blank delimited item
   --                  from Source
   -- Preconditions  : None
   -- Postconditions : If Source is a null string or a blank string then
   --                     Item is the null string
   --                  Else Item is the first sequence of non-blank characters
   --                     in Source.
   --                  Item is removed from Source

      -- A set of characters with just a single character - the space character
      Blank_Set : constant Ada.Strings.Maps.Character_Set := To_Set (' ');

      First : Positive;  -- Indices of the first and last
      Last  : Natural;   -- characters in a sequence of non-blank characters
   begin
      -- Determine the indices of the item within Source
      Find_Token (Source => Source,
                  Set    => Blank_Set,
                  Test   => Outside,
                  First  => First,
                  Last   => Last);
      -- Copy the item found
      Unbounded_Slice (Source => Source,
                       Target => Item,
                       Low    => First,
                       High   => Last);
      -- Remove the item from Source
      Delete (Source  => Source,
              From    => First,
              Through => Last);
   end Take_Item;

   ----------------------------------------------------------------------------
   function Is_Operator (Item : in Unbounded_String) return Boolean is
   -- Purpose   : Determines whether or not Item is an arithmetic operator
   -- Preconditions  : None
   -- Postconditions : Returns True if Item is an arithmetic operator and
   --                     False otherwise
   begin
      return Item = "+" or Item = "-" or Item = "*"  or Item = "/";
   end Is_Operator;

   ----------------------------------------------------------------------------
   function Evaluate_Operation (Left  : in Integer;
                                Op    : in Unbounded_String;
                                Right : in Integer) return Integer is
   -- Purpose   : Carries out the one arithmetic operation
   -- Preconditions  : Op is a valid arithmetic operator
   -- Postconditions : Result is Left Op Right
   begin
      if Op = "+" then
         return Left + Right;
      elsif Op = "-" then
         return Left - Right;
      elsif Op = "*" then
         return Left * Right;
      else
         return Left / Right;
      end if;
   end Evaluate_Operation;

   ----------------------------------------------------------------------------
   function Evaluate (Expression : in Unbounded_String) return Integer is
   -- Purpose        : Evaluate a postfix expression
   -- Preconditions  : All operands and operators in Expression are
   --                     separated by one or more blanks
   -- Postconditions : Returns the evaluation of Expression
   -- Exceptions     : INVALID_EXPRESSION raised when Expression is not valid
   --                                     or Expression has too many operands
   --                                     for this procedure to handle

      Operand_Stack : Integer_Stack.Stack_Type (Max_Size => Operand_Bound);

      Result : Integer;           -- The answer returned
      Buffer : Unbounded_String;  -- Local copy of expression
      Item   : Unbounded_String;  -- An operator or an operand
      Left   : Integer;           -- First operand
      Right  : Integer;           -- Second operand
   begin
      Buffer := Expression;  -- Make a local copy of Expression
      Result := 0;
      -- Evaluate the postfix expression in Buffer
      -- Each iteration, process one token (operand or operator) in Buffer
      loop
         Trim (Source => Buffer, Side => Ada.Strings.Left);
         exit when Length (Buffer) = 0;
         -- Remove the first item from the buffer
         Take_Item (Source => Buffer, Item => Item);
         if Is_Operator (Item) then
            -- Obtain the two previous operands from the stack
            Operand_Stack.Pop (Right);
            Operand_Stack.Pop (Left);
            -- Do the operation and put the result back on the stack
            Result := Evaluate_Operation (Left, Item, Right);
            Operand_Stack.Push (Result);
         else
            -- Convert the token to an integer and push it onto stack.
            -- If Item is not a valid integer, the conversion will
            -- raise CONSTRAINT_ERROR.
            Result := Integer'Value (To_String (Item));
            Operand_Stack.Push (Result);
         end if;
      end loop;

      -- Pop the result from the stack
      Operand_Stack.Pop (Result);
      -- If the stack is not empty at this point, the Expression is invalid
      if not Operand_Stack.Empty then
         raise INVALID_EXPRESSION;
      else
         return Result;
      end if;
   exception
      when CONSTRAINT_ERROR =>        -- An operand was not a valid integer
         raise INVALID_EXPRESSION;
      when Integer_Stack.UNDERFLOW => -- An operator not preceded by 2 operands
         raise INVALID_EXPRESSION;
   end Evaluate;

-------------------------------------------------------------------------------
   Expression : Unbounded_String;  -- A postfix expression
   Result     : Integer;           -- Result of evaluating an expression

begin
   -- Process a sequence of postfix expressions
   -- Each iteration, evaluate one postfix expression
   loop
      -- Get an expression
      Put_Line ("Enter a postfix expression.");
      Get_Line (Expression);

      -- Trim off leading and trailing blanks
      Trim (Source => Expression, Side => Both);
      exit when Length (Expression) = 0;

      -- Echo the input expression
      Put (Expression);

      -- Determine and display the result of evaluating Expression
      Exception_Block :
      begin
         Result := Evaluate (Expression);
         Put (" = ");
         Put (Item => Result, Width => 1);
      exception
         when INVALID_EXPRESSION =>
            Put (" is an invalid postfix expression" &
                 " (or too large for this program)");
      end Exception_Block;
      New_Line (2);
   end loop;
end Postfix;
