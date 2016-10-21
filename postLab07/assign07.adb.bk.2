with Stack;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Text_IO;              use Ada.Text_IO;

procedure Assign07 is

-- This program converts input of unbounded strings in infix
--  notation and displays it to the user in postfix notation
--
-- Written by   Vincent T. Mossman
-- Date         March 14, 2014
--
-- Input
--   From keyboard
--      1. String of input in infix notation
--
-- Output
--   To screen
--      1. Input converted to postfix notation
--
-- Assumptions
--   1. Only the operaters +, -, *, and / are used
--   2. All infix expressions entered by the user are valid

   -- Max stack size
   Operator_Bound : constant := 10;

   -- Set of arithmetic symbols
   Symbol_Set : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("+-*/()");

   -- Stack package instantiation
   package Symbol_Stack is new Stack (Element_Type => Unbounded_String);

----------------------------------------------------------------------

   function "<" (Left  : in Unbounded_String;
                 Right : in Unbounded_String) return boolean is

   -- Purpose        : Helper function for Process_Operator. Function determines
   --                   arithmetic precedence
   -- Preconditions  : Left and Right must be arithmetic symbols
   -- Postconditions : Returns true if Right has a higher precedence than Left

   begin

      return (Left = "+" or Left = "-") and (Right = "*" or Right = "/");

   end "<";

----------------------------------------------------------------------

   function Is_Symbol (Item : in Unbounded_String) return Boolean is

   -- Purpose        : Helper function for Take_Item. Function determines if
   --                   Item is arithmetic symbol
   -- Preconditions  : None
   -- Postconditions : Returns true if Item is arithmetic symbol

   begin

      return Item = "+" or Item = "-" or Item = "*" or Item = "/" or Item = "(" or Item = ")";

   end Is_Symbol;

----------------------------------------------------------------------

   procedure Process_Parenthesis (Stack : in out Symbol_Stack.Stack_Type;
                                  Token : in     Unbounded_String;
                                  Item  : in out Unbounded_String) is

   -- Purpose        : Procedure performs parenthesis algorithm
   -- Preconditions  : None
   -- Postconditions : Item is appended with proper operators and stack
   --                   contains proper tokens

      -- Variables
      Popped_Element : Unbounded_String; -- Item popped off stack

   begin   -- Process_Parenthesis

      if Token = "(" then
         Stack.Push (Token);
      elsif Token = ")" then
         -- Loop pops operators off of stack and appends to Item
         -- Each iteration, pops one operator
         loop
            Stack.Pop (Popped_Element);
            exit when Popped_Element = "(";
            Item := Item & ' ' & Popped_Element;
         end loop;
      end if;

   end Process_Parenthesis;

----------------------------------------------------------------------

   procedure Process_Operator (Stack : in out Symbol_Stack.Stack_Type;
                               Token : in     Unbounded_String;
                               Item  : in out Unbounded_String) is

   -- Purpose        : Procedure performs operator algorithm
   -- Preconditions  : None
   -- Postconditions : Item is appended with proper operators and stack
   --                   contains proper tokens

      -- Variables
      Popped_Element : Unbounded_String; -- Item popped off stack

   begin   -- Process_Operator

      -- Loop processes operator tokens
      -- Each iteration, processes one token
      loop
         exit when Symbol_Stack.Empty (Stack);
         Stack.Pop (Popped_Element);
         exit when Popped_Element < Token or Popped_Element = "(";
         Item := Item & ' ' & Popped_Element;
      end loop;

      if Popped_Element < Token or Popped_Element = "(" then
         Stack.Push (Popped_Element);
      end if;

      Stack.Push (Token);

   end Process_Operator;

----------------------------------------------------------------------

   procedure Take_Item (Source : in out Unbounded_String;
                        Item   :    out Unbounded_String) is

   -- Purpose        : Find and remove first token from source, copy
   --                   token to Item
   -- Preconditions  : Source is non-null unbounded string that is a valid
   --                   infix expression
   -- Postconditions : Item contains first token in source

      -- Variables
      First : Positive;   -- Position of beginning of token
      Last  : Natural;    -- Position of end of token

   begin   -- Take_Item

      -- Remove extra blanks
      Trim (Source => Source,
            Side   => Ada.Strings.Both);

      if Is_Symbol (Unbounded_Slice (Source => Source,
                                     Low    => 1,
                                     High   => 1)) then
         Unbounded_Slice (Source => Source,
                          Target => Item,
                          Low    => 1,
                          High   => 1);
         Delete (Source  => Source,
                 From    => 1,
                 Through => 1);
      else
         Find_Token (Source => Source,
                     Set    => Symbol_Set,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         Unbounded_Slice (Source => Source,
                          Target => Item,
                          Low    => First,
                          High   => Last);
         Delete (Source  => Source,
                 From    => First,
                 Through => Last);
      end if;

   end Take_Item;

----------------------------------------------------------------------

   function To_Postfix (Item : in Unbounded_String) return Unbounded_String is

   -- Purpose        : Function converts an infix expression
   --                   to a postfix expression
   -- Preconditions  : Item must be valid infix expression
   -- Postconditions : Returns postfix conversion of Item

      -- Variables
      Operator_Stack : Symbol_Stack.Stack_Type (Max_Size => Operator_Bound);
      Item_Copy      : Unbounded_String;   -- Copy of Item
      Result         : Unbounded_String;   -- Converted expression
      Token          : Unbounded_String;   -- Individual token

   begin   -- To_Postfix

      -- Initialize variables
      Item_Copy := Item;
      Result    := Null_Unbounded_String;

      -- Loop converts infix expression to postix expression
      -- Each iteration, processes one token
      Convert_Expression :
      loop
         exit Convert_Expression when Item_Copy = Null_Unbounded_String;

         -- Get a token
         Take_Item (Source => Item_Copy,
                    Item   => Token);

         -- Determine what to do with token
         if Token = "(" or Token = ")" then
            -- Perform parenthesis algorithm
            Process_Parenthesis (Stack => Operator_Stack,
                                 Token => Token,
                                 Item  => Result);
         elsif Token = "+" or Token = "-" or Token = "*" or Token = "/" then
            -- Perform operator algorithm
            Process_Operator (Stack => Operator_Stack,
                              Token => Token,
                              Item  => Result);
         else   -- Token is not a symbol
            Trim (Source => Token,
                  Side   => Ada.Strings.Both);
            Result := Result & ' ' & Token;
         end if;

      end loop Convert_Expression;

      -- Loop appends remaining operators to Result
      -- Each iteration, appends one operator
      Append_Remaining_Operators :
      loop
         exit Append_Remaining_Operators when Symbol_Stack.Empty (Operator_Stack);
         Operator_Stack.Pop (Token);
         Result := Result & ' ' & Token;
      end loop Append_Remaining_Operators;

      Trim (Source => Result,
            Side   => Ada.Strings.Both);

      return Result;

      -- Exceptions
      exception
         when Symbol_Stack.OVERFLOW =>
            return To_Unbounded_String ("The expression is too long to convert");

   end To_Postfix;

----------------------------------------------------------------------

   -- Variables
   Infix_Expression   : Unbounded_String;   -- Original infix expression
   Postfix_Expression : Unbounded_String;   -- Infix converted to postfix expression

begin   -- Assign07

   -- Loop converts infix expression to postfix expression
   -- Each iteration, converts one expression
   Convert_Expression :
   loop

      -- Get infix expression
      Put_Line ("Please enter an infix expression");
      Get_Line (Infix_Expression);
      New_Line (Spacing => 2);

      -- Exit when blank expression or null expression
      Trim (Source => Infix_Expression,
            Side   => Ada.Strings.Both);
      exit Convert_Expression when Infix_Expression = Null_Unbounded_String;

      -- Convert infix expression to postfix expression
      Postfix_Expression := To_Postfix (Infix_Expression);

      -- Display postfix expression (or error message)
      Put_Line ("Postfix Conversion:");
      Put_Line (Postfix_Expression);
      New_Line;

      -- Display original infix expression
      Put_Line ("Original Expression:");
      Put_Line (Infix_Expression);

      New_Line (Spacing => 2);

   end loop Convert_Expression;

end Assign07;