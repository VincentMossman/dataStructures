with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;
package body Heart is

   package Reading_IO is new Ada.Text_IO.Integer_IO (Num => Reading);

   Patient : Ada.Text_IO.File_Type;  -- For testing, use a file of data
   --                                -- instead of a real patient

   ----------------------------------------------------------------------------
   procedure Initialize (Patient_Name : in String) is
   begin
      Ada.Text_IO.Open (File => Patient,
                        Mode => Ada.Text_IO.In_File,
                        Name => Patient_Name);
   end Initialize;

   ----------------------------------------------------------------------------
   function Sense return Reading is
      Value : Reading;
   begin
      Reading_IO.Get (File => Patient,
                      Item => Value);
      delay 0.05;
      return Value;
   exception
      when CONSTRAINT_ERROR | Ada.IO_Exceptions.End_Error =>
         raise SENSE_ERROR;
      when Ada.IO_Exceptions.Data_Error =>
         Ada.Text_IO.Skip_Line (Patient);
         raise SENSE_ERROR;
   end Sense;


   ----------------------------------------------------------------------------
   procedure Shock is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("CLEAR!   ");
      delay 0.5;
      for Count in 1 .. 5 loop
         Ada.Text_IO.Put (Ada.Characters.Latin_1.BEL);
         delay 0.3;
      end loop;
      Ada.Text_IO.New_Line;
   end Shock;

   ----------------------------------------------------------------------------
   function Insured return Boolean is
   begin
      return not Ada.Text_IO.End_Of_File (Patient);
   end Insured;

end Heart;
