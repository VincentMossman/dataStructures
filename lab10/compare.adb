with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Square_Path;
with Ada.Real_Time;
with Big_Natural;    use Big_Natural;
with Big_Natural.IO; use Big_Natural.IO;
use type Ada.Real_Time.Time;

procedure Compare is

   -- For the input and output of time
   package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);

   Grid_Size : Natural;   -- Dimension of a square grid
   Num_Paths : Big_Natural.Big_Natural;   -- Result

   Start     : Ada.Real_Time.Time;  -- Times that we start and
   Finish    : Ada.Real_Time.Time;  -- finish execution of a procedure

begin
   delay 0.3;
   Input_Loop :  -- Calculate all the values for the user
   loop          -- Each iteration, compute number of paths for one square grid

      Ada.Text_IO.Put ("Enter a grid size or 0 to quit ");
      Ada.Integer_Text_IO.Get (Grid_Size);
      Ada.Text_IO.New_Line;
      exit Input_Loop when Grid_Size = 0;


--      Ada.Text_IO.Put_Line ("Path Algorithm 1");
--      Ada.Text_IO.Put ("   Number of paths           - ");
--      Start := Ada.Real_Time.Clock;
--      Num_Paths := Square_Path.Path_Algorithm_1 (Grid_Size);
--      Finish := Ada.Real_Time.Clock;
--      Ada.Integer_Text_IO.Put (Item  => Num_Paths,
--                               Width => 1);
--      Ada.Text_IO.New_Line;
--      Ada.Text_IO.Put ("   Number of recursive calls - ");
--      Ada.Integer_Text_IO.Put (Item  => Square_Path.Count_Algorithm_1,
--                               Width => 1);
--      Ada.Text_IO.New_Line;
--      Ada.Text_IO.Put ("   Number of milliseconds    - ");
--      Duration_IO.Put (Item => 1000 * Ada.Real_Time.To_Duration (Finish - Start),
--                       Fore => 1, Aft => 4, Exp => 0);
--      Ada.Text_IO.New_Line (2);


      Ada.Text_IO.Put_Line ("Path Algorithm 2");
      Ada.Text_IO.Put ("   Number of paths           - ");
      Start := Ada.Real_Time.Clock;
      Num_Paths := Square_Path.Path_Algorithm_2 (Grid_Size);
      Finish := Ada.Real_Time.Clock;
      Put (Item  => Num_Paths,
                    Width => 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("   Number of recursive calls - ");
      Put (Item  => Square_Path.Count_Algorithm_2,
           Width => 1);
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("   Number of milliseconds    - ");
      Duration_IO.Put (Item => 1000 * Ada.Real_Time.To_Duration (Finish - Start),
                       Fore => 1, Aft => 4, Exp => 0);
      Ada.Text_IO.New_Line (4);

   end loop Input_Loop;
end Compare;