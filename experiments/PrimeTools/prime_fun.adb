with Prime_Tools; use Prime_Tools;
with Ada.Integer_Text_IO;
with Ada.Text_IO;

procedure Prime_Fun is

-----------------------------------------------------------------------
-- OPTIONS
-----------------------------------------------------------------------

   procedure Prime_Pair_Experiment is
   begin
      null;
   end Prime_Pair_Experiment;

-----------------------------------------------------------------------
-- MAIN MENU
-----------------------------------------------------------------------

   procedure Display_Menu is

   begin   -- Display_Menu

      Ada.Text_IO.Put_Line ("**********************************");
      Ada.Text_IO.Put_Line ("***            MENU            ***");
      Ada.Text_IO.Put_Line ("**********************************");
      Ada.Text_IO.Put_Line ("*** 1.) Prime Pair Experiment  ***");
      Ada.Text_IO.Put_Line ("*** 2.) Exit                   ***");
      Ada.Text_IO.Put_Line ("**********************************");

   end Display_Menu;

-----------------------------------------------------------------------

   User_Menu_Choice : Integer;

begin   -- Prime_Fun

   Prime_Fun_Loop :
   loop

      -- Display Menu
      Display_Menu;

      -- Get User Choice
      Ada.Integer_Text_IO.Get (User_Menu_Choice);

      -- Determine User Choice
      case User_Menu_Choice is
         when 1 =>
            Prime_Pair_Experiment;
         when 2 =>
            exit Prime_Fun_Loop;
         when others =>
            Ada.Text_IO.Put_Line ("Invalid Selection");
      end case;

   end loop Prime_Fun_Loop;

end Prime_Fun;