with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Encryption_Tools; use Encryption_Tools;

procedure Methcrypt is

   Fuck_Stick : String (1 .. 5);

begin

   Fuck_Stick := Encrypt_Phrase (Item => "Balls",
                                 Last => 5);

   Ada.Text_IO.Put (Fuck_Stick (1 .. 5));

end Methcrypt;