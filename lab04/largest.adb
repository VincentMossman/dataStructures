function Largest (Values : in Array_Type) return Element_Type is

   Max : Element_Type;

begin

   Max := Values (Values'First);

   for Index in Index_Type'Succ (Values'First) .. Values'Last loop
      if Values (Index) > Max then
         Max := Values (Index);
      end if;
   end loop;

   return Max;

end Largest;

