function Adacore_Bb04_004 return Boolean is
begin
   Loop1:
   for F in Integer loop
      case F is
         when 1 =>
            null;

         when 2 =>
            Loop2:
            for K in Integer loop
               null;
            end loop Loop2;

         when others =>
            null;
      end case;
   end loop Loop1;
   return True;
end Adacore_Bb04_004;
-- Local Variables:
-- ada-indent-label: 0
-- End:
