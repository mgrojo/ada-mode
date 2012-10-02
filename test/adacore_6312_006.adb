--  Descr: Indentation problems with "begin .. end" blocks with no
--         "declare" section. The begin is then aligned on the first
--         column.
--  reported by: ACT customer (6312-006)
--  Date: Fri, 12 Mar 1999 11:19:53 -06:00
--  Fixed in: ada-mode 3.2

procedure Adacore_6312_006 (X, Y : in Character; I, J : out Integer) is
begin
   case X is
      when 'a' =>
         if Y in '0' .. '9' then
            begin  --  used to be uncorrectly indented
               I := Character'Pos (Y);
            end;
         end if;
      when 'b'=>
         declare
         begin  --  was correctly indented when 'declare' is found
            I := Character'Pos (Y);
         end;
         J := 0;
      when others =>
         null;
   end case;
end Adacore_6312_006;
