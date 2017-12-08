--  Demonstrate recover patterns for 'end *;'
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_Pattern_1 is
   procedure Case_Example
   is begin
      case  is
         when =>
         --  end case;
      end;

   procedure If_Example
   is begin
      if A > 0 then
         null;
         --  end if;
      end;

   procedure Loop_Example
   is begin
      loop
         null;
         -- end loop;
      end;

   function Return_Example return Integer
   is begin
      return A : Integer := 0 do
         null;
         -- end return;
      end;

end Ada_Mode.Recover_Pattern_1;
