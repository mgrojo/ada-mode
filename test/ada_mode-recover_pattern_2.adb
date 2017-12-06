--  Demonstrate recover patterns for 'end *;'
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_Pattern_2 is
   procedure Case_Example
   is begin
      case  is
         when =>
         --  end case;
      end Case_Example;

   procedure If_Example
   is begin
      if A > 0 then
         null;
         --  end if;
      end If_Example;

   procedure Loop_Example
   is begin
      loop
         null;
         -- end loop;
      end Loop_Example;

end Ada_Mode.Recover_Pattern_2;
