--  Demonstrate recovery for incorrect 'end *;'
--
-- Compare to ada_mode-recover_end_2.adb; that has matching end names
-- present.
--
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_End_1 is
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

end Ada_Mode.Recover_End_1;
-- Error recovery has a race condition; force it to return repeatable results
-- Local Variables:
-- wisi-mckenzie-task-count: 1
-- End:
