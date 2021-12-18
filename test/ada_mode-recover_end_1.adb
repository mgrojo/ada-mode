--  Demonstrate recovery for incorrect 'end *;'
--
-- Compare to ada_mode-recover_end_2.adb; that has matching end names
-- present.
--
-- We get different indent results from partial and incremental parse;
--EMACSCMD:(setq skip-reindent-test (not wisi-incremental-parse-enable))

--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_End_1 is
   procedure Case_Example
   is begin
      case A is
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
-- Local Variables:
-- ada-end-name-optional: t
-- compare-tree-text: nil
-- End:
