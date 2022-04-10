-- Incremental parse used to encounter a bug in Breakdown of an
-- optimized_list. Simplified from ada_mode-interactive_02.

-- We leave the file edited on purpose, to trigger the original bug.
--EMACSCMD:(setq skip-reindent-test t)
procedure Ada_Mode.Incremental_03
is
   function Local_Function_1 return Float;

   --EMACSCMD:(progn (end-of-line 2)(forward-char -2)(execute-kbd-macro ";\r"))
   procedure Local_Proc_1 (Param_1 : in Float);

   procedure Local_Proc_2;

   procedure Function_Access_1;

   procedure New_Line_2;

   --EMACSCMD:(progn (end-of-line 2)(kill-line 3)(execute-kbd-macro "\r"))
   type Record_1 is record
      Component_1 : Integer;
   end record;

begin
   null;
end Ada_Mode.Incremental_03;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
