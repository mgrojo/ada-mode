-- Simplified from ada_mode-long_parens.adb; found an infinite loop
-- in incremental parse due to a grammar conflict.
--EMACS_SKIP_UNLESS: wisi-incremental-parse-enable
procedure Ada_Mode.Incremental_01
is begin
   New_Symbol
     (Module,
      Format => Gds);
end Ada_Mode.Incremental_01;
