--  From a real editing session; missing a ';'.
--
-- After recover, too many parallel parsers were required. Now it
-- terminates the one with the highest cost repair, and continues.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package body Ada_Mode.Recover_13 is

   function Apply_Rule
     (Parser : in Procedural.Parser;
      R : in Token_Id;
      P : in Token_Index)
     return Memo_Entry
   is
      M : Memo_Entry -- missing ';' here.
      begin

   end Apply_Rule;

   ----------
   --  Public subprograms

   overriding procedure Parse (Parser : aliased in out Packrat.Parser);

end Ada_Mode.Recover_13;
