--  From a real editing session.
--
--  Now finds a good solution reasonably quickly.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq wisi-indent-region-fallback nil)
package Ada_Mode.Recover_5 is

   type Result (Label : Result_Label) is record
      case Label is
         when Success =>
            Value     : Integer;
            Remaining : Ada.Strings.Unbounded.Unbounded_String;
         when Failure =>
         null;
   --  Missing 'end case; end record;' here.

   function Additive (Input : String) return Result;

end Ada_Mode.Recover_5;
