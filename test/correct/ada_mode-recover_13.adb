package body Ada_Mode.Recover_13 is

   function Apply_Rule
     (Parser : in Procedural.Parser;
      R : in Token_Id;
      P : in Token_Index)
     return Memo_Entry
   is
      M : Memo_Entry;
   begin

   end Apply_Rule;
   overriding procedure Parse (Parser : aliased in out Packrat.Parser);

end Ada_Mode.Recover_13;
