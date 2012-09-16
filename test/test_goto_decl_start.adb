--  restore defaults: -*- ada-when-indent: 3, ada-label-indent: -4 -*-

--  Test several cases in ada-goto-matching-decl-start. In particular,
--  all the possible paths thru the "is" branch of "calculate
--  nest-depth", since that was modified significantly in the fix for
--  bug#5400.

--EMACSCMD: (setq ada-when-indent 0)
procedure Test_Goto_Decl_Start is

   procedure Test1 is null;

   subtype Small_Integer is Integer range 1 .. 10;
   --  Encounters "is" | "subtype"

   generic
      type Foo_Type (<>) is private;
      with function "=" (Left, Right : in Foo_Type) return Boolean is <>;
   procedure Gen_Transform (Foo : in out Foo_Type);

   procedure Gen_Transform (Foo : in out Foo_Type)
   is begin
      null;
   end;

   procedure Transform is new Gen_Transform (Integer);

   function Foo_P (A : in Integer) return Boolean;

   function Foo_P (A : in Integer) return Boolean
   is begin
      return A > 0;
   end Foo_P;

   protected type Protected_B is
      procedure Foo;
   private
      --  ada-move-to-end here encounters "is" | "protected type"
      --EMACSCMD: (progn (ada-move-to-end) (looking-at "Protected_B"))
      --EMACSRESULT: t
      Handle : Integer;
   end Protected_B;

   protected body Protected_B is
      procedure Foo is begin null; end;
   end Protected_B;

   B : Integer := 1;
begin

   case Test_Goto_Decl_Start.Foo_P -- possible comment
     (B) -- another possible comment
      is
   when True =>
      --  ada-move-to-end here encounters "is" | "case"
      --EMACSCMD: (progn (ada-move-to-end) (backward-word 1) (looking-at "case"))
      --EMACSRESULT: t
      Transform (B);
   when False =>
      null;
   end case;

end Test_Goto_Decl_Start;

--  <tab> here encounters:
--  "is" | "right is" in a recursive call on 'is begin' in Foo_P body.
--  "is" | generic formal on 'Boolean is' in Transform declaration.
--  "is" | type on 'type Foo_Type' in Transform declaration
--  "is" | null procedure on Test1

-- Local Variables:
-- eval: (test-ada-move-to-end)
-- end:
