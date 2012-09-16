--  8126-015. The second line in a parameter list was indented
--  relative to the opening parenthesis, and after adding ada-broken-indent.
--  It seems much more usual to indent in on the same colomn as the first
--  line after the parenthesis.

procedure Pragma_Indent is

   --  declaration of multiple parameters with the same type; ada-broken-indent
   procedure Foo (C,
                    Toto : Integer);

   procedure Foo (C, Toto : Integer) is begin null; end;

   procedure Toto;

   --  pragma parameter list; same as procedure call, not ada-broken-indent
   pragma Import (C, Toto,
                  "toto");
begin
   --  procedure call; not ada-broken-indent
   Foo (1,
        2);
end Pragma_Indent;
