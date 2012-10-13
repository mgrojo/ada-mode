
--  7301-003:  Problem with which-func-mode reporting an incorrect
--  function.
--  FIXME: need automated test for which-func-mode

procedure Indent10 is

   procedure Tutu is
   begin
      null;
   end Tutu;

   procedure T (A : Integer;
                B : Integer;
                C : Integer)
   is
      procedure T2 is
         procedure T4;
         procedure T5;
         procedure T3 is
         begin
            null;
         end T3;
         procedure T4 is begin null; end;
         procedure T5 is begin null; end;
      begin
         null;
      end T2;
   begin
      null; -- here we have [Toto]
   end T;

   generic
   procedure Gene;
   procedure Gene is begin null; end;

   procedure Titi is
      procedure Tata is new Gene;
   begin
      null; --  here we have [Tata]
   end Titi;

begin
   null;  --  here we have [Tata]
end Indent10;
