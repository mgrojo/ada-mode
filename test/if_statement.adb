
--  Complex if statements are incorrectly indented.

procedure If_Statement
  (A : in Boolean;
   B : in Boolean;
   C : in Boolean;
   D : in Boolean;
   E : in Boolean;
   G : in Boolean)
is
begin

   ----------------
   -- 'if' tests --
   ----------------

   if A
     or else B
     or else C
   then
      null;
   end if;

   if A
     or else (B
                and then C
                and then D)  --  Indented on 'if', instead of 'and then'
   then
      null;
   end if;

   if A
     or else (B
                and then C
                and then D)  --  Indented on 'if', instead of 'and then'
     or else ((B
                 and then C)
              or else
                (D
                   and then E))
     or else G
   then
      null;
   end if;

   -----------------
   -- while loops --
   -----------------

   while A
     or else B
   loop
      null;
   end loop;

   while A
     or else (B
                and then C
                and then D)  --  Indented on 'while' instead of 'and then'
   loop
      null;
   end loop;

   while A
     or else (B
                and then C
                and then D)  --  Indented on 'if', instead of 'and then'
     or else ((B
                 and then C)
              or else
                (D
                   and then E))
     or else G
   loop
      null;
   end loop;

   ---------------------
   -- exit statements --
   ---------------------

   loop
      exit when A
        or else B
        or else C;

      exit when A
        or else (B
                   and then C
                   and then D); --  Indented on 'exit' instead of 'and then'

      exit when A
        or else (B
                   and then C
                   and then D)  --  Indented on 'if', instead of 'and then'
        or else ((B
                    and then C)
                 or else
                   (D
                      and then E))
        or else G;
   end loop;

end If_Statement;
