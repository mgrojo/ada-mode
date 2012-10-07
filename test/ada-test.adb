-- FIXME: split out into separate files, so we can check syntax

-- Show we can handle incorrect code that someone is likely to type
procedure Test_Tt_Low(  );
function Toto (HIGH : in Integer ) return Res;

-------------------
-- If I do the following I get
-- "no matching procedure/function/task/declare/package"
-- when I do return (I reverse the mappings of ^j and ^m) after "private".
package Package1 is
   package Package1_1 is
      type The_Type is private;
   private
      null;
   end Package1_1;
end Package1;
--DONE, Re

-------------------
-- But what about this:
package G is
   type T1 is new Integer;
   type T2 is new Integer;  --< incorrect, correct if subtype
   package H is
      type T3 is new Integer;
      type T4;
   end H;                    --< Indentation is incorrect
end G;
-- DONE, re

-----------------------

-- type the following and you will get an error when you indent after a
-- return after is keyword (missing when between case and =>

procedure Y is
begin
   case X(A => A, B => C) is
      when '3' => null;
   end case;
end Y;
-- Done, RE
-----------------------

procedure A is
   package B is
      new C(D => E);

   procedure F (G : in out H); -- <<<<<< wrongly indented
begin
   null;
end A;
-- done
-----------------------

package Test is
   -- If I hit return on the "type" line it will indent the next line
   -- in another 3 space instead of heading out to the "(". If I hit
   -- tab or return it reindents the line correctly but does not initially.
   type Wait_Return is (Read_Success, Read_Timeout, Wait_Timeout,
                        Nothing_To_Wait_For_In_Wait_List);
   -- FIXME: need test of active <return>
   -- OPEN

   -- The following line will be wrongly reindented after typing it in after
   -- the initial indent for the line was correct after type return after
   -- this line. Subsequent lines will show the same problem.
   Unused:    constant Queue_ID := 0;
end Test;
-------------------
procedure Test is
begin
   Main_Unit              := False;
   begin
  Make_Loop : while not Empty_Q loop
         Need_To_Compile  := False;
         null;
         null;
      end loop;
      null;
      null;
   end;
end Test;

-----------------------

function Foo return Token is
begin
   case Bar is
      when '(' =>
         return Lparen;
      when others =>
         null;
   end case;
end Foo;
-----------------------
