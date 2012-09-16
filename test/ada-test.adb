-- Descr: Comments at the very beginning of the buffer (_before_ any code)
--        are not placed on the first column, as the first line of code
--        would be.
-- Fixed in: ada-mode 2.28
--EMACSCMD: (setq ada-indent-align-comments nil)


package body Ada_Mode is

   -- Integer ici est souligne
   type Type_1 is array (1 .. 10, 1 .. 10) of Integer;

   protected type Protected_1 is

      function F1 return Integer;
      entry E1 (X : Integer);
      procedure P1;

   private

      Local : Integer;

   end Protected_1;

   protected body Protected_1 is

      procedure P1 is
      begin
         null;
      end P1;

      function F1 return Integer is
      begin
         return 0;
      end F1;

      entry E1 (X : Integer) when Local = 0 is
         Tmp : Integer;
      begin
         Local := X;
      end E1;

   end Protected_1;

   --------------------------------------------------------------

   -- Ici l'exemple du chapitre 9 du RM sur le tasking

   protected Buffer is
      entry Read (C : out Character);
      entry Write (C : in  Character);
   private
      Pool      : String(1 .. 100);
      Count     : Natural := 0;
      In_Index, Out_Index : Positive := 1;
   end Buffer;

   protected body Buffer is
      entry Write(C : in Character)
      when Count < Pool'Length is
      begin
         Pool(In_Index) := C;
         In_Index := (In_Index mod Pool'Length) + 1;
         Count    := Count + 1;
      end Write;

      entry Read (C : out Character)
      when Count > 0 is
      begin
         C := Pool(Out_Index);
         Out_Index := (Out_Index mod Pool'Length) + 1;
         Count     := Count - 1;
      end Read;
   end Buffer;


begin
   null;
end Ada_Mode;

----------------------------------------------------------

separate (Parent)
package Test is
   First_Object : Integer;
   Second_Object : Integer;
end Test;
-- DONE, re

----------------------------------------------------------

package body Blabla is
   package Int_IO is new Integer_IO (Integer);
   use Int_IO;

   --- the following lines are wrongly indented.
   Test : Toto;
   -- DONE, re

end Blabla;

----------------------------------------------------------

package G is
   type T1 is new Integer;
   type T2 is new Integer;  --< incorrect, correct if subtype
   -- DONE, re
end G;

----------------------------------------------------------

-- For Ada mode w/ indent broken set to 2, I get the following indentation
-- The following two lines seem incorrect as integer is indented 3 spaces

package Test is

   Toto : Integer;
   type X_Type is access
     Integer;
   Toto : Integer;
end Test;
-- DONE, re

procedure Test_Tt_Low(  );
function Toto (HIGH : in Integer ) return Res;

-------------------
-- For tagged types the problem comes from the keyword abstract:

procedure Toto is
   type T2 is limited abstract tagged record
      X : Integer;
      Y : Float;
   end record;
begin
   null;
end Toto;
-- DONE

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

-------------------
-- Wrong indentation depending on the presence or absence of white space
-- at another line (????)
procedure X is

   package DB is
      procedure X;
   end DB;

   package Truc is
      procedure X;
   end Truc;

   package body DB is separate;

   package body TRUC is separate; -- this line gets indented as the nr of space character
   -- present on the line **before line the **  package body DB
   -- e.g. 4 spaces there => 4 spaces here

begin
   null;
end X;
-- (in other words, put e.g. 10 spaces on the line before the line package
-- body DB is separate,
-- then go to package body TRUC line
-- then press TAB -> package body TRUC will be indented to column 10.

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
