-- A comment before the first code
--
-- Indenting nested bodies is covered more thoroughly in FIXME:
-- Indenting expressions is covered more thoroughly in FIXME:

-- FIXME: add a function body that returns an access type

with Ada.Strings; -- test two context clauses
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body Ada_Mode.Nominal is

   use Ada.Strings;

   type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record;

   type Incomplete_Type_2 (Discriminant_1 : Integer) is tagged null
      record;

   type Incomplete_Type_3 (Discriminant_1 : Integer) is tagged
     null record;

   type Incomplete_Type_4 (Discriminant_1 : Integer) is
     tagged null record;
   -- rest of newline placement covered in spec

   type Incomplete_Type_5 (Discriminant_1 : access Integer) is tagged record
      -- "record" on same line as "type"; components indented 3 relative to "type"
      Component_1 : Integer;
      Component_2 :
        Integer;
      Component_3
        : Integer; -- don't care
   end record;

   protected
     body Protected_1 is
      -- don't care about the indentation of 'body' (nobody should use
      -- this style); testing that 'protected body' is not a combined
      -- token

      function F1 return Integer is
         -- some people like 'is' on the line with 'function' here

         function Local_Function return Integer
         is
            Bad_Thing : exception;
            Dummy : Boolean;
            pragma Unreferenced (Dummy); -- test ada-indent-statement-or-declaration handling of this in refine-begin
         begin
            if True then
               begin
                  return Integer (Function_1a);
               exception
                  when E : Constraint_Error =>
                     return 0;
                  when
                    Bad_Thing -- ada-mode 4.01 indentation
                    =>        -- ""
                     return 0;
                  when
                    E : others =>
                     return 1;
               end;
            elsif False
            then
               return 1;   -- a comment
                           -- another comment, aligned with previous
            else
               return 0;
            end if;
         end Local_Function;
      begin
         return B : Integer := Local_Function;
         -- non-do extended return
      end F1;

      function F2 (Param_1 : Discrete_Type_1; B : Float) return Float
      is
      begin
         return D : Float
         do
            -- extended return with do
            case Param_1 is
               when A | Nominal.B =>
                  P1;
               when C =>
                  D := B;
                  D := D + B * B;
               <<Label_1>>
                  D := D - Float (F1);
            end case;
         end return;
      end; -- no F2 on purpose

      entry E1 (X : Integer) when Local_1 = 0 is
         Tmp : Integer := 0;
         Local_4 : Discrete_Type_1 := A;
      begin
         Local_1 :=
           X + Tmp;

         -- A comment after a hanging line

         case Local_4 is
            when
              A | -- continuation line; ada-indent-broken = 2
              B |
              C
              => -- Ada mode 4.01 indentation
               for I in 1 .. 10 loop
                  Local_1 := Local_1 + Local_1;
               end loop;

            Loop_1 :
               loop
                  Local_1 := Local_1 + Local_1;
                  exit Loop_1 when Local_1 > 0;
               end loop Loop_1;

               loop
                  Local_1 := Local_1 + Local_1;
                  exit when Local_1 > 0;
               end loop;

            Loop_4 : while not (Local_1 > 0) loop
               -- Sort of wrong, but this is what Ada-mode 4.01 does.
               -- Don't put loop label on the same line as loop if
               -- ada-indent-label is not 0.
               Local_1 := Local_1 + 2;
            end loop Loop_4;

         Loop_5 : -- wrong because of previous line
            while not (Local_1 > 0) loop
               Local_1 := Local_1 + 2;
            end loop Loop_5;
         end case;

         -- A comment before 'end'
      end E1;

      entry E2
        (X : Integer)
        -- an expression with 'not' to see if we need that in the
        -- grammar (conflicts with 'not null')
        when Local_1 = 0 and not -- Ada mode 4.01 screws this up.
        (Local_2 = 1)
      is
         Tmp : Integer := 0;
      begin
         Local_2 := Tmp;
      end E2;

      procedure P1 is
      begin
         null;
      end P1;

      procedure P2 (A : Float; B : Float)
      is begin
         null;
      end; -- no P2
   end Protected_1;

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

   --------------------------------------------------------
   --  6804-008: problem for indentation after a task declaration
   --  The problem was caused by the task declaration with no
   --  block attached
   --------------------------------------------------------
   task Executive;
   task body Executive is
   begin
      null;
   end Executive;

   -- a more typical task
   task body Task_Type_1 is
      Local_1 : Integer;
      Started : Boolean := False;
   begin
      select
         accept Start (A) (Param_1 : in integer);
         Started := True;
      or
         when Started => -- Ada mode 4.01 ada-when-indent
            accept Middle_1 (Param_1 : in integer) do
               Local_1 := 0;
            end Middle_1;
      or
         when Started =>
            accept Middle_2
              (Param_1 : in integer);
            Local_1 := 0;

      or when Started
        =>
         accept Finish; -- Ada mode 4.01
         local_1 := 5;
      or
         terminate;
      end select;

      select -- need a separate select to test "else"
         accept Start (A) (Param_1 : in integer);
         Local_1 := 0;
      else
         Local_1 := 2;
      end select;
   end Task_Type_1;

   ----------
   -- subprograms

   procedure Procedure_1a (Item  : in out Parent_Type_1)
   is begin
      null;
   end Procedure_1a;
   procedure Procedure_2a is begin null; end;

   -- Functions can't be 'is null', so we test some indentation issues
   function Function_1a return Float
   is begin
      Procedure_2a;
      Procedure_2a;
      return 1.0;
   end Function_1a;

   function Function_1b return Float
   is
      Local_1 : constant := 3.0;
   begin
      declare -- no label, zero statements between begin, declare
      begin
         return Local_1;
      exception
         when others =>
            return 0.0;
      end;
   end
     Function_1b;

   function Function_1c return Float
   is
      Local_1 : constant Float := 2.0;
      Local_2 : constant Float
        := Local_1;
      Local_3 : constant Float :=
        Local_2;
      -- Comment after indented line
      Item : Parent_Type_1;
   begin
      Procedure_1a
        (Item);
      declare -- no label, one statement between begin, declare
      begin
         return
           Local_1 +
           Local_2 +
           Local_3;
      end;
   end;

   function Function_1d return Float
   is begin
      Procedure_2a
        ; -- hanging statement, to test that declare does not indent on it
      Procedure_2a;

      declare -- no label, two statements between begin, declare
      begin
         return 1.0;
      end;
   end;

   function Function_2a (Param : in Parent_Type_1) return Float
   is begin
   Block_1:
      declare -- label, no statements between begin, label
         Local_1 : Float := 1.0e-36;
         Local_2 : Integer := 2;
         Local_3 : Character := '3';
      begin
         Local_1 := 2.0;
         Local_2 := 3;
         Local_3 := '4';
         return 1.0;
      end Block_1;
   end;

   function Function_2b (Param : in Parent_Type_1) return Float
   is
   begin
      Procedure_2a;
   Block_1:
      declare -- label, one statements between begin, label
         Local_1 : Float := 1.0e-36;
         Local_2 : Integer := 2;
         Local_3 : Character := '3';
      begin
         Local_1 := 2.0;
         Local_2 := 3;
         Local_3 := '4';
         return 1.0;
      end Block_1;
   end;

   function Function_2c (Param : in Parent_Type_1) return Float
   is
      type Array_Type_1 is array (1 .. 3) of Float;
      type Array_Type_2 is array (1 .. 3) of Array_Type_1;
      Local_A : Array_Type_2 := (others => (others => 0.0));
   begin
      Procedure_2a
        ;

      -- second begin block FIXME: this comment is indented wrong!
      begin
         Local_a (1)(2) := 1.0;
      end;

   Block_1 :
      declare -- label, two statements between begin, label
         Local_1 : constant Float := Local_A (1)(2); -- test that refine-begin can skip parens
      begin
         return Local_1;
      end Block_1;
   end;

   function Function_2d (Param : in Parent_Type_1) return Float
   is
   begin
      begin -- no declare, no statements
         return
           1.0;
      end;
   end;

   function Function_2e
     (Param : in Parent_Type_1)
     return Float
   is begin
      Procedure_2a;
      begin -- no declare, one statement
         return 1.0;
      end;
   end;

   function Function_2f (Param : in Parent_Type_1)
     return Float is
   begin
      Procedure_2a
        ;
      Procedure_2a;
      begin -- no declare, two statements
         return 1.0;
      end;
   end;

   package body Separate_Package_1 is separate;

begin
   null;
end Ada_Mode.Nominal;
