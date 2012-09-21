-- A comment before the first code
--
-- Indenting nested bodies is covered more thoroughly in FIXME:

with Ada.Strings.Unbounded;
package body Ada_Mode.Nominal is

   type Incomplete_Type_1 (Discriminant_1 : Integer) is tagged null record;

   type Incomplete_Type_2 (Discriminant_1 : Integer) is tagged null
   record;

   type Incomplete_Type_3 (Discriminant_1 : Integer) is tagged
null record;
   type Incomplete_Type_4 (Discriminant_1 : Integer) is
      tagged null record;
   -- rest of newline placement covered in spec

   type Incomplete_Type_5 (Discriminant_1 : access Integer) is tagged record
      Component_1 : integer;
      Component_2 :
         integer;
      Component_3
      : integer;
   end record;

   protected body Protected_1 is

      function F1 return Integer is
      begin
         return 0;
      end F1;

      function F2 (A : Float; B : Float) return Float
      is begin
         return C : Float do
            C := (A * B);
            C := C * C;
         end return;
      end; -- no F2 on purpose

      entry E1 (X : Integer) when Local_1 = 0 is
         Tmp : Integer := 0;
      begin
         Local_1 :=
            X + Tmp; -- an indented line

         -- A comment after an indented line

      end E1;

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

   ----------
   -- subprograms
   procedure Procedure_1a (Item  : in out Ada.Strings.Unbounded.Unbounded_String; New_Item : Character)
      is begin
         null;
         end Procedure_1a;
   procedure Procedure_2a is begin null; end;

   -- Functions can't be 'is null', so we test some indentation issues
   function Function_1a return Float
   is begin
      return 1.0;
   end Function_1a;

   function Function_1b return Float
   is
      Local_1 : constant := 3.0;
   begin
      return Local_1;
   end
      Function_1b;
      
   function Function_1c return Float
   is
      Local_1 : constant Float := 2.0;
      Local_2 : constant Float
         := Local_1;
      Local_3 : constant Float :=
         Local_2;
      -- Comment
   begin
      return
         Local_3;
   end;
   
   function Function_1d return Float is begin return 1.0; end;

   function Function_2a (Param : in Integer) return Float is begin return Float (Param); end;
   function Function_2b (Param : in Integer) return Float
   is begin
      return Float (Param);
   end;
   function Function_2c (Param : in Integer) return Float
   is begin
      return Float
         (Param);
   end;
   function Function_2d (Param : in Integer) return Float
   is
   begin
      return
         Float (Param);
   end;
   function Function_2e (Param : in Integer) return Float is begin return Float (Param); end;
   
begin
   null;
end Ada_Mode.Nominal;
