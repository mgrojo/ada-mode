-- A comment before the first code

package body Ada_Mode.Nominal is

   -- Integer ici est souligne (Integer is highlighted here)
   type Type_1 is array (1 .. 10, 1 .. 10) of Integer;

   protected type Protected_1 is

      function F1 return Integer;
      function F2 (A : Float; B : Float) return Float;
      entry E1 (X : Integer);
      procedure P1;
      procedure P2 (A : Float; B : Float);

      -- This is a comment just before 'private'; default smie
      -- indentation doesn't do what we want here.
   private

      -- More than three objects, to be sure we are handling
      -- indefinite lists of objects properly
      Local_1 : Integer;
      Local_2 : Integer;
      Local_3 : Integer;
      Local_4 : Integer;

      -- A comment just before 'end'
   end Protected_1;

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
end Ada_Mode.Nominal;
