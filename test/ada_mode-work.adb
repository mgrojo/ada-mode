-- Comments at the very beginning of the buffer (_before_ any code)
--EMACSCMD: (setq ada-indent-align-comments nil)


package body Ada_Mode.Work is

   -- Integer ici est souligne (Integer is highlighted here)
   -- FIXME: highlighting is not checked in the automated test
   type Type_1 is array (1 .. 10, 1 .. 10) of Integer;

   protected type Protected_1 is

      function F1 return Integer;
      function F2 (A : Float; B : Float) return Integer;
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

      function F2 (A : Float; B : Float) return Float
      is begin
         return C : Float do
            C := (A * B);
            C := C * C;
         end return;
      end; -- no F2 on purpose

      entry E1 (X : Integer) when Local = 0 is
         Tmp : Integer := 0;
      begin
         Local := X + Tmp;
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
end Ada_Mode.Work;
