
--  We test the select statement in this package, with the different
--  possible constructions

--EMACSCMD:(jit-lock-fontify-now)
procedure Test_Select is

   subtype Entry_Range is Integer range 1 .. 10;

   task type Task_1 (D : Integer) is
      --EMACSCMD:(test-face "E1" 'font-lock-function-name-face)
      entry E1;
      --EMACSCMD:(test-face "E2" 'font-lock-function-name-face)
      entry E2 (X : in Integer);
      --EMACSCMD:(test-face "E3" 'font-lock-function-name-face)
      --EMACSCMD:(test-face "Entry_Range" 'font-lock-type-face)
      entry E3 (Entry_Range) (A,B : Integer);
   private
      entry E4;
   end Task_1;

   procedure A is
   begin
      null;
   end A;

   task body Task_1 is
      Local : Integer := 0;
   begin
      select
         --EMACSCMD:(test-face "E1" 'font-lock-function-name-face)
         accept E1;

      or
         when Local = 0 =>
            --EMACSCMD:(test-face "E2" 'font-lock-function-name-face)
            accept E2 (X : in Integer) do
               Local := X;
               --EMACSCMD:(test-face "E2" 'font-lock-function-name-face)
            end E2;
      or
         --EMACSCMD:(test-face "E3" 'font-lock-function-name-face)
         accept E3 (1) (A,B : Integer) do
            Local := A +B;
            --EMACSCMD:(test-face "E3" 'font-lock-function-name-face)
         end E3;
      else
         loop
            Local := Local + 1;
            accept E4;
         end loop;
      end select;
   end Task_1;

begin

   select
      delay 1.0;
   then abort
      null;
   end select;

   select
      delay 1.0;
   then
     abort -- ada-mode 4.01 broken indent
      null;-- ada-mode 4.01 gets this wrong; it uses another broken indent.
   end select;

end Test_Select;
