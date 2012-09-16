
--  We test the select statement in this package, with the different
--  possible constructions

procedure Test_Select is
   task type Task_1 (D : Integer) is
      entry E1;
      entry E2 (X : in Integer);
      entry E3 (Positive) (A,B : Integer);
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
         accept E1;
      or
         when Local = 0 =>
            accept E2 (X : in Integer) do
               Local := X;
            end E2;
      or
         accept E3 (1) (A,B : Integer) do
            Local := A +B;
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

end Test_Select;
