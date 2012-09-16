
--  Incorrect indentation of loop with adamode-3.4a
--  Works correctly with adamode-3.4b

procedure Loop_1 is
   task Foo is
      entry BILLY;
      entry SILLY;
      entry LILLY;
   end Foo;

   task body FOO is
   begin
      loop
         select
            accept BILLY;
            null;
         or
            accept SILLY;
            null;
         or
            accept LILLY do
               null;
            end LILLY;
         end select;
      end loop;
   end FOO;
begin
   null;
end Loop_1;
