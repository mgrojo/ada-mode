
procedure Adacore_7414_005 is
   --  First line blank used to cause problems
   procedure Read is
      procedure P is

         procedure Parse (Tag : in String) is
         begin
            if Tag = "123456" then
               null;
            end if;
         end Parse;

      begin
         null;
      end P;

   begin  --  incorrectly indented
      null;
   end Read;  --  Error when pressing enter
begin
   null;
end Adacore_7414_005;
