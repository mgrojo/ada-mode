
--  Tests the function ada-in-paramlist.
--  Up to 12/09/1999, ada-mode used to raise an error when indenting the second
--  comment line below.

procedure Paramlist is

   procedure Find --  (The_List : in T;
                  --   The_Item : in Item_T) return Natural is
   is
   begin
      null;
   end Find;
begin
   null;
end Paramlist;
