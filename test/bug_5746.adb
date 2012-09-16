procedure Bug_5746 is

   A : Integer := 1;

   function Current_Item_Ptr (It : Integer) return Integer is
      C : Integer
        --  This "renames" was indented incorrectly
        renames A;
   begin
      return 0;
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Integer) is
      C : Integer
        renames A;
   begin
      null;
   end Delete_Item_At;
begin
   null;
end Bug_5746;
