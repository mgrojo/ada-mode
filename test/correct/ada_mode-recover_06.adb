procedure Ada_Mode.Recover_6
is
begin
   declare
      Actions_Package_Name : constant String := -Data.Package_Name_Root & "_Actions";
   begin
      Create_Ada_Actions_Body (Ada_Action_Names, Ada_Check_Names);

   end;
end Ada_Mode.Recover_6;
