--  From a real editing session; used to raise an exception.
procedure Wisi.Output_Ada
is

begin

   declare
   Actions_Package_Name : constant String := -Data.Package_Name_Root & "_Actions";
begin
   Create_Ada_Actions_Body (Ada_Action_Names, Ada_Check_Names);

end Wisi.Output_Ada;
