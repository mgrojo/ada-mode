-- From a real editing session; used to raise an exception, now finds
-- a good solution quickly.

--EMACSCMD:(setq wisi-indent-region-fallback nil)
procedure Ada_Mode.Recover_6
is
begin
   declare
      Actions_Package_Name : constant String := -Data.Package_Name_Root & "_Actions";
   begin
      Create_Ada_Actions_Body (Ada_Action_Names, Ada_Check_Names);

   --  Missing 'end;'. Indent is consistent with extending block body.
end Ada_Mode.Recover_6;
-- Local Variables:
-- ada-end-name-optional: nil
-- End:
