--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "package Ada_Mode"))
procedure Ada_Mode.Child_Procedure
is
begin
   null;
end;
