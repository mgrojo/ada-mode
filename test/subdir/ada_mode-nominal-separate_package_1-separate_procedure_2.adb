separate (Ada_Mode.Nominal.Separate_Package_1)
procedure Separate_Procedure_2 (Item : in Integer)
is
   pragma Unreferenced (Item);
begin
   null;
end Separate_Procedure_2;
