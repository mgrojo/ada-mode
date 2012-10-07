with Ada.Text_IO;
separate (Ada_Mode.Nominal)
package body Separate_Package_1 is
   -- adapted from several earlier tests
   First_Object : Integer;
   Second_Object : Integer;

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Int_IO;

   --- the following lines were wrongly indented.
   Package_Local_1 : Integer;

   procedure Separate_Procedure_1 is separate;
   procedure Separate_Procedure_2 (Item : in Integer) is separate;

begin
   First_Object := 1;
   Second_Object := 2;
   Package_Local_1 := 3;

end Separate_Package_1;
