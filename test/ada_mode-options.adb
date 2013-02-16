--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
with
    Ada.Text_IO;
package body Ada_Mode.Options is

   type Discrete_Type_1 is (A, B, C);
   Local_1 : Discrete_Type_1 := C;

   procedure Label is
      I : Integer := 1;
   begin
      <<Start>> -- no statement between 'begin' and label
      I := I + 1;
   end Label;

begin

   case Local_1 is
   -- comment after "is", before "when"
   when A =>
      Local_1 := B;

   when B =>
      Local_1 := C;

   when C =>
      Local_1 := A;

   end case;

   Label_1 :
   declare
      Local_2 : Integer;
   begin
      Local_1 := C;
      <<Label_2>>
      Local_2 := 3;
   end Label_1;

   Loop1:
   for F in Integer loop
      null;
   end loop Loop1;
end Ada_Mode.Options;
-- Local Variables:
-- ada-indent-when: 0
-- ada-indent-label: 0
-- ada-indent-with: 4
-- End:
