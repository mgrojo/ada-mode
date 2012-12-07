--EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "ada_mode.adp")

-- test "Text_IO" case exception set in project file
--EMACSCMD:(progn (forward-line 1)(forward-word 2)(downcase-word 2)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "Text_IO")))
with Ada.Text_IO;
--EMACSRESULT:t

--EMACSCMD:(progn (forward-line 1)(ada-find-other-file t)(looking-at "package body Separate_Package_1 is separate"))
separate (Ada_Mode.Nominal)
package body Separate_Package_1 is
   --EMACSRESULT:t

   -- adapted from several earlier tests
   First_Object : Integer;

   -- test "ANother" partial-word case exception set in project file
   --EMACSCMD:(progn (forward-line 1)(downcase-word 2)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "ANother_Object")))
   ANother_Object : Integer;
   --EMACSRESULT:t

   -- again, as last partial-word
   --EMACSCMD:(progn (forward-line 1)(downcase-word 2)(ada-case-adjust)(let ((case-fold-search nil))(looking-back "Object_ANother")))
   Object_ANother : Integer;
   --EMACSRESULT:t

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   use Int_IO;

   --- the following lines were wrongly indented.
   Package_Local_1 : Integer;

   procedure Separate_Procedure_1 is separate;
   procedure Separate_Procedure_2 (Item : in Integer) is separate;

begin
   First_Object := 1;
   ANother_Object := 2;
   Package_Local_1 := 3;

end Separate_Package_1;
