--EMACSCMD:(wisi-prj-select-cache "ada_mode.adp" (ada-prj-default))

-- test "Text_IO" case exception set in project file
--EMACSCMD:(progn (forward-line 1)(forward-word 2)(downcase-word 2)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "Text_IO")))
with Ada.Text_IO;
--EMACSRESULT:t
-- Other file from a subunit on a context clause.
--EMACSCMD:(progn (forward-line -3)(ada-find-other-file)(looking-at "package Ada.Text_IO"))
--EMACSRESULT:t

--  WORKAROUND: GNAT GPL 2016 doesn't produce a .ali file for this
--  file, so gpr_query doesn't work. And gnat find doesn't work either
--
--  EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "Separate_Package_1 is"))
separate (Ada_Mode.Nominal)
package body Separate_Package_1 is
   --  EMACSRESULT:t

   -- adapted from several earlier tests
   First_Object : Integer;

   -- test "ANother" partial-word case exception set in project file
   --EMACSCMD:(progn (forward-line 1)(downcase-word 2)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "ANother_Object")))
   ANother_Object : Integer;
   --EMACSRESULT:t

   -- again, as last partial-word
   --EMACSCMD:(progn (forward-line 1)(downcase-word 2)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "Object_ANother")))
   Object_ANother : Integer;
   --EMACSRESULT:t
   pragma Unreferenced (Object_ANother);

   -- other file from a random place in the body
   -- WORKAROUND: see above
   -- EMACSCMD:(progn (forward-line 1)(ada-find-other-file)(looking-at "Separate_Package_1 is"))
   -- EMACSRESULT:t

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
