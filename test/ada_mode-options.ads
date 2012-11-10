-- Root of files testing non-default options settings
--
-- See Local Variables at end of file for which options are tested in each file.
--
-- Verify that Local Variables work (they do if they are marked safe):
--EMACSCMD: ada-indent-record-rel-type
--EMACSRESULT: 0
package Ada_Mode.Options is
   pragma Elaborate_Body (Options);

   type Private_Type_1 is tagged
   record
      Component_1 : Integer;
   end record;

   type Derived_Type_1 is
   new Private_Type_1 with record
      Component_2 : Integer;
   end record;

end Ada_Mode.Options;
-- Local Variables:
-- ada-indent-record-rel-type: 0
-- End:
