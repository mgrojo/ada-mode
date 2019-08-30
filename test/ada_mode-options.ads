-- Root of files testing non-default options settings
--
-- See Local Variables at end of file for which options are tested in each file.
--
-- Verify that Local Variables work (they do if they are marked safe):
--EMACSCMD: ada-indent-record-rel-type
--EMACSRESULT: 0

-- Testing with uppercase Ada keywords; parser must be case-insensitive!
PACKAGE Ada_Mode.Options IS
   PRAGMA Elaborate_Body (Options);

   --EMACSCMD:(progn (forward-line 1)(downcase-word 1)(wisi-case-adjust)(let ((case-fold-search nil))(looking-back "TYPE")))
   TYPE Private_Type_1 IS TAGGED
   RECORD
      --EMACSRESULT:t
      Component_1 : Integer;
   END RECORD;

-- comment in column 0

   TYPE Derived_Type_1 IS
   NEW Private_Type_1 WITH RECORD
      Component_2 : Integer;
   END RECORD;

END Ada_Mode.Options;
-- Local Variables:
-- ada-indent-record-rel-type: 0
-- ada-indent-comment-col-0: t
-- ada-case-keyword: upper-case
-- End:
