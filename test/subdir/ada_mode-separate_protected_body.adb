separate (ADA_MODE) protected body SEPARATE_PROTECTED_BODY
is
   -- no comment before "separate", no newline after )
   --
   -- also test ada-case-identifier = upcase-region

   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")

   --  WORKAROUND: GNAT GPL 2016 doesn't produce a .ali file for this
   --  file, so gpr_query doesn't work. And gnat find doesn't work either
   --
   --  EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "protected body Separate_Protected_Body is"))
   --  EMACSRESULT:t

   entry E when TRUE is
   begin
      null;
   end E;
   procedure P is
   begin
      null;
   end P;
   function F return BOOLEAN is
   begin
      return FALSE;
   end F;
end SEPARATE_PROTECTED_BODY;
-- Local Variables:
-- ada-case-identifier: upper-case
-- End:
