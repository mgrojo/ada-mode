separate (ADA_MODE) protected body SEPARATE_PROTECTED_BODY
is
   -- no comment before "separate", no newline after )
   --
   -- also test ada-case-identifier = upcase-region

   --EMACSCMD:(wisi-prj-select-cache "ada_mode.adp" (ada-prj-default))

   --  WORKAROUND: GNAT GPL 2016 doesn't produce a .ali file for this
   --  file, so gpr_query doesn't work. And gnat find doesn't work either
   --
   --  EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file)(looking-at "protected body Separate_Protected_Body is"))
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
-- ada-eglot-test-gpr: nil
-- End:
