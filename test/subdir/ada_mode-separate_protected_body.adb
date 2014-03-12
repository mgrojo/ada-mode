separate (ADA_MODE) protected body SEPARATE_PROTECTED_BODY
is
   -- no comment before "separate", no newline after )
   --
   -- also test ada-case-identifier = upcase-region

   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "protected body Separate_Protected_Body is"))
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
-- ada-case-identifier: upcase-region
-- End:
