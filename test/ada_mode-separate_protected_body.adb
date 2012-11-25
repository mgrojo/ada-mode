separate (Ada_Mode) protected body Separate_Protected_Body
is
   -- no comment before "separate", no newline after )
   --EMACSCMD:(ada-parse-prj-file "ada_mode.adp")
   --EMACSCMD:(ada-select-prj-file "ada_mode.adp")
   --EMACSCMD:(progn (goto-char (point-min))(ada-find-other-file t)(looking-at "protected body Separate_Protected_Body is"))
   entry E when True is
   begin
      null;
   end E;
   procedure P is
   begin
      null;
   end P;
   function F return Boolean is
   begin
      return False;
   end F;
end Separate_Protected_Body;
