--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
package Ada_Mode is
   Global_Exception_1 : exception;

   protected Separate_Protected_Body
   with
     Priority => 5
   is
      entry E;
      procedure P;
      function F return Boolean;
   end;

   --EMACSCMD:(progn (forward-line 2)(forward-word 2)(call-interactively 'wisi-goto-spec/body)(looking-at (cl-ecase ada-xref-tool (gpr_query "Separate_Procedure is separate")(gnat "Separate_Procedure is$"))))
   --EMACSRESULT:t
   procedure Separate_Procedure;
   -- WORKAROUND: GNAT GPL 2016/2017 puts a reference to the full body
   -- declaration in subdir/ada_mode-separate_procedure.adb in
   -- ada_mode.ali; gnatxref sees it, but gpr_query doesn't.

   --  test optional name in 'end'
end;
