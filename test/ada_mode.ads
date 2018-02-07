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

   --EMACSCMD:(progn (forward-line 2)(forward-word 2)(ada-goto-declaration)(looking-at "procedure Separate_Procedure;"))
   --EMACSRESULT:t
   procedure Separate_Procedure;
   -- WORKAROUND: GNAT GPL 2016/2017 puts a reference to the full body
   -- declaration in subdir/ada_mode-separate_procedure.adb in
   -- ada_mode.ali, but gpr_query doesn't see it.

   --  test optional name in 'end'
end;
