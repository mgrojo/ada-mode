--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-backend ((gpr_query eglot) "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
--EMACSCMD:(progn (wisi-parse-buffer 'face)(font-lock-ensure))

package Ada_Mode.Nominal.Child is

   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(test-face "new" font-lock-keyword-face)
   --EMACSCMD:(test-face "Parent_Type_1" nil)
   --EMACSCMD:(test-face "with" font-lock-keyword-face)
   type Child_Type_1 is
     -- comment after 'is', not before 'record'
     new Parent_Type_1 with
      -- comment between 'with' and 'record'
      record
         Child_Element_1 : Integer;
         Child_Element_2 : Float;
         Child_Element_3 : Boolean;
      end record;
   --EMACSCMD:(progn (forward-line -9)(test-all-defs "Child_Type_1"))
   --EMACSRESULT:(list (list "ada_mode-nominal-child.ads" (concat "Child_Type_1 " (cl-ecase ada-xref-backend ((gpr_query eglot) "record type")(gnat "spec")))))

   --EMACSCMD:(progn (end-of-line 3)(backward-word 1)(wisi-show-declaration-parents)(looking-at "Parent_Type_1"))
   --EMACSRESULT:t
   overriding procedure Procedure_1a (Item  : in out Child_Type_1);
   --EMACSCMD:(cl-ecase ada-xref-backend ((gpr_query eglot) (forward-line -1)(forward-word 3)(wisi-show-overridden) (back-to-indentation) (looking-at "not overriding procedure Procedure_1a")))
   --EMACSRESULT:(eq ada-xref-tool 'gpr_query)

   --EMACSCMD: (progn (forward-line 2)(ada-which-function))
   overriding procedure Procedure_1b
     (Item  : in out Child_Type_1) is null;
   --EMACSRESULT:"Procedure_1b"
   --EMACSCMD:(setq ff-function-name nil)

   procedure Procedure_1c_Different_Name (Item  : in out Child_Type_1) is null;

   overriding
   procedure Procedure_1c (Item  : in out Child_Type_1)
     renames Procedure_1c_Different_Name;  -- from ada-indent-renames

   overriding
   procedure Procedure_1d
     (Item   : in out Child_Type_1;
      Item_1 : in     Character;
      Item_2 : out    Character)
     is null;

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file)(and (looking-at "overriding function Function_2a") (string= (file-name-nondirectory (buffer-file-name)) "ada_mode-nominal-child.adb")))
   --EMACSRESULT:t
   overriding function Function_2a (Param : in Child_Type_1) return Float;

   --EMACSCMD:(test-all-refs "function Function_2b")
   --EMACSRESULT_START:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2b Parent_Type_1; dispatching call"))(gnat '("ada_mode-nominal-child.ads" "Function_2b spec")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; body"))(gnat '("ada_mode-nominal-child.adb" "Function_2b body")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; static call"))(gnat '("ada_mode-nominal-child.adb" "Function_2b")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Parent_Type_1; dispatching call"))(gnat '("ada_mode-nominal-child.adb" "Function_2b")))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.ads" "Function_2b Parent_Type_1; declaration"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; body"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; label on end line"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2b Child_Type_1; static call"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.ads" "Function_2b Child_Type_1; declaration"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2b Child_Type_2; dispatching call"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Child_Type_2; body"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Child_Type_2; static call"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.adb" "Function_2b Child_Type_2; dispatching call"))
   --EMACSRESULT_ADD:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.ads" "Function_2b Child_Type_2; declaration"))
   --EMACSRESULT_FINISH:
   overriding
   function Function_2b (Param : in Child_Type_1) return
     Float;
   overriding function Function_2c (Param : in Child_Type_1)
                                   return Float;

   function Function_2d_Different_Name
     (Param : in Child_Type_1) return Float;

   overriding
   function Function_2d
     (Param : in Child_Type_1)
     return Float   -- from ada-indent-return
     renames Function_2d_Different_Name;  -- from ada-indent-renames

   overriding function
     Function_2e (Param : in Child_Type_1) return Float;

   function Function_2f_Different_Name
     (Param : in Child_Type_1)
     return Float;

   overriding function Function_2f
     (Param : in Child_Type_1)
     return Float   -- from ada-indent-return
     renames Function_2f_Different_Name;  -- from ada-indent-renames

   overriding
   function Function_2h (Param : in Child_Type_1) return Float is (1.0); -- overriding with expression function

   overriding
   function Function_2i (Param : in Child_Type_1) return Child_Type_1 is
     (Parent_Element_1 => Param.Parent_Element_1 * 2,
      Parent_Element_2 => 3.0,
      Parent_Element_3 => False,
      Child_Element_1  => 1,
      Child_Element_2  => 1.0,
      Child_Element_3  => True); -- expression function returning aggregate

   --  New primitive operation on child type
   --EMACSCMD:(test-all-defs "function Child_Add")
   --EMACSRESULT_START:(list "ada_mode-nominal-child.ads" (concat "Child_Add " (cl-ecase ada-xref-backend ((gpr_query eglot) "Ada_Mode.Nominal.Child.Child_Type_1;(Left, Right) function")(gnat "spec"))))
   --EMACSRESULT_ADD:  (list "ada_mode-nominal-child.adb" (concat "Child_Add " (cl-ecase ada-xref-backend ((gpr_query eglot) "Ada_Mode.Nominal.Child.Child_Type_1;(Left, Right) body")(gnat "body"))))
   --EMACSRESULT_FINISH:
   function Child_Add (Left, Right : in Child_Type_1) return Child_Type_1;

   --  New primitive operation on child type, hominym of parent primitive op
   --EMACSCMD:(test-all-defs "procedure Function_2h")
   --EMACSRESULT_START:(cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.ads" "Function_2h Ada_Mode.Nominal.Parent_Type_1;(Param) function"))(gnat '("ada_mode-nominal-child.ads" "Function_2h spec")))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.ads" "Function_2h Ada_Mode.Nominal.Child.Child_Type_1;(Param) function"))(gnat '("ada_mode-nominal-child.adb" "Function_2h body")))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.ads" "Function_2h Ada_Mode.Nominal.Child.Child_Type_1;(Param_1, Param_2) procedure"))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal-child.adb" "Function_2h Ada_Mode.Nominal.Child.Child_Type_1;(Param_1, Param_2) body"))
   --EMACSRESULT_ADD:  (cl-ecase ada-xref-backend ((gpr_query eglot) '("ada_mode-nominal.ads" "Function_2h Ada_Mode.Nominal.Child.Child_Type_2;(Param) function"))
   --EMACSRESULT_FINISH:
   procedure Function_2h (Param_1 : in Child_Type_1; Param_2 : in Integer);

   Child_Obj_1 : constant Child_Type_1 :=
     (Default_Parent with 10, 12.0, True);

   Child_Obj_2 : constant Child_Type_1 :=
     (Default_Parent with
      10, 12.0, True);

   Child_Obj_3 : constant Child_Type_1 :=
     (Default_Parent with
      Child_Element_1 => 10,
      Child_Element_2 => 12.0,
      Child_Element_3 => True);

   Child_Obj_4 : constant Child_Type_1 :=
     (Parent_Type_1'(1, 2.0, False) with
      Child_Element_1 => 10,
      Child_Element_2 => 12.0,
      Child_Element_3 => True);

   Child_Obj_5 : constant Child_Type_1 :=
     (Parent_Type_1'
        (Parent_Element_1 => 1,
         Parent_Element_2 => 2.0,
         Parent_Element_3 => False)
      with
      Child_Element_1 => 10,
      Child_Element_2 => 12.0,
      Child_Element_3 => True);

   Child_Obj_6 : constant Child_Type_1 :=
     (Default_Parent
      with
      Child_Element_1 => 10,
      Child_Element_2 => 12.0,
      Child_Element_3 => True);

   type Child_Type_2 is new Parent_Type_1 with private;
   --  Child_Type_2 is used to test xref on a type tree

   overriding function Function_2i (Param : in Child_Type_2) return Child_Type_2;

private

   type Child_Type_2 is new Parent_Type_1 with record
      --  Only present to test xref on a type tree
      Child_2 : Integer;
   end record;

end Ada_Mode.Nominal.Child;
