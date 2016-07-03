--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(jit-lock-fontify-now)

package Ada_Mode.Nominal.Child is

   --EMACSCMD:(test-face "is" font-lock-keyword-face)
   --EMACSCMD:(test-face "new" font-lock-keyword-face)
   --EMACSCMD:(test-face "Parent_Type_1" font-lock-type-face)
   --EMACSCMD:(test-face "with" font-lock-keyword-face)
   type Child_Type_1 is new Parent_Type_1 with
   -- comment between 'with' and 'record'
      record
         Child_Element_1 : Integer;
         Child_Element_2 : Float;
         Child_Element_3 : Boolean;
      end record;

   -- goto parent type declaration for Child_Type_1
   --EMACSCMD:(unless (eq ada-xref-tool 'gnat) (end-of-line 3)(backward-word 1)(ada-show-declaration-parents)(looking-at "Parent_Type_1"))
   --EMACSRESULT:(not (eq ada-xref-tool 'gnat))
   overriding procedure Procedure_1a (Item  : in out Child_Type_1);
   --EMACSCMD:(unless (eq ada-xref-tool 'gnat) (forward-line -1)(forward-word 3)(ada-show-overridden t) (back-to-indentation) (looking-at "not overriding procedure Procedure_1a"))
   --EMACSRESULT:(not (eq ada-xref-tool 'gnat))
   -- FIXME: test multiple parents

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

   --EMACSCMD:(unless (eq ada-xref-tool 'gnat) (forward-line 2)(forward-word 1)(ada-find-other-file nil)(looking-at "overriding function Function_2a"))
   --EMACSRESULT:(not (eq ada-xref-tool 'gnat))
   overriding function Function_2a (Param : in Child_Type_1) return Float;
   -- FIXME: test does not distinguish spec from body!

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

   function Child_Add (Left, Right : in Child_Type_1) return Child_Type_1;

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

end Ada_Mode.Nominal.Child;
--  Local Variables:
--  ada-indent-comment-gnat: t
--  End:
