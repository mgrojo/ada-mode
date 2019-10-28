--EMACSCMD:(wisi-prj-select-cache (cl-ecase ada-xref-tool (gpr_query "subdir/ada_mode.adp") (gnat "subdir/ada_mode-gnatxref.prj")) (ada-prj-default))
--EMACSCMD:(progn (wisi-parse-buffer 'face)(font-lock-ensure))

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
   --EMACSCMD:(progn (end-of-line 3)(backward-word 1)(wisi-show-declaration-parents)(looking-at "Parent_Type_1"))
   --EMACSRESULT:t
   overriding procedure Procedure_1a (Item  : in out Child_Type_1);
   --EMACSCMD:(when (eq ada-xref-tool 'gpr_query) (forward-line -1)(forward-word 3)(wisi-show-overridden) (back-to-indentation) (looking-at "not overriding procedure Procedure_1a")))
   --EMACSRESULT:(eq ada-xref-tool 'gpr_query)
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

   --EMACSCMD:(progn (forward-line 2)(forward-word 1)(ada-find-other-file)(looking-at "overriding function Function_2a"))
   --EMACSRESULT:t
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

   overriding
   function Function_2i (Param : in Child_Type_1) return Child_Type_1 is
     (Parent_Element_1 => Param.Parent_Element_1 * 2,
      Parent_Element_2 => 3.0,
      Parent_Element_3 => False,
      Child_Element_1  => 1,
      Child_Element_2  => 1.0,
      Child_Element_3  => True); -- expression function returning aggregate

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
-- WORKAROUND: There is some race condition or uninit var that causes
-- indent to fail weirdly if wisi-disable-face is nil. Doesn't happen
-- in any other test, only happens when running full test (can't
-- reproduce otherwise).
--
--  Local Variables:
--  ada-indent-comment-gnat: t
--  wisi-disable-face: t
--  End:
