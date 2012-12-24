--EMACSCMD:(ada-parse-prj-file "subdir/ada_mode.adp")
--EMACSCMD:(ada-select-prj-file "subdir/ada_mode.adp")
package Ada_Mode.Nominal.Child is

   --EMACSCMD:(condition-case nil (delete-file "ada_mode-nominal-child.adb") (error nil))
   --EMACSCMD:(ada-make-package-body)
   --EMACSCMD:(progn (ada-find-other-file nil)(looking-at "package body Ada_Mode.Nominal.Child"))
   --EMACSCMD:(condition-case nil (delete-file "ada_mode-nominal-child.adb") (error nil))

   type Child_Type_1 is new Parent_Type_1 with
      record
         Child_Element_1 : Integer;
         Child_Element_2 : Float;
         Child_Element_3 : Boolean;
      end record;

   -- goto parent type declaration
   --EMACSCMD:(progn (end-of-line 3)(backward-word 2)(ada-goto-declaration nil t)(looking-at "Parent_Type_1"))
   --EMACSRESULT:t
   overriding procedure Procedure_1a (Item  : in out Child_Type_1);

   --EMACSCMD: (progn (forward-line 2)(ada-which-function))
   overriding procedure Procedure_1b
     (Item  : in out Child_Type_1) is null;
   --EMACSRESULT:"Procedure_1b"

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

   overriding function Function_2a (Param : in Child_Type_1) return Float;

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
