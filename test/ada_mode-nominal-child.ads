package Ada_Mode.Nominal.Child is

   type Child_Type_1 is new Parent_Type_1 with
      record
         Child_Element_1 : Integer;
         Child_Element_2 : Float;
         Child_Element_3 : Boolean;
      end record;

   overriding procedure Procedure_1a (Item  : in out Child_Type_1);

   overriding procedure Procedure_1b
     (Item  : in out Child_Type_1) is null;

   overriding
   procedure Procedure_1c (Item  : in out Child_Type_1) is null;

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

   overriding
   function Function_2d
     (Param : in Child_Type_1) return Float;

   overriding function
     Function_2e (Param : in Child_Type_1) return Float;

   overriding function Function_2f
     (Param : in Child_Type_1)
     return Float;

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
