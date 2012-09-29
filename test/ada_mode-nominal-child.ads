package Ada_Mode.Nominal.Child is

   type Child_Type_1 is new Parent_Type_1 with null record;

   overriding procedure Procedure_1a (Item  : in out Child_Type_1);

   overriding procedure Procedure_1b
      (Item  : in out Child_Type_1) is null;

   overriding
   procedure Procedure_1c (Item  : in out Child_Type_1) is null;

   overriding
   procedure Procedure_1d
      (Item  : in out Child_Type_1)
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

end Ada_Mode.Nominal.Child;
