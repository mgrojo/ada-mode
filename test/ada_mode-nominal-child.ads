package Ada_Mode.Nominal.Child is

   type Child_Type is new Parent_Type_1 with null record;

   overriding procedure Procedure_1a (Item  : in out Child_Type);

   overriding procedure Procedure_1b
      (Item  : in out Child_Type) is null;

   overriding
   procedure Procedure_1c (Item  : in out Child_Type) is null;

   overriding 
   procedure Procedure_1d
      (Item  : in out Child_Type)
      is null;

   type Child_Type is new Parent_Type_1 with null record;

   overriding procedure Procedure_1a (Item  : in out Child_Type);

   overriding procedure Procedure_1b
      (Item  : in out Child_Type) is null;

   overriding
   procedure Procedure_1c (Item  : in out Child_Type) is null;

   overriding 
   procedure Procedure_1d
      (Item  : in out Child_Type)
      is null;

   overriding function Function_2a (Param : in Child_Type) return Float;

   overriding
   function Function_2b (Param : in Child_Type) return
      Float;
      
   overriding function Function_2c (Param : in Child_Type)
      return Float;

   overriding    
   function Function_2d
      (Param : in Child_Type) return Float;

   overriding function
      Function_2e (Param : in Child_Type) return Float;

   overriding function Function_2f
      (Param : in Parent_Type_1)
      return Float;

end Ada_Mode.Nominal.Child;

