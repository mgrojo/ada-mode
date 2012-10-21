-- Various generic_instantiations

with Ada_Mode.Generic_Parent;
with Ada_Mode.Nominal;
package Ada_Mode.Generic_Instantiation is

   package Instance is new Ada_Mode.Generic_Parent;

   function Function_1 is new Instance.Generic_Function
     (Param_Type  => Integer,
      Result_Type => Boolean,
      Threshold   => 2);

   procedure Procedure_2 is new Instance.Generic_Procedure (Integer,
                                                            Function_1);
   procedure Procedure_3 is new Instance.Generic_Procedure
     (Integer, Function_1);
   procedure Procedure_4 is new Instance.
     Generic_Procedure (Integer, Function_1);
   procedure Procedure_5 is new Instance
     .Generic_Procedure (Integer, Function_1);
   procedure Procedure_6 is new
     Instance.Generic_Procedure (Integer, Function_1);
   procedure Procedure_7 is
     new Instance.Generic_Procedure (Integer, Function_1);
   procedure Procedure_8
     is new Instance.Generic_Procedure (Integer, Function_1);

   -- FIXME: doesn't compile
   --  type Child_Type_1 is new Ada_Mode.Nominal.Parent_Type_1 with null record;

   --  overriding
   --  procedure Procedure_1b is new Instance.Gen_Procedure_1b (Child_Type_1); -- freezes Child_Type_1

   --  type Child_Type_2 is new Ada_Mode.Nominal.Parent_Type_1 with null record;

   --  overriding
   --  function Function_2a is new Instance.Gen_Function_2a (Child_Type_2);

end Ada_Mode.Generic_Instantiation;
