-- Various generic_instantiations

with Ada_Mode.Generic_Package_Inst;
package Ada_Mode.Generic_Instantiation is

   --  FIXME: test align parameter list
   function Function_1 is new Ada_Mode.Generic_Package_Inst.Generic_Function
     (Param_Type  => Integer;
      Result_Type => Boolean,
      Threshold   => 2);

   procedure Procedure_1 is new Ada_Mode.Generic_Package_Inst.Generic_Procedure (Integer, Function_1);
   procedure Procedure_2 is new Ada_Mode.Generic_Package_Inst.Generic_Procedure (Integer,
                                                                                 Function_1);
   procedure Procedure_3 is new Ada_Mode.Generic_Package_Inst.Generic_Procedure
     (Integer, Function_1);
   procedure Procedure_4 is new Ada_Mode.Generic_Package_Inst.
     Generic_Procedure (Integer, Function_1);
   procedure Procedure_5 is new Ada_Mode.Generic_Package_Inst
     .Generic_Procedure (Integer, Function_1);
   procedure Procedure_6 is new
     Ada_Mode.Generic_Package_Inst.Generic_Procedure (Integer, Function_1);
   procedure Procedure_7 is
     new Ada_Mode.Generic_Package_Inst.Generic_Procedure (Integer, Function_1);
   procedure Procedure_8
     is new Ada_Mode.Generic_Package_Inst.Generic_Procedure (Integer, Function_1);

end Ada_Mode.Generic_Instantiation;
