-- minimal body to keep the compiler happy

package body Ada_Mode.Generic_Parent is

   procedure Generic_Procedure (Param_1 : in Param_Type)
   is
      pragma Unreferenced (Param_1);
   begin
      null;
   end Generic_Procedure;

   function Generic_Function (Param_1 : in Param_Type) return Result_Type
   is begin
      return Default;
   end Generic_Function;

   function Gen_Function_2a (Item : in Tagged_Type) return Float
   is
      pragma Unreferenced (Item);
   begin
      return 0.0;
   end Gen_Function_2a;

   procedure Gen_Procedure_1b (Item : in out Tagged_Type)
   is
      pragma Unreferenced (Item);
   begin
      null;
   end Gen_Procedure_1b;

end Ada_Mode.Generic_Parent;
