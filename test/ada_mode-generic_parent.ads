-- This is to test the indentation of declarations in generics package declarations
--
-- named "parent" for historical reasons

-- no code before "private"
private
generic
package Ada_Mode.Generic_Parent is --  target 0

   generic
      type Param_Type is (<>);
      with function Function_1 (Param : in Param_Type) return Boolean;
   procedure Generic_Procedure (Param_1 : in Param_Type);

   --EMACSCMD:(progn (ada-goto-declarative-region-start)(looking-at " --  target 0"))
   --EMACSRESULT:t

   generic procedure Generic_Procedure_Rename renames Generic_Procedure;

   generic
      type Param_Type is range <>;
      type Result_Type is (<>);
      Default   : in Result_Type;
      Threshold : in Param_Type;
   function Generic_Function (Param_1 : in Param_Type)
                             return Result_Type;

   generic function Generic_Function_Rename renames Generic_Function;

   -- These match some of the primitive operations of
   -- Ada_Mode.Nominal.Parent_Type_1.

   generic
      type Tagged_Type is abstract tagged limited private;
   function Gen_Function_2a (Item : in Tagged_Type) return Float;

   generic
      type Tagged_Type is abstract tagged limited private;
   procedure Gen_Procedure_1b (Item : in out Tagged_Type);

end Ada_Mode.Generic_Parent;
