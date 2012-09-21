-- This is to test the indentation of declarations in generics package declarations

pragma License (GPL);

with Ada.Text_IO;
with Ada.Strings.Unbounded;
generic
   -- one of each kind of generic_formal_parameter_definition from arm Annex P

   -- Types

   type Object_Formal_Access_Type is not null access all Integer;
   type Procedure_Formal_Access_Type is access protected procedure (A_Param : out Integer);
   type Function_Formal_Access_Type is access protected function (A_Param : in Float) return Standard.Float;
   type Unconstrained_Formal_Array_Type is array (Integer range <>, Standard.Character range <>) of
      Object_Formal_Access_Type;
   type Constrained_Formal_Array_Type is array (Character) of Ada.Text_IO.Count;
   type Formal_Private_Type is abstract tagged limited private;
   type Interface_Type is task interface;
   type Limited_Formal_Derived_Type is abstract limited new Formal_Private_Type with private;
   type Synchronized_Formal_Derived_Type is abstract synchronized new Formal_Private_Type and Interface_Type
      with private;
   type Incomplete_Type (<>) is tagged;
   type Formal_Discrete_Type is (<>);

   -- Numeric types
   type Formal_Decimal_Fixed_Point is delta <> digits <>;
   type Formal_Floating_Point is digits <>;
   type Formal_Modular_Type is mod <>;
   type Formal_Ordinary_Fixed_Point is delta <>;
   type Formal_Signed_Integer_Type is range <>;

   -- Objects
   A, B, C : Integer;
   F : in out Float;
   P : in not null Ada.Strings.Unbounded.String_Access;

   -- Subprograms
   with procedure Concrete_Defaulted_Procedure
      (Item  : in out Ada.Strings.Unbounded.Unbounded_String;
       New_Item : Character)
      is Ada.Strings.Unbounded.Append;

   with procedure Concrete_Defaulted_Procedure_2 is <>;
   with procedure Concrete_Defaulted_Procedure_3 is null;
   with procedure Concrete_Procedure;
   with procedure Abstract_Defaulted_Procedure (Item : access Formal_Private_Type) is abstract <>;
   with procedure Abstract_Procedure (Item : out Formal_Private_Type) is abstract;
   with function Concrete_Function return Float is <>;

   -- Packages
   with package A_Package is new Ada.Text_IO.Integer_IO (Num => Formal_Signed_Integer_Type);

package Ada_Mode.Generic_Package is
end Ada_Mode.Generic_Package;
