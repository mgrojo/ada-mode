--  Abstract :
--
--  Parse a file with the libadalang parser, output the corrected token stream.
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with GNAT.Traceback.Symbolic;
with Langkit_Support.Diagnostics;
with Libadalang.Analysis.More;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;
procedure Dump_Libadalang_Corrected
is
   procedure Put_Usage
   is begin
      Put_Line ("dump_libadalang_corrected <file> [verbosity]");
   end Put_Usage;

   Outline   : constant := 0;
   --  Detail    : constant := 1;
   --  Extra     : constant := 2;
   Verbosity : Natural := Outline;

   Ctx : constant Analysis_Context := Create_Context (Charset => "UTF-8", With_Trivia => False);
   Unit : aliased Analysis_Unit;

   procedure Put_Tokens (Node : in Ada_Node'Class)
   is
      use Libadalang.Common;

      function Not_Empty (Node : in Ada_Node_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Ada_Node_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Elsif_Stmt_Part_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Elsif_Stmt_Part_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Pragma_Node_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Pragma_Node_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Base_Assoc_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Base_Assoc_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Name_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Name_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Discriminant_Part) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Discriminant_Part_Has_Element (Node, 1);
      end Not_Empty;

   begin
      if Node = No_Ada_Node then
         return;
      end if;

      case Kind (Node) is
      when Ada_Abort_Absent | Ada_Abstract_Absent | Ada_Aliased_Absent | Ada_All_Absent | Ada_Constant_Absent |
        Ada_Limited_Absent | Ada_Not_Null_Absent | Ada_Private_Absent | Ada_Protected_Absent | Ada_Reverse_Absent |
        Ada_Synchronized_Absent | Ada_Tagged_Absent | Ada_Until_Absent | Ada_With_Private_Absent |
        Ada_Overriding_Unspecified =>
         null;
      when Ada_Abort_Present =>
         Put_Line ("abort");
      when Ada_Abstract_Present =>
         Put_Line ("abstract");
      when Ada_Ada_Node_List =>
         if Not_Empty (As_Ada_Node_List (Node)) then
            for N of As_Ada_Node_List (Node) loop
               Put_Tokens (N);
            end loop;
         end if;
         --  Ada_Alternatives_List,
         --  Ada_Constraint_List, Ada_Decl_List,
      when Ada_Stmt_List =>
         if Not_Empty (Ada_Node_List (As_Stmt_List (Node))) then
            for N of As_Stmt_List (Node) loop
               Put_Tokens (N);
            end loop;
         end if;
         --  Ada_Aspect_Assoc_List,
         --  Ada_Base_Assoc_List, Ada_Assoc_List, Ada_Case_Expr_Alternative_List,
         --  Ada_Case_Stmt_Alternative_List, Ada_Compilation_Unit_List,
         --  Ada_Contract_Case_Assoc_List, Ada_Defining_Name_List,
         --  Ada_Discriminant_Spec_List, Ada_Elsif_Expr_Part_List,
         --  Ada_Elsif_Stmt_Part_List, Ada_Enum_Literal_Decl_List,
         --  Ada_Expr_Alternatives_List, Ada_Discriminant_Choice_List, Ada_Name_List,
         --  Ada_Parent_List, Ada_Param_Spec_List, Ada_Pragma_Node_List,
         --  Ada_Select_When_Part_List, Ada_Unconstrained_Array_Index_List,
         --  Ada_Variant_List
      when Ada_Aliased_Present =>
         Put_Line ("aliased");
      when Ada_All_Present =>
         Put_Line ("all");
      when Ada_Constrained_Array_Indices =>
         declare
            M : constant Constraint_List := As_Constrained_Array_Indices (Node).F_List;
         begin
            if Not_Empty (Ada_Node_List (M)) then
               for N of M loop
                  Put_Tokens (N);
               end loop;
            end if;
         end;
      when Ada_Unconstrained_Array_Indices =>
         for N of As_Unconstrained_Array_Indices (Node).F_Types loop
            Put_Tokens (N);
         end loop;

         --  when Ada_Aspect_Assoc =>
         --     declare
         --        N : Aspect_Assoc := As_Aspect_Assoc (Node);
         --     begin
         --        null;
         --     end;
         --, Ada_At_Clause,
         --  Ada_Attribute_Def_Clause, Ada_Enum_Rep_Clause, Ada_Record_Rep_Clause,
         --  Ada_Aspect_Spec, Ada_Contract_Case_Assoc,
      when Ada_Pragma_Argument_Assoc =>
         declare
            N : constant Pragma_Argument_Assoc := As_Pragma_Argument_Assoc (Node);
         begin
            if N.F_ID /= No_Identifier then
               Put_Tokens (N.F_Id);
               Put_Line ("=>");
            end if;
            Put_Tokens (N.F_Expr);
         end;

         --  Ada_Entry_Spec, Ada_Enum_Subp_Spec,
      when Ada_Subp_Spec =>
         declare
            N : constant Subp_Spec := As_Subp_Spec (Node);
         begin
            Put_Tokens (N.F_Subp_Kind);
            Put_Tokens (N.F_Subp_Name);
            if N.F_Subp_Params /= No_Ada_Node then
               Put_Line ("(");
               Put_Tokens (N.F_Subp_Params);
               Put_Line (")");
            end if;
            if N.P_Returns /= No_Ada_Node then
               Put_Line ("return");
               Put_Tokens (N.P_Returns);
            end if;
         end;

         --  Ada_Component_List,
         --  Ada_Known_Discriminant_Part, Ada_Unknown_Discriminant_Part,
         --  Ada_Entry_Completion_Formal_Params, Ada_Generic_Formal_Part,
         --  Ada_Null_Record_Def, Ada_Record_Def, Ada_Aggregate_Assoc,
         --  Ada_Multi_Dim_Array_Assoc, Ada_Discriminant_Assoc, Ada_Param_Assoc,
         --  Ada_Component_Decl, Ada_Discriminant_Spec, Ada_Generic_Formal_Obj_Decl,
         --  Ada_Generic_Formal_Package, Ada_Generic_Formal_Subp_Decl,
         --  Ada_Generic_Formal_Type_Decl, Ada_Param_Spec,
         --  Ada_Generic_Package_Internal, Ada_Package_Decl,
         --  Ada_Discrete_Base_Subtype_Decl, Ada_Subtype_Decl,
         --  Ada_Classwide_Type_Decl, Ada_Incomplete_Type_Decl,
         --  Ada_Incomplete_Tagged_Type_Decl, Ada_Protected_Type_Decl,
      when Ada_Task_Type_Decl =>
         declare
            N : constant Task_Type_Decl := As_Task_Type_Decl (Node);
         begin
            Put_Line ("task");
            Put_Line ("type");
            Put_Tokens (N.F_Name);
            if Not_Empty (N.F_Discriminants) then
               Put_Line ("(");
               Put_Tokens (Discriminant_Part);
               Put_Line (")");
            end if;
            Put_Line ("is");
            Put_Tokens (N.F_Definition);
            --  F_End_Name is in F_Definition

         --  Ada_Single_Task_Type_Decl, Ada_Type_Decl,
         --  Ada_Anonymous_Type_Decl, Ada_Synth_Anonymous_Type_Decl,
         --  Ada_Abstract_Subp_Decl, Ada_Abstract_Formal_Subp_Decl,
         --  Ada_Concrete_Formal_Subp_Decl, Ada_Subp_Decl, Ada_Entry_Decl,
         --  Ada_Enum_Literal_Decl, Ada_Generic_Subp_Internal, Ada_Expr_Function,
         --  Ada_Null_Subp_Decl,
      when Ada_Subp_Body =>
         declare
            N : constant Subp_Body := As_Subp_Body (Node);
         begin
            Put_Tokens (N.F_Overriding);
            Put_Tokens (N.F_Subp_Spec);
            Put_Line ("is");
            Put_Tokens (N.F_Decls);
            Put_Line ("begin");
            Put_Tokens (N.F_Stmts);
            Put_Line ("end");
            Put_Tokens (N.F_End_Name);
            Put_Line (";");
         end;
         --, Ada_Subp_Renaming_Decl,
         --  Ada_Package_Body_Stub, Ada_Protected_Body_Stub, Ada_Subp_Body_Stub,
         --  Ada_Task_Body_Stub, Ada_Entry_Body,
      when Ada_Package_Body =>
         declare
            N : constant Package_Body := As_Package_Body (Node);
         begin
            Put_Line ("package");
            Put_Line ("body");
            Put_Tokens (F_Package_Name (N));
            Put_Line ("is");
            Put_Tokens (F_Decls (N));
            if F_Stmts (N) /= No_Handled_Stmts then
               Put_Tokens (F_Stmts (N));
            end if;
            Put_Line ("end");
            Put_Tokens (F_End_Name (N));
            Put_Line (";");
         end;

         --  Ada_Protected_Body,
         --  Ada_Task_Body, Ada_Entry_Index_Spec, Ada_Error_Decl, Ada_Exception_Decl,
         --  Ada_Exception_Handler, Ada_For_Loop_Var_Decl, Ada_Generic_Package_Decl,
         --  Ada_Generic_Subp_Decl, Ada_Generic_Package_Instantiation,
         --  Ada_Generic_Subp_Instantiation, Ada_Generic_Package_Renaming_Decl,
         --  Ada_Generic_Subp_Renaming_Decl, Ada_Label_Decl, Ada_Named_Stmt_Decl,
         --  Ada_Number_Decl,
      when Ada_Object_Decl =>
         declare
            M : constant Object_Decl := As_Object_Decl (Node);
            IDs : constant Defining_Name_List := F_IDs (M);
            I : Integer;
         begin
            I := Defining_Name_List_First;
            loop
               Put_Tokens (Defining_Name_List_Element (Args, I);
               I := Defining_Name_List_Next (Args, I);
               if Defining_Name_List_Has_Element (Args, I) then
                  Put_Line (",");
               else
                  exit;
               end if;
            end loop;
            Put_Line (":");
            if F_Has_Aliased (M) then
               Put_Line ("aliased");
            end if;
            if F_Has_Constant (M) then
               Put_Line ("constant");
            end if;
            Put_Tokens (F_Mode (M));
            Put_Tokens (F_Type_Expr (M));
            if F_Default_Expr (M) /= No_Expr then
               Put_Tokens (F_Default_Expr (M));
            end if;
            if F_Renaming_Clause (M) /= No_Renaming_Clause then
               Put_Tokens (F_Renaming_Clause (M));
            end if;

         --  Ada_Extended_Return_Stmt_Object_Decl,
         --  Ada_Package_Renaming_Decl, Ada_Single_Protected_Decl,
         --  Ada_Single_Task_Decl, Ada_Case_Stmt_Alternative,
      when Ada_Compilation_Unit =>
         declare
            M : constant Compilation_Unit := As_Compilation_Unit (Node);
         begin
            if Not_Empty (M.F_Prelude) then
               for N of M.F_Prelude loop
                  Put_Tokens (N);
               end loop;
            end if;
            Put_Tokens (M.F_Body);
            if Not_Empty (M.F_Pragmas) then
               for N of M.F_Pragmas loop
                  Put_Tokens (N);
               end loop;
            end if;
         end;
         --  Ada_Component_Clause, Ada_Component_Def,
         --  Ada_Constant_Present, Ada_Delta_Constraint, Ada_Digits_Constraint,
         --  Ada_Discriminant_Constraint, Ada_Index_Constraint, Ada_Range_Constraint,
      when Ada_Declarative_Part =>
         declare
            M : constant Ada_Node_List := As_Declarative_Part (Node).F_Decls;
         begin
            if Not_Empty (M) then
               for N of M loop
                  Put_Tokens (N);
               end loop;
            end if;
         end;
         --  Ada_Private_Part, Ada_Public_Part,
         --  Ada_Elsif_Expr_Part, Ada_Elsif_Stmt_Part, Ada_Allocator, Ada_Aggregate,
         --  Ada_Null_Record_Aggregate,
      when Ada_Bin_Op | Ada_Relation_Op =>
         declare
            N : constant Bin_Op := As_Bin_Op (Node);
         begin
            Put_Tokens (N.F_Left);
            Put_Tokens (N.F_Op);
            Put_Tokens (N.F_Right);
         end;
         --  Ada_Box_Expr,
         --  Ada_Case_Expr, Ada_Case_Expr_Alternative, Ada_Contract_Cases,
         --  Ada_If_Expr, Ada_Membership_Expr, Ada_Attribute_Ref,
         --  Ada_Update_Attribute_Ref,
      when Ada_Call_Expr =>
         declare
            N : Constant Call_Expr := As_Call_Expr (Node);
         begin
            Put_Tokens (F_Name (N));
            Put_Tokens (F_Suffix (N));
         end;

      when Ada_Defining_Name =>
         Put_Tokens (As_Defining_Name (Node).F_Name);
         --  Ada_Discrete_Subtype_Name,
      when Ada_Dotted_Name =>
         Put_Tokens (As_Dotted_Name (Node).F_Prefix);
         Put_Line (".");
         Put_Tokens (As_Dotted_Name (Node).F_Suffix);
      when Ada_End_Name =>
         Put_Tokens (As_End_Name (Node).F_Name);
         --  Ada_Explicit_Deref, Ada_Qual_Expr, Ada_Char_Literal,
      when Ada_Identifier =>
         Put ("IDENTIFIER "); Ada.Wide_Wide_Text_IO.Put_Line (Text (Token_Start (Node)));
         --  Ada_Op_Abs, Ada_Op_And,
      when Ada_Op_And_Then =>
         Put_Line ("and");
         Put_Line ("then");
         --  Ada_Op_Concat, Ada_Op_Div,
         --  Ada_Op_Double_Dot,
      when Ada_Op_Eq =>
         Put_Line ("=");
         --  Ada_Op_Gt, Ada_Op_Gte, Ada_Op_In,
         --  Ada_Op_Lt, Ada_Op_Lte, Ada_Op_Minus, Ada_Op_Mod, Ada_Op_Mult, Ada_Op_Neq,
         --  Ada_Op_Not, Ada_Op_Not_In, Ada_Op_Or, Ada_Op_Or_Else, Ada_Op_Plus,
         --  Ada_Op_Pow, Ada_Op_Rem, Ada_Op_Xor, Ada_String_Literal,
      when Ada_Null_Literal =>
         Put_Line ("null");
      when Ada_Int_Literal =>
         Put_Line ("NUMERIC_LITERAL");
         --  Ada_Real_Literal, Ada_Target_Name, Ada_Paren_Expr,
         --  Ada_Quantified_Expr, Ada_Raise_Expr, Ada_Un_Op,
      when Ada_Handled_Stmts =>
         declare
            M : constant Handled_Stmts := As_Handled_Stmts (Node);
         begin
            if Not_Empty (Ada_Node_List (M.F_Stmts)) then
               for N of M.F_Stmts loop
                  Put_Tokens (N);
               end loop;
            end if;
            if Not_Empty (M.F_Exceptions) then
               for N of M.F_Exceptions loop
                  Put_Tokens (N);
               end loop;
            end if;
         end;
         --  Ada_Interface_Kind_Limited, Ada_Interface_Kind_Protected,
         --  Ada_Interface_Kind_Synchronized, Ada_Interface_Kind_Task,
         --  Ada_Iter_Type_In, Ada_Iter_Type_Of,
      when Ada_Library_Item =>
         Put_Tokens (As_Library_Item (Node).F_Item);

         --  Ada_Limited_Present, Ada_For_Loop_Spec, Ada_While_Loop_Spec,
         --  Ada_Mode_Default, Ada_Mode_In, Ada_Mode_In_Out, Ada_Mode_Out,
         --  , Ada_Not_Null_Present, Ada_Null_Component_Decl,
         --  Ada_Others_Designator, Ada_Overriding_Not_Overriding,
         --  Ada_Overriding_Overriding, ,
      when Ada_Params =>
         Put_Tokens (F_Params (As_Params (Node)));

      when Ada_Pragma_Node =>
         declare
            N : constant Pragma_Node := As_Pragma_Node (Node);
            Args : constant Base_Assoc_List := N.F_Args;
            I : Integer;
         begin
            Put_Line ("pragma");
            Put_Tokens (N.F_Id);
            if Not_Empty (Args) then
               Put_Line ("(");
               I := Base_Assoc_List_First (Args);
               loop
                  Put_Tokens (Base_Assoc_List_Element (Args, I));
                  I := Base_Assoc_List_Next (Args, I);
                  exit when not Base_Assoc_List_Has_Element (Args, I);
                  Put_Line (",");
               end loop;
               Put_Line (")");
            end if;
            Put_Line (";");
         end;

         --  Ada_Prim_Type_Accessor
         --  Ada_Private_Present, Ada_Protected_Def, ,
         --  Ada_Protected_Present, Ada_Quantifier_All, Ada_Quantifier_Some,
         --  Ada_Range_Spec, Ada_Renaming_Clause, ,
         --  Ada_Reverse_Present, Ada_Select_When_Part, Ada_Accept_Stmt,
         --  Ada_Accept_Stmt_With_Stmts, Ada_For_Loop_Stmt,
      when Ada_Loop_Stmt =>
         declare
            N : constant Loop_Stmt := As_Loop_Stmt (Node);
         begin
            Put_Line ("loop");
            Put_Tokens (N.F_Spec);
            Put_Tokens (N.F_Stmts);
            Put_Line ("end");
            Put_Line ("loop");
            Put_Tokens (N.F_End_Name);
            Put_Line (";");
         end;
         --  Ada_While_Loop_Stmt,
      when Ada_Begin_Block =>
         Put_Line ("begin");
         Put_Tokens (As_Begin_Block (Node).F_Stmts);
         Put_Line ("end");
         Put_Tokens (As_Begin_Block (Node).F_End_Name);
         Put_Line (";");
         --  Ada_Decl_Block, Ada_Case_Stmt,
         --  Ada_Extended_Return_Stmt,
      when Ada_If_Stmt =>
         declare
            N : constant If_Stmt := As_If_Stmt (Node);
         begin
            Put_Line ("if");
            Put_Tokens (N.F_Cond_Expr);
            Put_Line ("then");
            if Not_Empty (Ada_Node_List (N.F_Then_Stmts)) then
               for I of N.F_Then_Stmts loop
                  Put_Tokens (I);
               end loop;
            end if;
            if Not_Empty (N.F_Alternatives) then
               for I of N.F_Alternatives loop
                  Put_Tokens (I);
               end loop;
            end if;
            if Not_Empty (Ada_Node_List (N.F_Else_Stmts)) then
               Put_Line ("else");
               for I of N.F_Else_Stmts loop
                  Put_Tokens (I);
               end loop;
            end if;
            Put_Line ("end");
            Put_Line ("if");
            Put_Line (";");
         end;
         --  Ada_Named_Stmt, Ada_Select_Stmt,
      when Ada_Error_Stmt =>
         --  Indicates a syntax error
         null;
         --  Ada_Abort_Stmt,
      when Ada_Assign_Stmt =>
         declare
            N : constant Assign_Stmt := As_Assign_Stmt (Node);
         begin
            Put_Tokens (N.F_Dest);
            Put_Line (":=");
            Put_Tokens (N.F_Expr);
         end;

      when Ada_Call_Stmt =>
         Put_Tokens (As_Call_Stmt (Node).F_Call);
         Put_Line (";");
         --  Ada_Delay_Stmt,
      when Ada_Exit_Stmt =>
         declare
            N : constant Exit_Stmt := As_Exit_Stmt (Node);
         begin
            Put_Line ("exit");
            Put_Tokens (N.F_Loop_Name);
            if N.F_Cond_Expr /= No_Ada_Node then
               Put_Line ("when");
               Put_Tokens (N.F_Cond_Expr);
            end if;
            Put_Line (";");
         end;
         --  Ada_Goto_Stmt, Ada_Label, Ada_Null_Stmt,
         --  Ada_Raise_Stmt, Ada_Requeue_Stmt,
      when Ada_Return_Stmt =>
         Put_Line ("return");
         Put_Tokens (Node.As_Return_Stmt.F_Return_Expr);
         Put_Line (";");

         --  Ada_Terminate_Alternative,
      when Ada_Subp_Kind_Function =>
         Put_Line ("function");
      when Ada_Subp_Kind_Procedure =>
         Put_Line ("procedure");
         --, Ada_Subunit, ,
         --  Ada_Synchronized_Present, , Ada_Tagged_Present,
         --  Ada_Task_Def, Ada_Access_To_Subp_Def, Ada_Anonymous_Type_Access_Def,
         --  Ada_Type_Access_Def, Ada_Array_Type_Def, Ada_Derived_Type_Def,
         --  Ada_Enum_Type_Def, Ada_Formal_Discrete_Type_Def, Ada_Interface_Type_Def,
         --  Ada_Mod_Int_Type_Def, Ada_Private_Type_Def, Ada_Decimal_Fixed_Point_Def,
         --  Ada_Floating_Point_Def, Ada_Ordinary_Fixed_Point_Def,
         --  Ada_Record_Type_Def, Ada_Signed_Int_Type_Def, Ada_Anonymous_Type,
         --  Ada_Enum_Lit_Synth_Type_Expr, Ada_Subtype_Indication,
         --  Ada_Constrained_Subtype_Indication, Ada_Discrete_Subtype_Indication,
         --  Ada_Unconstrained_Array_Index, , Ada_Until_Present,
         --  Ada_Use_Package_Clause, Ada_Use_Type_Clause, Ada_Variant,
         --  Ada_Variant_Part,
      when Ada_With_Clause =>
         declare
            N : constant With_Clause := As_With_Clause (Node);
            Args : constant Name_List := F_Packages (N);
            I : Integer;
         begin
            if F_Has_Limited (N) then
               Put_Tokens (F_Has_Limited (N));
            end if;
            if F_Has_Private (N) then
               Put_Tokens (F_Has_Private (N));
            end if;
            Put_Line ("with");
            if Not_Empty (Args) then
               I := Name_List_First (Args);
               loop
                  Put_Tokens (Name_List_Element (Args, I));
                  I := Name_List_Next (Args, I);
                  exit when not Name_List_Has_Element (Args, I);
                  Put_Line (",");
               end loop;
            end if;
            Put_Line (";");
         end;

         --  Ada_With_Private_Present =>
      when others =>
         Put_Line ("unhandled: " & Kind (Node)'Image);
      end case;
   end Put_Tokens;

begin
   declare
      use Ada.Command_Line;
   begin
      if Argument_Count < 1 then
         Put_Usage;
         Set_Exit_Status (Failure);
         return;
      end if;

      Unit := Get_From_File (Ctx, Argument (1));

      if Argument_Count > 1 then
         Verbosity := Integer'Value (Argument (2));
      end if;
   end;

   if Verbosity > 0 and Has_Diagnostics (Unit) then
      for D of Diagnostics (Unit) loop
         Put_Line (Langkit_Support.Diagnostics.To_Pretty_String (D));
      end loop;
   end if;

   if Root (Unit) = No_Ada_Node then
      Put_Line (Current_Error, "parse failed; no libadalang tree");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Verbosity > Outline then
      Print (Root (Unit));
      New_Line;
   end if;

   Put_Tokens (Root (Unit));

exception
when E : others =>
   Put_Line ("exception " & Ada.Exceptions.Exception_Name (E) & ": " & Ada.Exceptions.Exception_Message (E));
   Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Dump_Libadalang_Corrected;
