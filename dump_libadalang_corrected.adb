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

      function Not_Empty (Node : in Basic_Assoc_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Basic_Assoc_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Identifier_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Identifier_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Elsif_Expr_Part_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Elsif_Expr_Part_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Case_Expr_Alternative_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Case_Expr_Alternative_List_Has_Element (Node, 1);
      end Not_Empty;

      function Not_Empty (Node : in Aspect_Assoc_List) return Boolean
      is begin
         return Libadalang.Analysis.More.Is_Node (Node) and then Aspect_Assoc_List_Has_Element (Node, 1);
      end Not_Empty;

      procedure Put_Ada_Node_List (Node : in Ada_Node_List; Separator : in String := "")
      is begin
         if Not_Empty (Node) then
            declare
               I : Integer := Node.Ada_Node_List_First;
            begin
               loop
                  Put_Tokens (Node.Ada_Node_List_Element (I));
                  I :=  Node.Ada_Node_List_Next (I);
                  exit when not Node.Ada_Node_List_Has_Element (I);
                  if Separator /= "" then
                     Put_Line (Separator);
                  end if;
               end loop;
            end;
         end if;
      end Put_Ada_Node_List;

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
         Put_Ada_Node_List (Node.As_Ada_Node_List);

      when Ada_Alternatives_List =>
         Put_Ada_Node_List (Ada_Node_List (Node.As_Alternatives_List), "|");

         --  Ada_Constraint_List, Ada_Decl_List,
      when Ada_Stmt_List =>
         Put_Ada_Node_List (Ada_Node_List (As_Stmt_List (Node)));

      when Ada_Aspect_Assoc_List =>
         declare
            M : constant Aspect_Assoc_List := Node.As_Aspect_Assoc_List;
            I : Integer := M.Aspect_Assoc_List_First;
         begin
            loop
               Put_Tokens (M.Aspect_Assoc_List_Element (I));
               I := M.Aspect_Assoc_List_Next (I);
               exit when not M.Aspect_Assoc_List_Has_Element (I);
               Put_Line (",");
            end loop;
         end;

      when Ada_Base_Assoc_List =>
         declare
            Assocs : constant Base_Assoc_List := Node.As_Base_Assoc_List;
            I : Integer := Base_Assoc_List_First (Assocs);
         begin
            loop
               Put_Tokens (Base_Assoc_List_Element (Assocs, I));
               I := Base_Assoc_List_Next (Assocs, I);
               exit when not Base_Assoc_List_Has_Element (Assocs, I);
               Put_Line (",");
            end loop;
         end;

      when Ada_Assoc_List =>
         declare
            N : constant Basic_Assoc_List := Node.As_Basic_Assoc_List;
            I : Integer := N.Basic_Assoc_List_First;
         begin
            loop
               Put_Tokens (Basic_Assoc_List_Element (N, I));
               I := Basic_Assoc_List_Next (N, I);
               if Basic_Assoc_List_Has_Element (N, I) then
                  Put_Line (",");
               else
                  exit;
               end if;
            end loop;
         end;

      when Ada_Case_Expr_Alternative_List =>
         declare
            M : constant Case_Expr_Alternative_List := Node.As_Case_Expr_Alternative_List;
            I : Integer := M.Case_Expr_Alternative_List_First;
         begin
            loop
               Put_Tokens (M.Case_Expr_Alternative_List_Element (I));
               I := M.Case_Expr_Alternative_List_Next (I);
               exit when not M.Case_Expr_Alternative_List_Has_Element (I);
               Put_Line (",");
            end loop;
         end;

      when Ada_Case_Stmt_Alternative_List =>
         for N of Node.As_Case_Stmt_Alternative_List loop
            Put_Tokens (N);
         end loop;

      when Ada_Compilation_Unit_List =>
         for N of Node.As_Compilation_Unit_List loop
            Put_Tokens (N);
         end loop;

         --  Ada_Contract_Case_Assoc_List,
      when Ada_Defining_Name_List =>
         declare
            Names : constant Defining_Name_List := Node.As_Defining_Name_List;
            I : Integer := Names.Defining_Name_List_First;
         begin
            loop
               Put_Tokens (Names.Defining_Name_List_Element (I));
               I := Names.Defining_Name_List_Next (I);
               if Names.Defining_Name_List_Has_Element (I) then
                  Put_Line (",");
               else
                  exit;
               end if;
            end loop;
         end;

      when Ada_Discriminant_Spec_List =>
         declare
            M : constant Discriminant_Spec_List := Node.As_Discriminant_Spec_List;
            I : Integer := M.Discriminant_Spec_List_First;
         begin
            loop
               Put_Tokens (M.Discriminant_Spec_List_Element (I));
               I := M.Discriminant_Spec_List_Next (I);
               if M.Discriminant_Spec_List_Has_Element (I) then
                  Put_Line (";");
               else
                  exit;
               end if;
            end loop;
         end;

         --  Ada_Elsif_Expr_Part_List,
         --  Ada_Elsif_Stmt_Part_List, Ada_Enum_Literal_Decl_List,
      when Ada_Expr_Alternatives_List =>
         declare
            M : constant Expr_List := Expr_List (Node.As_Expr_Alternatives_List);
            I : Integer := M.Expr_List_First;
         begin
            loop
               Put_Tokens (M.Expr_List_Element (I));
               I := M.Expr_List_Next (I);
               exit when not M.Expr_List_Has_Element (I);
               Put_Line ("|");
            end loop;
         end;

      when Ada_Discriminant_Choice_List =>
         declare
            N : constant Identifier_List := Node.As_Identifier_List;
            I : Integer := N.Identifier_List_First;
         begin
            loop
               Put_Tokens (N.Identifier_List_Element (I));
               I := N.Identifier_List_Next (I);
               exit when not N.Identifier_List_Has_Element (I);
               Put_Line ("|");
            end loop;
         end;

      when Ada_Name_List | Ada_Parent_List =>
         declare
            Names : constant Name_List := Node.As_Name_List;
            I : Integer := Names.Name_List_First;
         begin
            loop
               Put_Tokens (Names.Name_List_Element (I));
               I := Names.Name_List_Next (I);
               if Names.Name_List_Has_Element (I) then
                  Put_Line (",");
               else
                  exit;
               end if;
            end loop;
         end;

      when Ada_Param_Spec_List =>
         declare
            M : constant Param_Spec_List := As_Param_Spec_List (Node);
            I : Integer := Param_Spec_List_First (M);
         begin
            loop
               Put_Tokens (Param_Spec_List_Element (M, I));
               I := Param_Spec_List_Next (M, I);
               if Param_Spec_List_Has_Element (M, I) then
                  Put_Line (";");
               else
                  exit;
               end if;
            end loop;
         end;

         --  Ada_Pragma_Node_List,
         --  Ada_Select_When_Part_List, Ada_Unconstrained_Array_Index_List,
         --  Ada_Variant_List
      when Ada_Aliased_Present =>
         Put_Line ("aliased");
      when Ada_All_Present =>
         Put_Line ("all");
      when Ada_Constrained_Array_Indices =>
         Put_Ada_Node_List (Ada_Node_List (Node.As_Constrained_Array_Indices.F_List));

      when Ada_Unconstrained_Array_Indices =>
         for N of As_Unconstrained_Array_Indices (Node).F_Types loop
            Put_Tokens (N);
         end loop;

      when Ada_Aspect_Assoc =>
         Put_Tokens (Node.As_Aspect_Assoc.F_Id);
         Put_Line ("=>");
         Put_Tokens (Node.As_Aspect_Assoc.F_Expr);

         --, Ada_At_Clause,
         --  Ada_Attribute_Def_Clause, Ada_Enum_Rep_Clause, Ada_Record_Rep_Clause,
      when Ada_Aspect_Spec =>
         if Not_Empty (Node.As_Aspect_Spec.F_Aspect_Assocs) then
            Put_Line ("with");
            Put_Tokens (Node.As_Aspect_Spec.F_Aspect_Assocs);
         end if;

         --  Ada_Contract_Case_Assoc,
      when Ada_Pragma_Argument_Assoc =>
         declare
            N : constant Pragma_Argument_Assoc := As_Pragma_Argument_Assoc (Node);
         begin
            if N.F_Id /= No_Identifier then
               Put_Tokens (N.F_Id);
               Put_Line ("=>");
            end if;
            Put_Tokens (N.F_Expr);
         end;

      when Ada_Entry_Spec =>
         declare
            N : constant Entry_Spec := Node.As_Entry_Spec;
         begin
            Put_Tokens (N.F_Entry_Name);
            Put_Tokens (N.F_Family_Type);
            Put_Tokens (N.F_Entry_Params);
         end;

         --  Ada_Enum_Subp_Spec,
      when Ada_Subp_Spec =>
         declare
            N : constant Subp_Spec := As_Subp_Spec (Node);
         begin
            Put_Tokens (N.F_Subp_Kind);
            Put_Tokens (N.F_Subp_Name);
            Put_Tokens (N.F_Subp_Params);
            if N.P_Returns /= No_Ada_Node then
               Put_Line ("return");
               Put_Tokens (N.P_Returns);
            end if;
         end;

         --  Ada_Component_List,
      when Ada_Known_Discriminant_Part =>
         Put_Line ("(");
         Put_Tokens (Node.As_Known_Discriminant_Part.F_Discr_Specs);
         Put_Line (")");

         --  Ada_Unknown_Discriminant_Part,
         --  Ada_Entry_Completion_Formal_Params, Ada_Generic_Formal_Part,
         --  Ada_Null_Record_Def, Ada_Record_Def,
      when Ada_Aggregate_Assoc =>
         if Not_Empty (Ada_Node_List (Node.As_Aggregate_Assoc.F_Designators)) then
            Put_Tokens (Node.As_Aggregate_Assoc.F_Designators);
            Put_Line ("=>");
         end if;
         Put_Tokens (Node.As_Aggregate_Assoc.F_R_Expr);

         --  Ada_Multi_Dim_Array_Assoc,
      when Ada_Discriminant_Assoc =>
         if Not_Empty (Identifier_List (Node.As_Discriminant_Assoc.F_Ids)) then
            Put_Tokens (Node.As_Discriminant_Assoc.F_Ids);
            Put_Line ("=>");
         end if;
         Put_Tokens (Node.As_Discriminant_Assoc.F_Discr_Expr);

      when Ada_Param_Assoc =>
         declare
            N : constant Param_Assoc := Node.As_Param_Assoc;
         begin
            if N.F_Designator /= No_Ada_Node then
               Put_Tokens (N.F_Designator);
               Put_Line ("=>");
            end if;
            Put_Tokens (N.F_R_Expr);
         end;

         --  Ada_Component_Decl,
      when Ada_Discriminant_Spec =>
         declare
            M : constant Discriminant_Spec := As_Discriminant_Spec (Node);
         begin
            Put_Tokens (M.F_Ids);
            Put_Line (":");
            Put_Tokens (M.F_Type_Expr);
            if M.F_Default_Expr /= No_Expr then
               Put_Line (":=");
               Put_Tokens (M.F_Default_Expr);
            end if;
         end;

         --  Ada_Generic_Formal_Obj_Decl,
         --  Ada_Generic_Formal_Package, Ada_Generic_Formal_Subp_Decl,
         --  Ada_Generic_Formal_Type_Decl,
      when Ada_Param_Spec =>
         declare
            M : constant Param_Spec := As_Param_Spec (Node);
         begin
            Put_Tokens (M.F_Ids);
            Put_Line (":");
            Put_Tokens (M.F_Has_Aliased);
            Put_Tokens (M.F_Mode);
            Put_Tokens (M.F_Type_Expr);
            if M.F_Default_Expr /= No_Expr then
               Put_Line (":=");
               Put_Tokens (M.F_Default_Expr);
            end if;
         end;

         --  Ada_Generic_Package_Internal,
      when Ada_Package_Decl =>
         declare
            N : constant Package_Decl := Node.As_Package_Decl;
         begin
            Put_Line ("package");
            Put_Tokens (N.F_Package_Name);
            Put_Line ("is");
            Put_Tokens (N.F_Public_Part);
            Put_Tokens (N.F_Private_Part);
            Put_Line ("end");
            Put_Tokens (N.F_End_Name);
            Put_Line (";");
         end;

         --  Ada_Discrete_Base_Subtype_Decl,
      when Ada_Subtype_Decl =>
         Put_Line ("subtype");
         Put_Tokens (Node.As_Subtype_Decl.F_Name);
         Put_Line ("is");
         Put_Tokens (Node.As_Subtype_Decl.F_Subtype);
         Put_Line (";");

         --  Ada_Classwide_Type_Decl, Ada_Incomplete_Type_Decl,
         --  Ada_Incomplete_Tagged_Type_Decl, Ada_Protected_Type_Decl,
      when Ada_Task_Type_Decl =>
         declare
            N : constant Task_Type_Decl := As_Task_Type_Decl (Node);
         begin
            Put_Line ("task");
            Put_Line ("type");
            Put_Tokens (N.F_Name);
            Put_Tokens (N.F_Discriminants);
            Put_Line ("is");
            Put_Tokens (N.F_Definition);
            --  F_End_Name is in F_Definition
         end;

         --  Ada_Single_Task_Type_Decl,
      when Ada_Type_Decl | Ada_Anonymous_Type_Decl =>
         declare
            N : constant Type_Decl := As_Type_Decl (Node);
         begin
            if N.F_Name /= No_Defining_Name then
               Put_Line ("type");
               Put_Tokens (N.F_Name);
               Put_Line ("is");
            end if;
            Put_Tokens (N.F_Discriminants);
            Put_Tokens (N.F_Type_Def);
            if N.F_Name /= No_Defining_Name then
               Put_Line (";");
            end if;
         end;

         --  Ada_Synth_Anonymous_Type_Decl,
         --  Ada_Abstract_Subp_Decl, Ada_Abstract_Formal_Subp_Decl,
         --  Ada_Concrete_Formal_Subp_Decl,
      when Ada_Subp_Decl =>
         declare
            N : constant Subp_Decl := Node.As_Subp_Decl;
         begin
            Put_Tokens (N.F_Overriding);
            Put_Tokens (N.F_Subp_Spec);
            Put_Line (";");
         end;

      when Ada_Entry_Decl =>
         declare
            N : constant Entry_Decl := Node.As_Entry_Decl;
         begin
            Put_Tokens (N.F_Overriding);
            Put_Line ("entry");
            Put_Tokens (N.F_Spec);
            Put_Line (";");
         end;

         --  Ada_Enum_Literal_Decl, Ada_Generic_Subp_Internal, Ada_Expr_Function,
         --  Ada_Null_Subp_Decl,
      when Ada_Subp_Body =>
         declare
            N : constant Subp_Body := As_Subp_Body (Node);
         begin
            Put_Tokens (N.F_Overriding);
            Put_Tokens (N.F_Subp_Spec);
            Put_Tokens (N.F_Aspects);
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
         --  Ada_Task_Body, Ada_Entry_Index_Spec, , Ada_Exception_Decl,
      when Ada_Exception_Handler =>
         declare
            N : constant Exception_Handler := Node.As_Exception_Handler;
         begin
            Put_Line ("when");
            if N.F_Exception_Name /= No_Defining_Name then
               Put_Tokens (N.F_Exception_Name);
               Put_Line (":");
            end if;
            Put_Tokens (N.F_Handled_Exceptions);
            Put_Line ("=>");
            Put_Tokens (N.F_Stmts);
         end;

      when Ada_For_Loop_Var_Decl =>
         declare
            N : constant For_Loop_Var_Decl := Node.As_For_Loop_Var_Decl;
         begin
            Put_Tokens (N.F_Id);
            Put_Tokens (N.F_Id_Type);
         end;

         --  Ada_Generic_Package_Decl,
         --  Ada_Generic_Subp_Decl,
      when Ada_Generic_Package_Instantiation =>
         Put_Line ("package");
         Put_Tokens (Node.As_Generic_Package_Instantiation.F_Name);
         Put_Line ("is");
         Put_Line ("new");
         Put_Tokens (Node.As_Generic_Package_Instantiation.F_Generic_Pkg_Name);
         if Not_Empty (Basic_Assoc_List (Node.As_Generic_Package_Instantiation.F_Params)) then
            Put_Line ("(");
            Put_Tokens (Node.As_Generic_Package_Instantiation.F_Params);
            Put_Line (")");
         end if;
         Put_Line (";");

         --  Ada_Generic_Subp_Instantiation, Ada_Generic_Package_Renaming_Decl,
         --  Ada_Generic_Subp_Renaming_Decl,
      when Ada_Label_Decl =>
         Put_Tokens (Node.As_Label_Decl.F_Name);

      when Ada_Named_Stmt_Decl =>
         Put_Tokens (Node.As_Named_Stmt_Decl.F_Name);
         Put_Line (":");

         --  Ada_Number_Decl,
      when Ada_Object_Decl | Ada_Extended_Return_Stmt_Object_Decl =>
         declare
            M : constant Object_Decl := As_Object_Decl (Node);
         begin
            Put_Tokens (M.F_Ids);
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
               Put_Line (":=");
               Put_Tokens (F_Default_Expr (M));
            end if;
            if F_Renaming_Clause (M) /= No_Renaming_Clause then
               Put_Tokens (F_Renaming_Clause (M));
            end if;
            if Kind (Node) /= Ada_Extended_Return_Stmt_Object_Decl then
               Put_Line (";");
            end if;
         end;

         --  Ada_Package_Renaming_Decl, Ada_Single_Protected_Decl,
         --  Ada_Single_Task_Decl,
      when Ada_Case_Stmt_Alternative =>
         Put_Line ("when");
         Put_Tokens (Node.As_Case_Stmt_Alternative.F_Choices);
         Put_Line ("=>");
         Put_Tokens (Node.As_Case_Stmt_Alternative.F_Stmts);

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

         --  Ada_Component_Clause,
      when Ada_Component_Def =>
         Put_Tokens (Node.As_Component_Def.F_Has_Aliased);
         Put_Tokens (Node.As_Component_Def.F_Has_Constant);
         Put_Tokens (Node.As_Component_Def.F_Type_Expr);

      when Ada_Constant_Present =>
         Put_Line ("constant");

         --  Ada_Delta_Constraint, Ada_Digits_Constraint,
      when Ada_Discriminant_Constraint =>
         Put_Line ("(");
         Put_Tokens (Node.As_Discriminant_Constraint.F_Constraints);
         Put_Line (")");

      when Ada_Index_Constraint =>
         Put_Line ("(");
         Put_Ada_Node_List (Ada_Node_List (Node.As_Index_Constraint.F_Constraints), ",");
         Put_Line (")");

      when Ada_Range_Constraint =>
         Put_Line ("range");
         Put_Tokens (Node.As_Range_Constraint.F_Range);

      when Ada_Declarative_Part =>
         Put_Ada_Node_List (As_Declarative_Part (Node).F_Decls);

         --  Ada_Private_Part,
      when Ada_Public_Part =>
         for D of Node.As_Declarative_Part.F_Decls loop
            Put_Tokens (D);
         end loop;

      when Ada_Elsif_Expr_Part =>
         Put_Line ("elsif");
         Put_Tokens (Node.As_Elsif_Expr_Part.F_Cond_Expr);
         Put_Line ("then");
         Put_Tokens (Node.As_Elsif_Expr_Part.F_Then_Expr);

      when Ada_Elsif_Stmt_Part =>
         Put_Line ("elsif");
         Put_Tokens (Node.As_Elsif_Stmt_Part.F_Cond_Expr);
         Put_Line ("then");
         Put_Tokens (Node.As_Elsif_Stmt_Part.F_Stmts);

      when Ada_Allocator =>
         Put_Line ("new");
         if Node.As_Allocator.F_Subpool /= No_Ada_Node then
            Put_Line ("(");
            Put_Tokens (Node.As_Allocator.F_Subpool);
            Put_Line (")");
         end if;
         Put_Tokens (Node.As_Allocator.F_Type_Or_Expr);

      when Ada_Aggregate =>
         Put_Line ("(");
         if Node.As_Aggregate.F_Ancestor_Expr /= No_Ada_Node then
            Put_Tokens (Node.As_Aggregate.F_Ancestor_Expr);
            Put_Line ("with");
         end if;
         Put_Tokens (Node.As_Aggregate.F_Assocs);
         Put_Line (")");

         --  Ada_Null_Record_Aggregate,
      when Ada_Bin_Op | Ada_Relation_Op =>
         declare
            N : constant Bin_Op := As_Bin_Op (Node);
         begin
            Put_Tokens (N.F_Left);
            Put_Tokens (N.F_Op);
            Put_Tokens (N.F_Right);
         end;

      when Ada_Box_Expr =>
         Put_Line ("<>");

      when Ada_Case_Expr =>
         Put_Line ("case");
         Put_Tokens (Node.As_Case_Expr.F_Expr);
         Put_Line ("is");
         if Not_Empty (Node.As_Case_Expr.F_Cases) then
            Put_Tokens (Node.As_Case_Expr.F_Cases);
         end if;

      when Ada_Case_Expr_Alternative =>
         Put_Line ("when");
         Put_Tokens (Node.As_Case_Expr_Alternative.F_Choices);
         Put_Line ("=>");
         Put_Tokens (Node.As_Case_Expr_Alternative.F_Expr);

         --  Ada_Contract_Cases,
      when Ada_If_Expr =>
         Put_Line ("if");
         Put_Tokens (Node.As_If_Expr.F_Cond_Expr);
         Put_Line ("then");
         Put_Tokens (Node.As_If_Expr.F_Then_Expr);
         if Not_Empty (Node.As_If_Expr.F_Alternatives) then
            for N of Node.As_If_Expr.F_Alternatives loop
               Put_Tokens (N);
            end loop;
         end if;
         if Node.As_If_Expr.F_Else_Expr /= No_Expr then
            Put_Line ("else");
            Put_Tokens (Node.As_If_Expr.F_Else_Expr);
         end if;

      when Ada_Membership_Expr =>
         Put_Tokens (Node.As_Membership_Expr.F_Expr);
         Put_Tokens (Node.As_Membership_Expr.F_Op);
         Put_Tokens (Node.As_Membership_Expr.F_Membership_Exprs);

      when Ada_Attribute_Ref =>
         Put_Tokens (Node.As_Attribute_Ref.F_Prefix);
         Put_Line ("'");
         Put_Tokens (Node.As_Attribute_Ref.F_Attribute);
         if Node.As_Attribute_Ref.F_Args /= No_Ada_Node then
            Put_Line ("(");
            Put_Tokens (Node.As_Attribute_Ref.F_Args);
            Put_Line (")");
         end if;

         --  Ada_Update_Attribute_Ref,
      when Ada_Call_Expr =>
         declare
            N : constant Call_Expr := Node.As_Call_Expr;
         begin
            Put_Tokens (N.F_Name);
            if N.F_Suffix /= No_Ada_Node then
               Put_Line ("(");
               Put_Tokens (N.F_Suffix);
               Put_Line (")");
            end if;
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

      when Ada_Explicit_Deref =>
         Put_Tokens (Node.As_Explicit_Deref.F_Prefix);
         Put_Line (".");
         Put_Line ("all");

      when Ada_Qual_Expr =>
         Put_Tokens (Node.As_Qual_Expr.F_Prefix);
         Put_Line ("'");
         Put_Tokens (Node.As_Qual_Expr.F_Suffix);

      when Ada_Char_Literal =>
         Put ("CHARACTER_LITERAL "); Ada.Wide_Wide_Text_IO.Put_Line (Text (Token_Start (Node)));

      when Ada_Identifier =>
         Put ("IDENTIFIER "); Ada.Wide_Wide_Text_IO.Put_Line (Text (Token_Start (Node)));

      when Ada_Op_Abs =>
         Put_Line ("abs");

      when Ada_Op_And =>
         Put_Line ("and");

      when Ada_Op_And_Then =>
         Put_Line ("and");
         Put_Line ("then");

      when Ada_Op_Concat =>
         Put_Line ("&");

      when Ada_Op_Div =>
         Put_Line ("/");

      when Ada_Op_Double_Dot =>
         Put_Line ("..");

      when Ada_Op_Eq =>
         Put_Line ("=");

      when Ada_Op_Gt =>
         Put_Line (">");

      when Ada_Op_Gte =>
         Put_Line (">=");

      when Ada_Op_In =>
         Put_Line ("in");

      when Ada_Op_Lt =>
         Put_Line ("<");

      when Ada_Op_Lte =>
         Put_Line ("<=");

      when Ada_Op_Minus =>
         Put_Line ("-");

      when Ada_Op_Mod =>
         Put_Line ("mod");

      when Ada_Op_Mult =>
         Put_Line ("*");

      when Ada_Op_Neq =>
         Put_Line ("/=");

      when Ada_Op_Not =>
         Put_Line ("not");

      when Ada_Op_Not_In =>
         Put_Line ("not");
         Put_Line ("in");

      when Ada_Op_Or =>
         Put_Line ("or");

      when Ada_Op_Or_Else =>
         Put_Line ("or");
         Put_Line ("else");

      when Ada_Op_Plus =>
         Put_Line ("+");

      when Ada_Op_Pow =>
         Put_Line ("**");

      when Ada_Op_Rem =>
         Put_Line ("rem");

      when Ada_Op_Xor =>
         Put_Line ("xor");

      when Ada_String_Literal =>
         Put_Line ("STRING_LITERAL");

      when Ada_Null_Literal =>
         Put_Line ("null");
      when Ada_Int_Literal =>
         Put_Line ("NUMERIC_LITERAL");
         --  Ada_Real_Literal, Ada_Target_Name,

      when Ada_Paren_Expr =>
         Put_Line ("(");
         Put_Tokens (Node.As_Paren_Expr.F_Expr);
         Put_Line (")");

      when Ada_Quantified_Expr =>
         Put_Line ("for");
         Put_Tokens (Node.As_Quantified_Expr.F_Quantifier);
         Put_Tokens (Node.As_Quantified_Expr.F_Loop_Spec);
         Put_Line ("=>");
         Put_Tokens (Node.As_Quantified_Expr.F_Expr);

      when Ada_Raise_Expr =>
         Put_Line ("raise");
         Put_Tokens (Node.As_Raise_Expr.F_Exception_Name);
         if Node.As_Raise_Expr.F_Error_Message /= No_Expr then
            Put_Line ("with");
            Put_Tokens (Node.As_Raise_Expr.F_Error_Message);
         end if;

      when Ada_Un_Op =>
         Put_Tokens (Node.As_Un_Op.F_Op);
         Put_Tokens (Node.As_Un_Op.F_Expr);

      when Ada_Handled_Stmts =>
         declare
            M : constant Handled_Stmts := As_Handled_Stmts (Node);
         begin
            Put_Ada_Node_List (Ada_Node_List (M.F_Stmts));
            if Not_Empty (M.F_Exceptions) then
               Put_Line ("exception");
               for N of M.F_Exceptions loop
                  Put_Tokens (N);
               end loop;
            end if;
         end;

         --  Ada_Interface_Kind_Limited, Ada_Interface_Kind_Protected,
         --  Ada_Interface_Kind_Synchronized, Ada_Interface_Kind_Task,
      when Ada_Iter_Type_In =>
         Put_Line ("in");
      when Ada_Iter_Type_Of =>
         Put_Line ("of");

      when Ada_Library_Item =>
         Put_Tokens (As_Library_Item (Node).F_Item);

         --  Ada_Limited_Present,
      when Ada_For_Loop_Spec =>
         declare
            N : constant For_Loop_Spec := Node.As_For_Loop_Spec;
         begin
            Put_Tokens (N.F_Var_Decl);
            Put_Tokens (N.F_Loop_Type);
            Put_Tokens (N.F_Has_Reverse);
            Put_Tokens (N.F_Iter_Expr);
         end;

         --  Ada_While_Loop_Spec,
         --
      when Ada_Mode_Default =>
         null;

      when Ada_Mode_In =>
         Put_Line ("in");

      when Ada_Mode_In_Out =>
         Put_Line ("in");
         Put_Line ("out");

      when Ada_Mode_Out =>
         Put_Line ("out");

      when Ada_Not_Null_Present =>
         Put_Line ("not");
         Put_Line ("null");

         --  Ada_Null_Component_Decl,
      when Ada_Others_Designator =>
         Put_Line ("others");

      when Ada_Overriding_Not_Overriding =>
         Put_Line ("not");
         Put_Line ("overriding");

      when Ada_Overriding_Overriding =>
         Put_Line ("overriding");

      when Ada_Params =>
         Put_Line ("(");
         Put_Tokens (Node.As_Params.F_Params);
         Put_Line (")");

      when Ada_Pragma_Node =>
         declare
            N : constant Pragma_Node := As_Pragma_Node (Node);
         begin
            Put_Line ("pragma");
            Put_Tokens (N.F_Id);
            if Not_Empty (N.F_Args) then
               Put_Line ("(");
               Put_Tokens (N.F_Args);
               Put_Line (")");
            end if;
            Put_Line (";");
         end;

         --  Ada_Prim_Type_Accessor
         --  Ada_Private_Present, Ada_Protected_Def, ,
         --  Ada_Protected_Present, Ada_Quantifier_All,
      when Ada_Quantifier_Some =>
         Put_Line ("some");

      when Ada_Range_Spec =>
         Put_Tokens (Node.As_Range_Spec.F_Range);

      when Ada_Renaming_Clause =>
         Put_Line ("renames");
         Put_Tokens (Node.As_Renaming_Clause.F_Renamed_Object);

      when Ada_Reverse_Present =>
         Put_Line ("reverse");

         --  Ada_Select_When_Part, Ada_Accept_Stmt,
         --  Ada_Accept_Stmt_With_Stmts,
      when Ada_For_Loop_Stmt =>
         Put_Line ("for");
         Put_Tokens (Node.As_For_Loop_Stmt.F_Spec);
         Put_Line ("loop");
         Put_Tokens (Node.As_For_Loop_Stmt.F_Stmts);
         Put_Line ("end");
         Put_Line ("loop");
         Put_Tokens (Node.As_For_Loop_Stmt.F_End_Name);
         Put_Line (";");

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

      when Ada_Decl_Block =>
         declare
            N : constant Decl_Block := Node.As_Decl_Block;
         begin
            Put_Line ("declare");
            Put_Tokens (N.F_Decls);
            Put_Line ("begin");
            Put_Tokens (N.F_Stmts);
            Put_Line ("end");
            Put_Tokens (N.F_End_Name);
            Put_Line (";");
         end;

      when Ada_Case_Stmt =>
         Put_Line ("case");
         Put_Tokens (Node.As_Case_Stmt.F_Expr);
         Put_Line ("is");
         Put_Tokens (Node.As_Case_Stmt.F_Alternatives);
         Put_Line ("end");
         Put_Line ("case");
         Put_Line (";");

      when Ada_Extended_Return_Stmt =>
         Put_Line ("return");
         Put_Tokens (Node.As_Extended_Return_Stmt.F_Decl);
         Put_Line ("do");
         Put_Tokens (Node.As_Extended_Return_Stmt.F_Stmts);
         Put_Line ("end");
         Put_Line ("return");
         Put_Line (";");

      when Ada_If_Stmt =>
         declare
            N : constant If_Stmt := As_If_Stmt (Node);
         begin
            Put_Line ("if");
            Put_Tokens (N.F_Cond_Expr);
            Put_Line ("then");
            Put_Ada_Node_List (Ada_Node_List (N.F_Then_Stmts));
            if Not_Empty (N.F_Alternatives) then
               for I of N.F_Alternatives loop
                  Put_Tokens (I);
               end loop;
            end if;
            if Not_Empty (Ada_Node_List (N.F_Else_Stmts)) then
               Put_Line ("else");
               Put_Ada_Node_List (Ada_Node_List (N.F_Else_Stmts));
            end if;
            Put_Line ("end");
            Put_Line ("if");
            Put_Line (";");
         end;

      when Ada_Named_Stmt =>
         Put_Tokens (Node.As_Named_Stmt.F_Decl);
         Put_Tokens (Node.As_Named_Stmt.F_Stmt);

         --  Ada_Select_Stmt,
      when Ada_Error_Stmt | Ada_Error_Decl =>
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
            Put_Line (";");
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

         --  Ada_Goto_Stmt,
      when Ada_Label =>
         Put_Line ("<<");
         Put_Tokens (Node.As_Label.F_Decl);
         Put_Line (">>");

      when Ada_Null_Stmt =>
         Put_Line ("null");
         Put_Line (";");

      when Ada_Raise_Stmt =>
         Put_Line ("raise");
         Put_Tokens (Node.As_Raise_Stmt.F_Exception_Name);
         if Node.As_Raise_Stmt.F_Error_Message /= No_Expr then
            Put_Line ("with");
            Put_Tokens (Node.As_Raise_Stmt.F_Error_Message);
         end if;
         Put_Line (";");

         --  Ada_Requeue_Stmt

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
      when Ada_Task_Def =>
         declare
            M : constant Task_Def := As_Task_Def (Node);
         begin
            Put_Tokens (M.F_Interfaces);
            Put_Tokens (M.F_Public_Part);
            if M.F_Private_Part /= No_Private_Part then
               Put_Line ("private");
               Put_Tokens (M.F_Private_Part);
            end if;
            Put_Line ("end");
            Put_Tokens (M.F_End_Name);
            Put_Line (";");
         end;

         --  Ada_Access_To_Subp_Def, Ada_Anonymous_Type_Access_Def,
      when Ada_Type_Access_Def =>
         declare
            N : constant Type_Access_Def := Node.As_Type_Access_Def;
         begin
            Put_Tokens (N.F_Has_Not_Null);
            Put_Line ("access");
            Put_Tokens (N.F_Has_All);
            Put_Tokens (N.F_Has_Constant);
            Put_Tokens (N.F_Subtype_Indication);
         end;

      when Ada_Array_Type_Def =>
         Put_Line ("array");
         Put_Line ("(");
         Put_Tokens (Node.As_Array_Type_Def.F_Indices);
         Put_Line (")");
         Put_Line ("of");
         Put_Tokens (Node.As_Array_Type_Def.F_Component_Type);

         --  Ada_Derived_Type_Def,
         --  Ada_Enum_Type_Def, Ada_Formal_Discrete_Type_Def, Ada_Interface_Type_Def,
         --  Ada_Mod_Int_Type_Def, Ada_Private_Type_Def, Ada_Decimal_Fixed_Point_Def,
         --  Ada_Floating_Point_Def, Ada_Ordinary_Fixed_Point_Def,
         --  Ada_Record_Type_Def, Ada_Signed_Int_Type_Def,
      when Ada_Anonymous_Type =>
         Put_Tokens (Node.As_Anonymous_Type.F_Type_Decl);

         --  Ada_Enum_Lit_Synth_Type_Expr,
      when Ada_Subtype_Indication =>
         declare
            N : constant Subtype_Indication := As_Subtype_Indication (Node);
         begin
            if N.F_Has_Not_Null then
               Put_Tokens (N.F_Has_Not_Null);
            end if;
            Put_Tokens (N.F_Name);
            Put_Tokens (N.F_Constraint);
         end;

         --  Ada_Constrained_Subtype_Indication, Ada_Discrete_Subtype_Indication,
         --  Ada_Unconstrained_Array_Index, , Ada_Until_Present,

      when Ada_Use_Package_Clause =>
         Put_Line ("use");
         Put_Tokens (Node.As_Use_Package_Clause.F_Packages);
         Put_Line (";");

      when Ada_Use_Type_Clause =>
         Put_Line ("use");
         Put_Tokens (Node.As_Use_Type_Clause.F_Has_All);
         Put_Line ("type");
         Put_Tokens (Node.As_Use_Type_Clause.F_Types);
         Put_Line (";");

         --  Ada_Variant,
         --  Ada_Variant_Part,
      when Ada_With_Clause =>
         declare
            N : constant With_Clause := As_With_Clause (Node);
         begin
            if F_Has_Limited (N) then
               Put_Tokens (F_Has_Limited (N));
            end if;
            if F_Has_Private (N) then
               Put_Tokens (F_Has_Private (N));
            end if;
            Put_Line ("with");
            Put_Tokens (F_Packages (N));
            Put_Line (";");
         end;

         --  Ada_With_Private_Present =>
      when others =>
         Put_Line ("unhandled: " & Kind (Node)'Image);
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
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
