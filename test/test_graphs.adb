--  Abstract :
--
--  See spec.
--
--  References:
--
--  see sal-gen_graphs.ads
--
--  Copyright (C) 2017 - 2019 Stephen Leake.  All Rights Reserved.
--
--  This program is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or (at
--  your option) any later version. This program is distributed in the
--  hope that it will be useful, but WITHOUT ANY WARRANTY; without even
--  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE. See the GNU General Public License for more details. You
--  should have received a copy of the GNU General Public License
--  distributed with this program; see file COPYING. If not, write to
--  the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
--  MA 02111-1307, USA.

pragma License (GPL);
with AUnit.Checks.Containers;
with Ada.Text_IO;
with SAL.Gen_Graphs.Gen_AUnit;
with SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit;
package body Test_Graphs is

   ----------
   --  Test procedures

   procedure Test_Find_Path (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Tst);
      use AUnit.Checks.Containers;

      --  Test uses the graph in [1] figure 22.3. That does not have
      --  labels or weights on the edges; we add an integer to each to
      --  allow testing edge_data. We also treat it as a directed graph.

      R : constant Integer := 1;
      S : constant Integer := 2;
      T : constant Integer := 3;
      U : constant Integer := 4;
      V : constant Integer := 5;
      W : constant Integer := 6;
      X : constant Integer := 7;
      Y : constant Integer := 8;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => 0,
         Path_Index        => Positive);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check, Integer'Image);
      use Graphs_AUnit;

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Arrays.Vector;
   begin
      --  Fill graph.

      Graph.Add_Edge (V, R, 1);
      Graph.Add_Edge (R, S, 2);
      Graph.Add_Edge (S, W, 3);
      Graph.Add_Edge (W, T, 4);
      Graph.Add_Edge (W, X, 5);
      Graph.Add_Edge (T, U, 6);
      Graph.Add_Edge (T, X, 7);
      Graph.Add_Edge (U, Y, 8);
      Graph.Add_Edge (X, U, 9);
      Graph.Add_Edge (X, Y, 10);

      Computed := Graph.Find_Paths (V, 2);
      Check ("v - 2.length", Computed.Length, 1);
      Check ("v - 2.first", Computed (Computed.First), ((V, 1), (R, 2)));

      Computed := Graph.Find_Paths (V, 10);
      Check ("v - 10.length", Computed.Length, 1);
      Check ("v - 10.first", Computed (Computed.First), ((V, 1), (R, 2), (S, 3), (W, 5), (X, 10)));

   end Test_Find_Path;

   procedure Test_Find_Cycles (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Tst);

      --  Test uses the graph in [2] figure 1.

      type Base_Vertex_Index is range 0 .. 5;
      subtype Vertex_Index is Base_Vertex_Index range 1 .. 5;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer, -- "edge number"
         Default_Edge_Data => 0,
         Vertex_Index      => Vertex_Index,
         Invalid_Vertex    => 6,
         Path_Index        => Positive);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check, Integer'Image);

      procedure Check is new SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit
        (Index_Type    => Positive,
         Element_Type  => Path,
         Vectors       => Graphs.Path_Arrays,
         Check_Index   => Graphs_AUnit.Check,
         Check_Element => Graphs_AUnit.Check);

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Arrays.Vector;
      Expected : Graphs.Path_Arrays.Vector;
   begin
      --  Fill graph.

      Graph.Add_Edge (1, 2, 1);
      Graph.Add_Edge (2, 2, 2);
      Graph.Add_Edge (2, 3, 3);
      Graph.Add_Edge (2, 4, 4);
      Graph.Add_Edge (3, 5, 5);
      Graph.Add_Edge (4, 3, 6);
      Graph.Add_Edge (5, 1, 7);

      --  Set expected as in [2] fig 2 page 723
      Expected.Append (Path'((1, 7), (2, 1), (3, 3), (5, 5)));
      Expected.Append (Path'((1, 7), (2, 1), (4, 4), (3, 6), (5, 5)));
      Expected.Append (Path'(1 => (2, 2)));

      Computed := Graph.Find_Cycles;
      Check ("1", Computed, Expected);
   end Test_Find_Cycles;

   procedure Test_Conflict_Name (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      Test : Test_Case renames Test_Case (Tst);

      --  Graph is from WisiToken conflict_name.wy grammar.
      --
      --  Nodes are nonterminals, edges are occurence of nonterminal in a
      --  production.
      --
      --  Nonterminals:
      --   6 => wisitoken_accept
      --   7 => aggregate
      --   8 => attribute_reference
      --   9 => attribute_designator
      --  10 => name
      --  11 => qualified_expression

      --  Productions:
      --   6.0 wisitoken_accept <= name Wisi_EOI
      --
      --   7.0 aggregate <= LEFT_PAREN name RIGHT_PAREN
      --
      --   8.0 attribute_reference <= name TICK attribute_designator
      --
      --   9.0 attribute_designator <= name
      --
      --        name
      --  10.0  <= IDENTIFIER
      --  10.1  | attribute_reference
      --  10.2  | qualified_expression
      --
      --  11.0 qualified_expression <= name TICK aggregate


      --  In WisiToken, we might want Edge_Data to be:
      --  type Edge is record
      --     LHS       : Positive  := 1;
      --     RHS       : Natural   := 10;
      --     Token     : Positive  := 10;
      --     Recursive : Recursion := None;
      --  end record;
      --
      --  But using that here just makes the test harder to read

      type Unknown_Recursion_Index is range 0 .. Integer'Last;
      subtype Recursion_Index is Unknown_Recursion_Index range 1 .. Unknown_Recursion_Index'Last;
      Invalid_Recursion_Index : constant Unknown_Recursion_Index := 0;
      pragma Unreferenced (Invalid_Recursion_Index);

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Positive,
         Invalid_Vertex    => Integer'Last,
         Path_Index        => Recursion_Index);
      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check, Integer'Image);

      procedure Check is new SAL.Ada_Containers.Gen_Indefinite_Vectors_AUnit
        (Index_Type    => Recursion_Index,
         Element_Type  => Graphs.Path,
         "="           => Graphs."=",
         Vectors       => Graphs.Path_Arrays,
         Check_Index   => Graphs_AUnit.Check,
         Check_Element => Graphs_AUnit.Check);

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Arrays.Vector;
      Expected : Graphs.Path_Arrays.Vector;

      use Graphs;
   begin
      Graph.Add_Edge  (6, 10, 1);
      Graph.Add_Edge  (7, 10, 2);
      Graph.Add_Edge  (8, 10, 3);
      Graph.Add_Edge  (8,  9, 4);
      Graph.Add_Edge  (9, 10, 5);
      Graph.Add_Edge (10,  8, 6);
      Graph.Add_Edge (10, 11, 7);
      Graph.Add_Edge (11, 10, 8);
      Graph.Add_Edge (11,  7, 9);

      --  Cycles are found in start nonterminal order, arbitrary within
      --  start nonterminal; cycles start with lowest nonterm.
      Expected.Append (Path'((7, 9), (10, 2), (11, 7)));
      Expected.Append (Path'((8, 6), (10, 3)));
      Expected.Append (Path'((8, 6), (9, 4), (10, 5)));
      Expected.Append (Path'((10, 8), (11, 7)));

      Computed := Graph.Find_Cycles;
      if Test.Trace > 0 then
         for Cycle of Computed loop
            Ada.Text_IO.Put_Line (Graphs_AUnit.Image (Cycle));
         end loop;
      end if;

      Check ("1", Computed, Expected);
   end Test_Conflict_Name;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Path'Access, "Test_Find_Path");
      Register_Routine (T, Test_Find_Cycles'Access, "Test_Find_Cycles");
      Register_Routine (T, Test_Conflict_Name'Access, "Test_Conflict_Name");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_graphs.adb");
   end Name;

end Test_Graphs;
