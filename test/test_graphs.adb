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
with SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit;
with SAL.Gen_Graphs.Gen_AUnit;
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

      type Base_Vertex_Index is (Invalid, R, S, T, U, V, W, X, Y);
      pragma Unreferenced (Invalid);
      subtype Vertex_Index is Base_Vertex_Index range R .. Y;

      package Graphs is new SAL.Gen_Graphs
        (Edge_Data         => Integer,
         Default_Edge_Data => 0,
         Vertex_Index      => Vertex_Index);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      use Graphs_AUnit;

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Lists.List;
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
         Vertex_Index      => Vertex_Index);
      use Graphs;

      package Graphs_AUnit is new Graphs.Gen_AUnit (AUnit.Checks.Check);
      package Paths_AUnit is new SAL.Ada_Containers.Gen_Indefinite_Doubly_Linked_Lists_AUnit
        (Element_Type  => Path,
         "="           => "=",
         Lists         => Path_Lists,
         Check_Element => Graphs_AUnit.Check);
      use Paths_AUnit;

      Graph    : Graphs.Graph;
      Computed : Graphs.Path_Lists.List;
      Expected : Graphs.Path_Lists.List;
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
      Expected.Append (((1, 0), (2, 1), (3, 3), (5, 5)));
      Expected.Append (((1, 0), (2, 1), (4, 4), (3, 6), (5, 5)));
      Expected.Append ((1 => (2, 0)));

      Computed := Graph.Find_Cycles;
      Check ("1", Computed, Expected);
   end Test_Find_Cycles;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Find_Path'Access, "Test_Find_Path");
      Register_Routine (T, Test_Find_Cycles'Access, "Test_Find_Cycles");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_graphs.adb");
   end Name;

end Test_Graphs;
