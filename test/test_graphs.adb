--  Abstract :
--
--  See spec.
--
--  References:
--
--  see sal-gen_graphs.ads
--
--  Copyright (C) 2017, 2018 Stephen Leake.  All Rights Reserved.
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
with SAL.Gen_Graphs.Gen_Aunit;
package body Test_Graphs is

   --  Test uses the graph in [1] figure 22.3. That does not have
   --  labels or weights on the edges; we add an integer to each to
   --  allow testing edge_data. We also treat it as a directed graph.

   type Vertex_Index is (R, S, T, U, V, W, X, Y);

   package Graphs is new SAL.Gen_Graphs
     (Edge_Data         => Integer,
      Default_Edge_Data => 0,
      Vertex_Index      => Vertex_Index);
   use Graphs;

   package Graphs_Aunit is new Graphs.Gen_Aunit (AUnit.Checks.Check);

   ----------
   --  Test procedures

   procedure Nominal (Tst : in out AUnit.Test_Cases.Test_Case'Class)
   is
      pragma Unreferenced (Tst);
      use AUnit.Checks.Containers;
      use Graphs_Aunit;

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

   end Nominal;

   ----------
   --  Public routines

   overriding procedure Register_Tests (T : in out Test_Case)
   is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Nominal'Access, "Nominal");
   end Register_Tests;

   overriding function Name (T : Test_Case) return AUnit.Message_String
   is
      pragma Unreferenced (T);
   begin
      return new String'("test_graphs.adb");
   end Name;

end Test_Graphs;
