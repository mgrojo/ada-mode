--  Abstract :
--
--  Type and operations for graphs.
--
--  References:
--
--  [1] Introduction to Algorithms, Thomas H. Cormen, Charles E.
--  Leiserson, Ronald L. Rivest, Clifford Stein.
--
--  Copyright (C) 2017 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers.Doubly_Linked_Lists;
generic
   type Edge_Data is private;
   Default_Edge_Data : in Edge_Data;
   type Vertex_Index is (<>);
package SAL.Gen_Graphs is

   type Graph is tagged private;

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data);
   --  Adds a directed edge from Vertex_A to Vertex_B.

   type Path_Item is record
      Vertex : Vertex_Index;
      Edge   : Edge_Data; -- leading to Vertex
   end record;

   type Path is array (Positive range <>) of Path_Item;

   type Find_Label is (Edge, Vertex);
   type Find_Target (Label : Find_Label) is record
      case Label is
      when Edge =>
         Data : Edge_Data;

      when Vertex =>
         Vertex : Vertex_Index;
      end case;
   end record;

   function Find_Path (Graph : in out Gen_Graphs.Graph; From : in Vertex_Index; To : in Find_Target) return Path;
   --  First entry in result is From, with first edge.
   --
   --  If To.Label is:
   --
   --  - Edge, last entry in result contains edge data for To, leaving
   --  last vertex.
   --
   --  - Vertex, last entry in result contains To, with default edge
   --  data.

private
   type Edge_Node is record
      Vertex_B : Vertex_Index;
      Data     : Edge_Data;
   end record;

   package Edge_Lists is new Ada.Containers.Doubly_Linked_Lists (Edge_Node);

   type Colors is (White, Gray, Black);

   type Vertex_Node is record
      Edges       : Edge_Lists.List;
      Color       : Colors;
      D           : Natural;
      Parent      : Vertex_Index'Base; -- FIXME: use greek Pi; need to fix wisitoken to use better lexer
      Parent_Set  : Boolean;
      Parent_Edge : Edge_Lists.Cursor;
   end record;

   type Vertex_Array is array (Vertex_Index) of Vertex_Node;

   type Graph is tagged record
      Vertices : Vertex_Array;
   end record;

end SAL.Gen_Graphs;
