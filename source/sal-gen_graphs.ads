--  Abstract :
--
--  Type and operations for graphs.
--
--  References:
--
--  [1] Introduction to Algorithms, Thomas H. Cormen, Charles E.
--  Leiserson, Ronald L. Rivest, Clifford Stein.
--
--  [2] "An Efficient Search Algorithm to Find the Elementary Circuits
--  of a Graph", James C. Tiernan, Communications of the ACM Volume 13
--  Number 12 December 1970.
--  https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.516.9454&rep=rep1&type=pdf
--
--  Copyright (C) 2017, 2019 Stephen Leake All Rights Reserved.
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
with Ada.Containers.Indefinite_Vectors;
with SAL.Gen_Unbounded_Definite_Vectors;
generic
   type Edge_Data is private;
   Default_Edge_Data : in Edge_Data;
   type Vertex_Index is range <>;
   Invalid_Vertex : in Vertex_Index'Base;

   type Path_Index is range <>;
package SAL.Gen_Graphs is

   type Graph is tagged private;

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data);
   --  Adds a directed edge from Vertex_A to Vertex_B.

   type Path_Item is record
      Vertex : Vertex_Index'Base := Invalid_Vertex;
      Edge   : Edge_Data         := Default_Edge_Data; -- leading to Vertex
   end record;

   type Path is array (Positive range <>) of Path_Item;

   package Path_Arrays is new Ada.Containers.Indefinite_Vectors (Path_Index, Path);

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Arrays.Vector;
   --  Return all non-cyclic paths starting at From that lead to a To edge.
   --  First entry in each item in result is From, with first edge. Last
   --  entry in result contains edge data for To, leaving last vertex.

   function Find_Cycles (Graph : in out Gen_Graphs.Graph) return Path_Arrays.Vector;
   --  Return all cyclic paths in Graph.

private
   type Edge_Node is record
      Vertex_B : Vertex_Index;
      Data     : Edge_Data; -- to Vertex_B
   end record;

   package Edge_Lists is new Ada.Containers.Doubly_Linked_Lists (Edge_Node);

   type Colors is (White, Gray, Black);

   type Vertex_Node is record
      Edges       : Edge_Lists.List;

      --  FIXME: The following are used only in the Find_Path algorithm; move to
      --  a separate type?
      Color       : Colors            := Colors'First;
      D           : Natural           := Natural'Last;
      Parent      : Vertex_Index'Base := Invalid_Vertex;
      Parent_Set  : Boolean           := False;
      Parent_Edge : Edge_Lists.Cursor := Edge_Lists.No_Element;
   end record;

   package Vertex_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Vertex_Index, Vertex_Node, (others => <>));

   type Graph is tagged record
      Vertices : Vertex_Arrays.Vector;
   end record;

end SAL.Gen_Graphs;
