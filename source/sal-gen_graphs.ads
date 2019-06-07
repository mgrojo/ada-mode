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
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
generic
   type Edge_Data is private;
   Default_Edge_Data : in Edge_Data;
   type Vertex_Index is (<>);
package SAL.Gen_Graphs is

   Invalid_Vertex : constant Vertex_Index'Base := Vertex_Index'Base'Pred (Vertex_Index'First);

   type Graph is tagged private;

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data);
   --  Adds a directed edge from Vertex_A to Vertex_B.

   type Path_Item is record
      Vertex : Vertex_Index'Base;
      Edge   : Edge_Data; -- leading to Vertex
   end record;

   type Path is array (Positive range <>) of Path_Item;

   package Path_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Path);

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Lists.List;
   --  Return all non-cyclic paths starting at From that lead to a To edge.
   --  First entry in each item in result is From, with first edge. Last
   --  entry in result contains edge data for To, leaving last vertex.

   function Find_Cycles (Graph : in out Gen_Graphs.Graph) return Path_Lists.List;
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

      --  FIXME: The following are used in the Find_Path algorithm; move to
      --  a derived type.
      Color       : Colors;
      D           : Natural;
      Parent      : Vertex_Index'Base;
      Parent_Set  : Boolean;
      Parent_Edge : Edge_Lists.Cursor;
   end record;

   type Vertex_Array is array (Vertex_Index) of Vertex_Node;

   type Graph is tagged record
      Vertices : Vertex_Array;
   end record;

end SAL.Gen_Graphs;
