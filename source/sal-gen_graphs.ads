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

   with function Edge_Image (Item : in Edge_Data) return String;

package SAL.Gen_Graphs is

   type Graph is tagged private;

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data);
   --  Adds a directed edge from Vertex_A to Vertex_B.

   function Multigraph (Graph : in Gen_Graphs.Graph) return Boolean;
   --  If more than one edge is added between two vertices, the graph is
   --  a multigraph. The edges are given separate identifiers internally.

   Multigraph_Error : exception;

   type Base_Edge_ID is range 0 .. Integer'Last;
   subtype Edge_ID is Base_Edge_ID range 1 .. Base_Edge_ID'Last;
   Invalid_Edge_ID : constant Base_Edge_ID := 0;
   --  Edge ids are unique graph-wide, assigned by Add_Edge.

   type Path_Item is record
      Vertex  : Vertex_Index'Base       := Invalid_Vertex;
      Edge_ID : Gen_Graphs.Base_Edge_ID := Invalid_Edge_ID;
      Edge    : Edge_Data               := Default_Edge_Data;
      --  Edge_ID, Edge describe the edge leading from the previous vertex
      --  in the path to Vertex. If this is the first vertex in an open
      --  path, Edge_ID is Invalid_Edge_ID. If it is the first vertex in a
      --  cycle, the edge is from the last vertex in the cycle.
   end record;

   type Path is array (Positive range <>) of Path_Item;

   function Image (Item : in Path; Include_Edge_ID : in Boolean := False) return String;
   --  For trace, debugging.

   package Path_Arrays is new Ada.Containers.Indefinite_Vectors (Path_Index, Path);

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Arrays.Vector;
   --  Return all non-cyclic paths starting at From that lead to a To
   --  edge, using algorithm [1]. First entry in each item in result is
   --  From, with first edge. Last entry in result contains edge data for
   --  To.
   --
   --  Raises Multigraph_Error if Graph is a multigraph.

   function Find_Cycles (Graph : in out Gen_Graphs.Graph) return Path_Arrays.Vector;
   --  Return all cyclic paths in Graph, using algorithm [2] extended for
   --  multigraphs.

private

   type Edge_Node is record
      --  Edge is from vertex contaning this Node to Vertex_B
      ID         : Edge_ID;
      Vertex_B   : Vertex_Index;
      Data       : Edge_Data;
   end record;

   package Edge_Lists is new Ada.Containers.Doubly_Linked_Lists (Edge_Node);

   package Vertex_Arrays is new SAL.Gen_Unbounded_Definite_Vectors
     (Vertex_Index, Edge_Lists.List, Edge_Lists.Empty_List);

   type Graph is tagged record
      Last_Edge_ID : Base_Edge_ID := Invalid_Edge_ID;
      Multigraph   : Boolean      := False;
      Vertices     : Vertex_Arrays.Vector;
   end record;

end SAL.Gen_Graphs;
