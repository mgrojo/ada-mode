--  Abstract :
--
--  See spec.
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

with Ada.Strings.Unbounded;
with SAL.Gen_Bounded_Definite_Queues;
with SAL.Gen_Trimmed_Image;
package body SAL.Gen_Graphs is

   package Vertex_Queues is new SAL.Gen_Bounded_Definite_Queues (Vertex_Index);

   function Find (Data : in Edge_Data; List : in Edge_Lists.List) return Edge_Lists.Cursor
   is begin
      for I in List.Iterate loop
         if Edge_Lists.Element (I).Data = Data then
            return I;
         end if;
      end loop;
      return Edge_Lists.No_Element;
   end Find;

   function Trimmed_Image is new SAL.Gen_Trimmed_Image (Vertex_Index);

   ----------
   --  Visible subprograms

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data)
   is
      Multigraph : Boolean := False;

      procedure Update_First_Last (Vertex : in Vertex_Index)
      is
         use all type Ada.Containers.Count_Type;
      begin
         if Graph.Vertices.Length = 0 then
            Graph.Vertices.Set_First_Last (Vertex, Vertex);
         else
            if Vertex < Graph.Vertices.First_Index then
               Graph.Vertices.Set_First (Vertex);
            end if;
            if Vertex > Graph.Vertices.Last_Index then
               Graph.Vertices.Set_Last (Vertex);
            end if;
         end if;
      end Update_First_Last;

   begin
      Update_First_Last (Vertex_A);
      Update_First_Last (Vertex_B);

      Graph.Last_Edge_ID := Graph.Last_Edge_ID + 1;
      if (for some E of Graph.Vertices (Vertex_A) => E.Vertex_B = Vertex_B) then
         Multigraph       := True;
         Graph.Multigraph := True;
      end if;

      Graph.Vertices (Vertex_A).Append ((Graph.Last_Edge_ID, Vertex_B, Multigraph, Data));
   end Add_Edge;

   function Multigraph (Graph : in Gen_Graphs.Graph) return Boolean
   is begin
      return Graph.Multigraph;
   end Multigraph;

   function Image (Item : in Path) return String
   is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String := To_Unbounded_String ("(");

   begin
      for I in Item'Range loop
         Result := Result & Trimmed_Image (Item (I).Vertex) &
           --  FIXME: add edge id?
           Edge_Image ((if I = Item'Last then Item (Item'First).Edge else Item (I).Edge)) & " -> ";
      end loop;
      Result := Result & ")";
      return To_String (Result);
   end Image;

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Arrays.Vector
   is
      Vertex_Queue  : Vertex_Queues.Queue_Type
        (Size => Integer (Graph.Vertices.Last_Index - Graph.Vertices.First_Index + 1));

      type Aux_Node is record
         Color       : Colors            := Colors'First;
         D           : Natural           := Natural'Last;
         Parent      : Vertex_Index'Base := Invalid_Vertex;
         Parent_Set  : Boolean           := False;
         Parent_Edge : Edge_Lists.Cursor := Edge_Lists.No_Element;
      end record;

      package Aux_Arrays is new SAL.Gen_Unbounded_Definite_Vectors (Vertex_Index, Aux_Node, (others => <>));
      Aux : Aux_Arrays.Vector;

      function Build_Path
        (Tail_Vertex : in Vertex_Index;
         Tail_Edge   : in Edge_Lists.Cursor)
        return Path
      is
      begin
         return Result : Path (1 .. Aux (Tail_Vertex).D + 1)
         do
            declare
               use Edge_Lists;
               V_Index   : Vertex_Index := Tail_Vertex;
               Last_Edge : Cursor       := Tail_Edge;
            begin
               for I in reverse 1 .. Result'Length loop
                  declare
                     V : Aux_Node renames Aux (V_Index);
                  begin
                     if Last_Edge = No_Element then
                        Result (I) := (V_Index, Invalid_Edge_ID, Default_Edge_Data);
                     else
                        Result (I) := (V_Index, Element (Last_Edge).ID, Element (Last_Edge).Data);
                     end if;

                     if V.Parent_Set then
                        Last_Edge := V.Parent_Edge;
                        V_Index   := V.Parent;
                     end if;
                  end;
               end loop;
            end;
         end return;
      end Build_Path;

      Result_List : Path_Arrays.Vector;
      Result_Edge : Edge_Lists.Cursor;
   begin
      --  [1] figure 22.3 breadth-first search; 'From' = s.

      Aux.Set_First_Last (Graph.Vertices.First_Index, Graph.Vertices.Last_Index);

      for I in Aux.First_Index .. Aux.Last_Index loop
         if I = From then
            Aux (I).Color      := Gray;
            Aux (I).D          := 0;
            Aux (I).Parent_Set := False;

         else
            Aux (I).Color      := White;
            Aux (I).D          := Natural'Last;
            Aux (I).Parent_Set := False;
         end if;
      end loop;

      Vertex_Queue.Put (From);

      while not Vertex_Queue.Is_Empty loop
         declare
            U_Index : constant Vertex_Index := Vertex_Queue.Get;
            U       : Aux_Node renames Aux (U_Index);
         begin
            Edges :
            for C in Graph.Vertices (U_Index).Iterate loop
               declare
                  use all type Edge_Lists.Cursor;
                  V_Index : constant Vertex_Index := Edge_Lists.Element (C).Vertex_B;
                  V       : Aux_Node renames Aux (V_Index);
               begin
                  if V.Color = White then
                     V.Color       := Gray;
                     V.D           := U.D + 1;
                     V.Parent      := U_Index;
                     V.Parent_Edge := C;
                     V.Parent_Set  := True;

                     Result_Edge := Find (To, Graph.Vertices (V_Index));
                     if Result_Edge /= Edge_Lists.No_Element then
                        Result_List.Append (Build_Path (V_Index, Result_Edge));
                     end if;

                     Vertex_Queue.Put (V_Index);
                  end if;
               end;
            end loop Edges;
            U.Color := Black;
         end;
      end loop;
      return Result_List;
   end Find_Paths;

   function Find_Cycles (Graph : in out Gen_Graphs.Graph) return Path_Arrays.Vector
   is
      --  Implements [2] "Algorithm EC"
      --
      --  vertex 0 = Invalid_Vertex
      --  vertex 1 = Graph.Vertices.First_Index
      --  vertex N = Graph.Vertices.Last_Index

      First : Vertex_Index renames Graph.Vertices.First_Index;
      Last  : Vertex_Index renames Graph.Vertices.Last_Index;

      G : Vertex_Arrays.Vector renames Graph.Vertices;
      P : Path (1 .. Integer (Last - First + 1));
      K : Positive := 1; -- ie P_Last

      type H_Row is array (G.First_Index .. G.Last_Index) of Vertex_Index'Base;
      H : array (G.First_Index .. G.Last_Index) of H_Row := (others => (others => Invalid_Vertex));

      Next_Vertex_Found : Boolean;

      Result : Path_Arrays.Vector;

      function Contains (P : in Path; V : in Vertex_Index) return Boolean
      is (for some N of P => N.Vertex = V);

      function Contains (Row : in H_Row; V : in Vertex_Index) return Boolean
      is (for some N of Row => N = V);

   begin
      if Graph.Multigraph then raise Multigraph_Error; end if;

      P (1) := (First, Invalid_Edge_ID, Default_Edge_Data);

      All_Initial_Vertices :
      loop
         Explore_Vertex :
         loop
            Path_Extension :
            loop  -- EC2 Path Extension

               Next_Vertex_Found := False;

               Find_Next_Vertex :
               for Edge of G (P (K).Vertex) loop
                  declare
                     Next_Vertex : constant Vertex_Index := Edge.Vertex_B; -- ie G[P[k],j]
                  begin
                     if Next_Vertex > P (1).Vertex and -- (1)
                       (not Contains (P, Next_Vertex)) and -- (2)
                       (not Contains (H (P (K).Vertex), Next_Vertex))
                     then
                        K     := K + 1;
                        P (K) := (Next_Vertex, Edge.ID, Edge.Data);

                        Next_Vertex_Found := True;
                        exit Find_Next_Vertex;
                     end if;
                  end;
               end loop Find_Next_Vertex;

               exit Path_Extension when not Next_Vertex_Found;
            end loop Path_Extension;

            --  EC3 Circuit Confirmation
            for Edge of G (P (K).Vertex) loop
               if Edge.Vertex_B = P (1).Vertex then
                  P (1).Edge    := Edge.Data;
                  Result.Append (P (1 .. K));
                  exit;
               end if;
            end loop;

            --  EC4 Vertex Closure
            exit Explore_Vertex when K = 1;

            H (P (K).Vertex) := (others => Invalid_Vertex);
            for M in H (P (K - 1).Vertex)'Range loop
               if H (P (K - 1).Vertex)(M) = Invalid_Vertex then
                  H (P (K - 1).Vertex)(M) := P (K).Vertex;
                  P (K) := (Invalid_Vertex, Invalid_Edge_ID, Default_Edge_Data);
                  exit;
               end if;
            end loop;
            K := K - 1;
         end loop Explore_Vertex;

         --  EC5 Advance Initial Index
         exit All_Initial_Vertices when P (1).Vertex = Graph.Vertices.Last_Index;

         P (1) := (P (1).Vertex + 1, Invalid_Edge_ID, Default_Edge_Data);
         pragma Assert (K = 1);
         H := (others => (others => Invalid_Vertex));
      end loop All_Initial_Vertices;

      --  EC6 Terminate
      return Result;
   end Find_Cycles;

end SAL.Gen_Graphs;
