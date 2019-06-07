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

with SAL.Gen_Bounded_Definite_Queues;
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

   function Build_Path
     (Graph       : in Gen_Graphs.Graph;
      Tail_Vertex : in Vertex_Index;
      Tail_Edge   : in Edge_Lists.Cursor)
     return Path
   is
   begin
      return Result : Path (1 .. Graph.Vertices (Tail_Vertex).D + 1)
      do
         declare
            use Edge_Lists;
            V_Index   : Vertex_Index := Tail_Vertex;
            Last_Edge : Cursor       := Tail_Edge;
         begin
            for I in reverse 1 .. Result'Length loop
               declare
                  V : Vertex_Node renames Graph.Vertices (V_Index);
               begin
                  Result (I) :=
                    (V_Index,
                     (if Last_Edge = No_Element
                      then Default_Edge_Data
                      else Element (Last_Edge).Data));

                  if V.Parent_Set then
                     Last_Edge := V.Parent_Edge;
                     V_Index   := V.Parent;
                  end if;
               end;
            end loop;
         end;
      end return;

   end Build_Path;

   ----------
   --  Visible subprograms

   procedure Add_Edge
     (Graph    : in out Gen_Graphs.Graph;
      Vertex_A : in     Vertex_Index;
      Vertex_B : in     Vertex_Index;
      Data     : in     Edge_Data)
   is begin
      Graph.Vertices (Vertex_A).Edges.Append ((Vertex_B, Data));
   end Add_Edge;

   function Find_Paths
     (Graph : in out Gen_Graphs.Graph;
      From  : in     Vertex_Index;
      To    : in     Edge_Data)
     return Path_Lists.List
   is
      Vertex_Queue  : Vertex_Queues.Queue_Type
        (Size => Integer (Vertex_Index'Pos (Vertex_Index'Last) - Vertex_Index'Pos (Vertex_Index'First) + 1));

      Result_List : Path_Lists.List;
      Result_Edge : Edge_Lists.Cursor;
   begin
      --  [1] figure 22.3 breadth-first search; 'From' = s.

      for I in Graph.Vertices'Range loop
         if I = From then
            Graph.Vertices (I).Color      := Gray;
            Graph.Vertices (I).D          := 0;
            Graph.Vertices (I).Parent_Set := False;

         else
            Graph.Vertices (I).Color      := White;
            Graph.Vertices (I).D          := Natural'Last;
            Graph.Vertices (I).Parent_Set := False;
         end if;
      end loop;

      Vertex_Queue.Put (From);

      while not Vertex_Queue.Is_Empty loop
         declare
            U_Index : constant Vertex_Index := Vertex_Queue.Get;
            U       : Vertex_Node renames Graph.Vertices (U_Index);
         begin
            Edges :
            for C in Graph.Vertices (U_Index).Edges.Iterate loop
               declare
                  use all type Edge_Lists.Cursor;
                  V_Index : constant Vertex_Index := Edge_Lists.Element (C).Vertex_B;
                  V       : Vertex_Node renames Graph.Vertices (V_Index);
               begin
                  if V.Color = White then
                     V.Color       := Gray;
                     V.D           := U.D + 1;
                     V.Parent      := U_Index;
                     V.Parent_Edge := C;
                     V.Parent_Set  := True;

                     Result_Edge := Find (To, V.Edges);
                     if Result_Edge /= Edge_Lists.No_Element then
                        Result_List.Append (Build_Path (Graph, V_Index, Result_Edge));
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

   function Find_Cycles (Graph : in out Gen_Graphs.Graph) return Path_Lists.List
   is
      --  Implements [2] "Algorithm EC"
      --
      --  vertex "0" = Invalid_Vertex
      --  vertex  N  = Vertex_Index'Last

      G : Gen_Graphs.Graph renames Graph;
      P : Path (1 .. Vertex_Index'Pos (Vertex_Index'Last) - Vertex_Index'Pos (Vertex_Index'First) + 1);
      K : Positive := 1; -- ie P_Last

      type H_Row is array (Vertex_Index) of Vertex_Index'Base;
      H : array (Vertex_Index) of H_Row := (others => (others => Invalid_Vertex));

      Next_Vertex_Found : Boolean;
      Circuit_Found     : Boolean;

      Result : Path_Lists.List;

      function Contains (P : in Path; V : in Vertex_Index) return Boolean
      is (for some N of P => N.Vertex = V);

      function Contains (Row : in H_Row; V : in Vertex_Index) return Boolean
      is (for some N of Row => N = V);

   begin
      P (1) := (Vertex_Index'First, Default_Edge_Data);

      All_Initial_Vertices :
      loop
         Explore_Vertex :
         loop
            Path_Extension :
            loop  -- EC2 Path Extension

               Next_Vertex_Found := False;
               Circuit_Found     := False;

               Find_Next_Vertex :
               for Edge of G.Vertices (P (K).Vertex).Edges loop
                  declare
                     Next_Vertex : constant Vertex_Index := Edge.Vertex_B; -- ie G[P[k],j]
                  begin
                     Circuit_Found := Circuit_Found or Next_Vertex = P (1).Vertex;

                     if Next_Vertex > P (1).Vertex and -- (1)
                       (not Contains (P, Next_Vertex)) and -- (2)
                       (not Contains (H (P (K).Vertex), Next_Vertex))
                     then
                        K     := K + 1;
                        P (K) := (Next_Vertex, Edge.Data);

                        Next_Vertex_Found := True;
                        exit Find_Next_Vertex;
                     end if;
                  end;
               end loop Find_Next_Vertex;

               exit Path_Extension when not Next_Vertex_Found;
            end loop Path_Extension;

            --  EC3 Circuit Confirmation
            if Circuit_Found then
               Result.Append (P (1 .. K));
            end if;

            --  EC4 Vertex Closure
            exit Explore_Vertex when K = 1;

            H (P (K).Vertex) := (others => Invalid_Vertex);
            for M in Vertex_Index loop
               if H (P (K - 1).Vertex)(M) = Invalid_Vertex then
                  H (P (K - 1).Vertex)(M) := P (K).Vertex;
                  P (K) := (Invalid_Vertex, Default_Edge_Data);
                  exit;
               end if;
            end loop;
            K := K - 1;
         end loop Explore_Vertex;

         --  EC5 Advance Initial Index
         exit All_Initial_Vertices when P (1).Vertex = Vertex_Index'Last;

         P (1) := (Vertex_Index'Succ (P (1).Vertex), Default_Edge_Data);
         pragma Assert (K = 1);
         H := (others => (others => Invalid_Vertex));
      end loop All_Initial_Vertices;

      --  EC6 Terminate
      return Result;
   end Find_Cycles;

end SAL.Gen_Graphs;
