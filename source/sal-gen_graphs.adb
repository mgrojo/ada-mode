--  Abstract :
--
--  See spec.
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

   function Find_Path (Graph : in out Gen_Graphs.Graph; From : in Vertex_Index; To : in Find_Target) return Path
   is
      Vertex_Queue  : Vertex_Queues.Queue_Type
        (Size => Integer (Vertex_Index'Pos (Vertex_Index'Last) - Vertex_Index'Pos (Vertex_Index'First) + 1));
      Result_Length : Integer := 0;
      Result_Tail   : Vertex_Index;
      Result_Edge   : Edge_Lists.Cursor;
   begin
      --  [1] figure 22.3 breadth-first search; 'From' = s, terminate
      --  when encounter To.

      --  FIXME: never checks Parent_Set?

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

      while not Vertex_Queue.Is_Empty and Result_Length = 0 loop
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

                     case To.Label is
                     when Edge =>
                        Result_Edge := Find (To.Data, V.Edges);
                        if Result_Edge /= Edge_Lists.No_Element then
                           Result_Length := V.D + 1;
                           Result_Tail   := V_Index;
                           exit Edges;
                        end if;

                     when Vertex =>
                        if V_Index = To.Vertex then
                           Result_Length := V.D + 1;
                           Result_Tail   := V_Index;
                           exit Edges;
                        end if;
                     end case;

                     Vertex_Queue.Put (V_Index);
                  end if;
               end;
            end loop Edges;
            U.Color := Black;
         end;
      end loop;

      return Result : Path (1 .. Result_Length)
      do
         declare
            use Edge_Lists;
            V_Index   : Vertex_Index := Result_Tail;
            Last_Edge : Cursor       := Result_Edge;
         begin
            for I in reverse 1 .. Result_Length loop
               declare
                  V : Vertex_Node renames Graph.Vertices (V_Index);
               begin
                  Result (I) :=
                    (V_Index,
                     (if Last_Edge = No_Element
                      then Default_Edge_Data
                      else Element (Last_Edge).Data));
                  Last_Edge := V.Parent_Edge;
                  V_Index   := V.Parent;
               end;
            end loop;
         end;
      end return;
   end Find_Path;

end SAL.Gen_Graphs;
