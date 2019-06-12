--  Abstract :
--
--  see spec.
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

package body SAL.Gen_Graphs.Gen_AUnit is

   procedure Check
     (Label    : in String;
      Computed : in Edge_Item;
      Expected : in Edge_Item)
   is begin
      Check (Label & ".ID", Computed.ID, Expected.ID);
      Check_Edge_Data (Label & ".Data", Computed.Data, Expected.Data);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Path_Item;
      Expected : in Path_Item)
   is begin
      Check (Label & ".Vertex", Computed.Vertex, Expected.Vertex);
      Check (Label & ".Edges", Computed.Edges, Expected.Edges);
   end Check;

   procedure Check
     (Label    : in String;
      Computed : in Path;
      Expected : in Path)
   is begin
      Check_Path (Label, Computed, Expected, Strict_Indices => True);
   end Check;

   function "&" (Left : in Edge_Lists.List; Right : in Edge_Item) return Edge_Lists.List
   is
      use Edge_Lists;
   begin
      return Result : List := Left do
         Append (Result, Right);
      end return;
   end "&";

   function "+" (Right : in Vertex_Index) return Vertex_Lists.List
   is
      use Vertex_Lists;
   begin
      return Result : List do
         Append (Result, Right);
      end return;
   end "+";

   function "&" (Left : in Vertex_Lists.List; Right : in Vertex_Index) return Vertex_Lists.List
   is
      use Vertex_Lists;
   begin
      return Result : List := Left do
         Append (Result, Right);
      end return;
   end "&";
end SAL.Gen_Graphs.Gen_AUnit;
