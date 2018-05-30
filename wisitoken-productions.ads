--  Abstract :
--
--  Type and operations for building grammar
--  productions.
--
--  Copyright (C) 2018 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with Ada.Containers.Vectors;
with SAL.Gen_Definite_Doubly_Linked_Lists;
with WisiToken.Semantic_Checks;
with WisiToken.Syntax_Trees;
package WisiToken.Productions is

   package Token_ID_Lists is new SAL.Gen_Definite_Doubly_Linked_Lists (Token_ID);

   type Right_Hand_Side is record
      Tokens     : Token_ID_Lists.List;
      Action     : WisiToken.Syntax_Trees.Semantic_Action;
      Check      : WisiToken.Semantic_Checks.Semantic_Check;
      Name_Index : Integer;
      --  Index of production among productions for a single nonterminal (the LHS)
   end record;

   type Instance is record
      LHS : Token_ID;
      RHS : aliased Right_Hand_Side;
   end record;

   package Arrays is new Ada.Containers.Vectors (Production_ID, Instance);

   type Production_ID_Range is record
      First : Production_ID;
      Last  : Production_ID;
   end record;

   function Find (Grammar : in Arrays.Vector; Nonterm : in Token_ID) return Production_ID_Range;

end WisiToken.Productions;
