--  Abstract :
--
--  Checks for Ada.Containers
--
--  Copyright (C) 2016, 2019  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
package AUnit.Checks.Containers is

   procedure Check is new Gen_Check_Discrete (Ada.Containers.Count_Type);

   generic
      type Index_Type is range <>;
      type Element_Type is private;
      with package Container_Pkg is new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Element
        (Label    : in String;
         Computed : in Element_Type;
         Expected : in Element_Type);
   procedure Gen_Check_Vector
     (Label          : in String;
      Computed       : in Container_Pkg.Vector;
      Expected       : in Container_Pkg.Vector;
      Strict_Indices : in Boolean := True);

   generic
      type Element_Type (<>) is private;
      type Index_Type is range <>;
      with package Container_Pkg is new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);
      with procedure Check_Index
        (Label    : in String;
         Computed : in Index_Type;
         Expected : in Index_Type);
      with procedure Check_Element
        (Label    : in String;
         Computed : in Element_Type;
         Expected : in Element_Type);
   procedure Gen_Check_Indefinite_Vector
     (Label          : in String;
      Computed       : in Container_Pkg.Vector;
      Expected       : in Container_Pkg.Vector;
      Strict_Indices : in Boolean := True);

   generic
      type Element_Type is private;
      with package Container_Pkg is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Element_Type);
      with procedure Check_Element
        (Label    : in String;
         Computed : in Element_Type;
         Expected : in Element_Type);
   procedure Gen_Check_Doubly_Linked_List
     (Label    : in String;
      Computed : in Container_Pkg.List;
      Expected : in Container_Pkg.List);

end AUnit.Checks.Containers;
