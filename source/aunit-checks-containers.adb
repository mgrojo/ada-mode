--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2016, 2018 - 2019  All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with AUnit.Assertions;
package body AUnit.Checks.Containers is

   procedure Gen_Check_Vector
     (Label          : in String;
      Computed       : in Container_Pkg.Vector;
      Expected       : in Container_Pkg.Vector;
      Strict_Indices : in Boolean := True)
   is
      use all type Ada.Containers.Count_Type;
      J : Index_Type := Expected.First_Index;
   begin
      if Strict_Indices and Computed.Length > 0 then
         --  Can't check indices if length = 0
         Check_Index (Label & ".First", Computed.First_Index, Expected.First_Index);
         Check_Index (Label & ".Last", Computed.Last_Index, Expected.Last_Index);
      else
         Check (Label & ".Length", Computed.Length, Expected.Length);
      end if;

      for I in Computed.First_Index .. Computed.Last_Index loop
         Check_Element (Label & "." & Index_Type'Image (I), Computed (I), Expected (J));
         if J /= Index_Type'Last then
            J := Index_Type'Succ (J);
         end if;
      end loop;
   end Gen_Check_Vector;

   procedure Gen_Check_Indefinite_Vector
     (Label          : in String;
      Computed       : in Container_Pkg.Vector;
      Expected       : in Container_Pkg.Vector;
      Strict_Indices : in Boolean := True)
   is
      J : Index_Type := Expected.First_Index;
   begin
      if Strict_Indices then
         Check_Index (Label & ".First", Computed.First_Index, Expected.First_Index);
         Check_Index (Label & ".Last", Computed.Last_Index, Expected.Last_Index);
      else
         Check (Label & ".Length", Computed.Length, Expected.Length);
      end if;

      for I in Computed.First_Index .. Computed.Last_Index loop
         Check_Element (Label & "." & Index_Type'Image (I), Computed (I), Expected (J));
         if J /= Index_Type'Last then
            J := Index_Type'Succ (J);
         end if;
      end loop;
   end Gen_Check_Indefinite_Vector;

   procedure Gen_Check_Doubly_Linked_List
     (Label    : in String;
      Computed : in Container_Pkg.List;
      Expected : in Container_Pkg.List)
   is
      use Container_Pkg;
      I : Cursor  := Computed.First;
      J : Cursor  := Expected.First;
      K : Integer := 1;
   begin
      loop
         exit when I = No_Element or J = No_Element;
         Check_Element (Label & Integer'Image (K), Element (I), Element (J));
         Next (I);
         Next (J);
         K := K + 1;
      end loop;

      Standard.AUnit.Assertions.Assert (I = No_Element and J = No_Element, Label & " different lengths");
   end Gen_Check_Doubly_Linked_List;


end AUnit.Checks.Containers;
