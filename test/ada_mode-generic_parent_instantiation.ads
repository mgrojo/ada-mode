-- test pragma after library unit
-- This file does not compile; parents are not pure.
with Ada_Mode.Generic_Parent;
private package Ada_Mode.Generic_Parent_Instantiation is new
  Ada_Mode.Generic_Parent;
pragma Pure (Ada_Mode.Generic_Parent_Instantiation);
