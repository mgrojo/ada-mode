--  Test refactor 3

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Refactor_Object_Index_To_Element_Object is

   --EMACSCMD:(test-oieo "Config_Op" "Err.Recover.Op (I)")
   Op : Config_Op := Err.Recover.Op (I);
   --EMACSCMD:(test-refactor-inverse)

   --EMACSCMD:(test-oieo "Boolean" "Item.Config.Ops (")
   A : Boolean := Item.Config.Ops (Item.Config.Ops.Last_Index).Op = Fast_Forward;
   --EMACSCMD:(test-refactor-inverse)

end Ada_Mode.Refactor_Object_Index_To_Element_Object;
