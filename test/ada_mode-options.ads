-- These commands are executed before the buffer is indented, so they
-- affect the whole file.
--
--EMACSCMD: (setq ada-indent-record-rel-type 0)
package Ada_Mode.Options is
   pragma Elaborate_Body (Options);

   type Private_Type_1 is tagged
   record
      Component_1 : Integer;
   end record;

   type Derived_Type_1 is
   new Private_Type_1 with record
      Component_2 : Integer;
   end record;

end Ada_Mode.Options;
