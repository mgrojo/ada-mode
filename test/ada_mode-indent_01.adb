--  Encountered bug in grammar indent statements
procedure Ada_Mode.Indent_01
is
   Query_Overriding_Parameters : constant Prepared_Statement :=
     Prepare
       (Sql_Select
          (Entities2_Fields,
           Where =>
             --  Output parameter must have the same name as original
             Entities2.Name = Entities.Name
               and Entities.Id = Integer_Param));
begin
   null;
end Ada_Mode.Indent_01;
