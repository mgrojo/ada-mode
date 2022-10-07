--  From a real editing session. Found a bug in Language_Fixes.
--
--EMACSCMD:(setq skip-recase-test t)
package body Ada_Mode.Recover_Renaming_1 is

   procedure Insert
     (DB              : in Database;
      Play_After      : in Integer     := Null_ID)
   is
   begin

      Checked_Execute (DB, -Statement, Params (1 .. Last));
   end Insert_Update;


   Play_After_Field      : constant GNATCOLL.SQL.Exec.Field_Index := Play_Before_Field + 1;

   Field_Fields : constant array (Fields) of GNATCOLL.SQL.Exec.Field_Index :=
     (Artist => Artist_Field,
      Album  => Album_Field,
      Title  => Title_Field);

end Ada_Mode.Recover_Renaming_1;
--  Local Variables:
--  wisi-disable-face: t
--  End:
