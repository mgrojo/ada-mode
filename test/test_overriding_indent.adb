with Ada.Finalization;
procedure Test_Overriding_Indent
is
   type Controlled_Type is new Ada.Finalization.Controlled with null record;

   overriding procedure Initialize (Item : in out Controlled_Type);

   overriding
   procedure Finalize (Item : in out Controlled_Type);

   overriding procedure Initialize (Item : in out Controlled_Type)
   is begin
      null;
   end Initialize;

   overriding
   procedure Finalize (Item : in out Controlled_Type)
   is begin
      null;
   end Finalize;

begin
   null;
end Test_Overriding_Indent;
