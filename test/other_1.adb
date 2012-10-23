--  The following code is not indented correctly
--  (Works if we remove the first declare' statement)

procedure Other_1 is
   procedure Gtk_New (Item : in Integer) is null;
   function Gtk_Button_Box (Item : in Integer) return Integer is begin return 10; end;
   subtype Gtk_HButton_Box is Integer range 1 .. 10;
   subtype Gtk_VButton_Box is Integer range 10 .. 20;

   BBox : Integer;
begin
   if True then
      declare
         B : Gtk_HButton_Box := 10;
      begin
         Gtk_New (B);
         BBox := Gtk_Button_Box (B);
      end;
   else
      declare
         B : Gtk_VButton_Box := 10;  --  This line is incorrect
      begin
         Gtk_New (B);
         BBox := Gtk_Button_Box (B);
      end;
   end if;
end Other_1;
