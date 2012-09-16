--  The following code is not indented correctly
--  (Works if we remove the first declare' statement)

procedure Other_1 is
begin
   if Horizontal then
      declare
         B : Gtk_HButton_Box;
      begin
         Gtk_New (B);
         BBox := Gtk_Button_Box (B);
      end;
   else
      declare
         B : Gtk_VButton_Box;  --  This line is incorrect
      begin
         Gtk_New (B);
         BBox := Gtk_Button_Box (B);
      end;
   end if;
end Other_1;

