-- From a real editing session. Used to cause overflow appending
-- config.ops to delete all the string literals except the first; now
-- keeps the last.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
procedure Ada_Mode.Recover_String_Quote_3
is
   Response : Unbounded_String := +"<!DOCTYPE html>" & Ascii.Lf &
     "<html lang=""en"">" &
     "<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"">" & Ascii.Lf &
     "<head>" & Ascii.Lf &
     "<script src=""/" & (-Server_Data) & "/songs.js""></script>" & Ascii.Lf &
     "<script src=""/" & (-Server_Data) & "/debug_head.js""></script>" & Ascii.Lf &
     "<link type=""text/css"" rel=""stylesheet"" href=""/" & (-Server_Data) & "/songs.css""/>" &
     "</head><body>" & Ascii.Lf &
     "<form action=""/search"" method=get>" &
     "<label>Title</label>" &
     "<input type=search autofocus name=""search"">" &
<Div><Input type=Submit Value=""Search""></Div>";
begin
end Ada_Mode.Recover_String_Quote_3;
