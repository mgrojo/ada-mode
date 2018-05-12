-- From a real editing session. Used to cause overflow appending
-- config.ops to delete all the string literals except the first; now
-- keeps the last.

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
procedure Ada_Mode.Recover_String_Quote_3
is
Response : Unbounded_String := +"<!DOCTYPE html>" & ASCII.LF &
        "<html lang=""en"">" &
        "<meta http-equiv=""Content-Type"" content=""text/html; charset=utf-8"">" & ASCII.LF &
        "<head>" & ASCII.LF &
        "<script src=""/" & (-Server_Data) & "/songs.js""></script>" & ASCII.LF &
        "<script src=""/" & (-Server_Data) & "/debug_head.js""></script>" & ASCII.LF &
        "<link type=""text/css"" rel=""stylesheet"" href=""/" & (-Server_Data) & "/songs.css""/>" &
        "</head><body>" & ASCII.LF &
        "<form action=""/search"" method=get>" &
        "<label>Title</label>" &
        "<input type=search autofocus name=""search"">" &
       <div><input type=submit value=""Search""></div>";
begin
end Ada_Mode.Recover_String_Quote_3;
