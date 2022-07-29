--  Found a bug in the navigate grammar actions for exception_handler.

procedure Ada_Mode.Navigate_Exception
is
begin

   Finish_Parse;

exception
   when Partial_Parse =>

      begin
         Tree.Soi := Soi;
      end;
      --EMACSCMD:(progn (end-of-line 0)(backward-sexp 2)(looking-at "begin"))
      --EMACSRESULT: t

      Finish_Parse;

      Trace.Put_Clock ("finish partial parse");

   when Syntax_Error =>
      raise;

end Ada_Mode.Navigate_Exception;
