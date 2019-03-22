--  Recover failed on enqueue limit. FIXME: still does
--
--  Converting an 'if' statement to a 'case' statement

--EMACS_SKIP_UNLESS:(and nil (eq ada-parser 'process))

case N.Label  Shared_Terminal then
         Result := Result & (+Token_Index'Image (N.Terminal)) & ":";
