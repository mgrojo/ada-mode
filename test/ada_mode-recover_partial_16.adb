--  encountered "error during resume". FIXME: still does

--EMACS_SKIP_UNLESS:(and nil (eq ada-parser 'process))

begin
accept Start;

      loop
         Explore.Process_One (Super, Shared, Status);

         if Status = All_Done then
         exit;
            end if;
         end select;
