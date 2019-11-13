-- Encountered "error during resume".
begin
   accept Start;

   loop
      Explore.Process_One (Super, Shared, Status);

      if Status = All_Done then
         exit;
      end if;
   end select;
