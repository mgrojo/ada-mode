package body Ada_Mode.Recover_String_Quote_2 is
   procedure Handle_Search
   is

   begin
      if URI_Param.Is_Empty then
         return "text/html";

      else

         Response := Response & "</table></body></html>";
      end if;
   end Handle_Search;
end Ada_Mode.Recover_String_Quote_2;
