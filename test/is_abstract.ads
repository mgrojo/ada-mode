--  Reported by Stephen Leake.
--  The comment after "is abstract" is incorrectly indented with adamode 3.3

--EMACSCMD: (setq ada-indent-align-comments nil)
package Is_Abstract is

   function Get_Message_Error
     (Device : in Integer;     --  ada-broken-indent=2
      Slot   : in Integer)
     return String
      is abstract;    --  uncorrectly indented
   --  This comment is also uncorrectly indented

   procedure Bar; --  This line too has incorrect indentation

end Is_Abstract;
