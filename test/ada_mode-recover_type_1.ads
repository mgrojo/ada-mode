--  From a real editing session. Used to be very slow, require cost 13 to find a solution.
--EMACS_SKIP_UNLESS:(eq ada-parser 'process)

package Ada_Mode.Recover_Type_1 is
   type Derivs (Label : Derivs_Label) is record
      case Label is
      when dvDecimal=>

         procedure Parse (Input : in String)
         is begin

         end Parse;
      end Ada_Mode.Recover_Type_1;
