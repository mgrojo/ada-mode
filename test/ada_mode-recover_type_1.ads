--  From a real editing session.
--
-- Used to be very slow, require cost 13 to find a solution. Now finds
-- an excellent solution quickly.

package Ada_Mode.Recover_Type_1 is
   type Derivs (Label : Derivs_Label) is record
      case Label is
         when Dvdecimal=>
            --  missing 'end case; end record;' recover inserts that at end of previous line.

            procedure Parse (Input : in String);
end Ada_Mode.Recover_Type_1;
