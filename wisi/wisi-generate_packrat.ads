--  Abstract :
--
--  Generate Ada code for a Packrat parser.
--
--  References:
--
--  See wisitoken-parse-packrat.ads.
--
--  Design:
--
--  We only support two parser types; sequence and "or"; those are the
--  only types supported by the .wy grammar file syntax. This allows
--  producing both an LALR and packrat parser from the same grammar
--  file.
--
--  Copyright (C) 2018 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

with WisiToken.Productions;
procedure Wisi.Generate_Packrat
  (Grammar      : in WisiToken.Productions.Prod_Arrays.Vector;
   Action_Names : in Names_Array_Array;
   Check_Names  : in Names_Array_Array;
   Descriptor   : in WisiToken.Descriptor);
--  Generate the nonterminal parse procedures. Package level done in
--  wisi-output_ada.adb and wisi-gen_output_ada_common.adb.
