--  Abstract :
--
--  Language-specific runtime for wisi_grammar_mode_parse.
--
--  Actually empty, but wisitoken requires this file.
--
--  Copyright (C) 2017, 2018 Stephen Leake All Rights Reserved.
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

package WisiToken.Wisi_Runtime.Wisi_Grammar_1 is

   type Parse_Data_Type is new Wisi_Runtime.Parse_Data_Type with null record;

end WisiToken.Wisi_Runtime.Wisi_Grammar_1;
