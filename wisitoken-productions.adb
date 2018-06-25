--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2018 Stephe Leake
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. This library is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHAN- TABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are granted
--  additional permissions described in the GCC Runtime Library Exception,
--  version 3.1, as published by the Free Software Foundation.

pragma License (Modified_GPL);

package body WisiToken.Productions is

   function Image
     (LHS        : in Token_ID;
      RHS_Index  : in Natural;
      RHS        : in Token_ID_Arrays.Vector;
      Descriptor : in WisiToken.Descriptor'Class)
     return String
   is begin
      return Trimmed_Image ((LHS, RHS_Index)) & ": " & Image (LHS, Descriptor) & " <= " &
        Image (RHS, Descriptor);
   end Image;

end WisiToken.Productions;
