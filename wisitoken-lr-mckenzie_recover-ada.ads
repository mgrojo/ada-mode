--  Abstract :
--
--  Ada language specific algorithms for McKenzie_Recover
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

package WisiToken.LR.McKenzie_Recover.Ada is

   function Language_Fixes
     (Trace             : in out WisiToken.Trace'Class;
      Lexer             : in     WisiToken.Lexer.Handle;
      Parser_Label      : in     Natural;
      Terminals         : in     Base_Token_Arrays.Vector;
      Tree              : in     Syntax_Trees.Tree;
      Local_Config_Heap : in out Config_Heaps.Heap_Type;
      Config            : in     Configuration)
     return Non_Success_Status;

   function Constrain_Terminals
     (Trace        : in out WisiToken.Trace'Class;
      Parser_Label : in     Natural;
      Table        : in     Parse_Table;
      Config       : in     Configuration)
     return WisiToken.Token_ID_Set;

end WisiToken.LR.McKenzie_Recover.Ada;
