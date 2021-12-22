--  Abstract :
--
--  See spec.
--
--  Copyright (C) 2021 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Traceback.Symbolic;
with System.Address_Image;
with System.Storage_Elements;
package body WisiToken.Memory_Statistics is

   function Dump_Memory_Statistics
     (Size   : Positive;
      Report : Report_Type := GNATCOLL.Memory.Memory_Usage)
      return String
   is
      Buffer : Unbounded_String := To_Unbounded_String
        ("Memory use: " & Ada.Characters.Latin_1.LF);

      procedure Trace_Put (S : String)
      is begin
         Append (Buffer, S);
      end Trace_Put;

      procedure Trace_Put_Line (S : String) is
      begin
         Append (Buffer, S & Ada.Characters.Latin_1.LF);
      end Trace_Put_Line;

      procedure Internal is new GNATCOLL.Memory.Redirectable_Dump
        (Put_Line => Trace_Put_Line,
         Put      => Trace_Put);

   begin
      Internal (Size, Report);
      return To_String (Buffer);
   end Dump_Memory_Statistics;

end WisiToken.Memory_Statistics;
