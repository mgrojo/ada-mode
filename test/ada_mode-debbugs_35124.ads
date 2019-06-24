-- Expansion of problem reported in debbugs 35214.
--
-- When partial parse is active, the package name after 'end' is not
-- highlighted when the file is first loaded. This is because the
-- partial parse invoked by font-lock does not include the package
-- start, and the error recovery inserts 'begin', leaving a labeled
-- block, in which the label is not highlighted.

-- In this file, the package name has a '.', so it can't be a block
-- label. IMPROVEME: error recovery should insert 'package
-- <identifier> is begin'.

-- No test-face except on the final package name; doing it earlier
-- invokes a full parse which defeats the test.

package Ada_Mode.Debbugs_35124 is

   -- Filler lines, so package start is not visible in the buffer
   -- when package end is, and font-lock actually uses partial parse.

   -- more filler ...


   -- more filler ...


   -- more filler ...

   procedure Proc_1;

   -- more filler ...


   -- more filler ...


   -- more filler ...


   -- more filler ...

   --EMACSCMD:(test-face "Debbugs_35124" font-lock-function-name-face)
end Ada_Mode.Debbugs_35124;
-- Local Variables:
-- wisi-partial-parse-threshold: 0
-- End:
