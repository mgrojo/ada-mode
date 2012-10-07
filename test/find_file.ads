--  Set the cursor on P_Bug
--  Call \M-x ff-find-other-file
--  We expect to be on P_Bug in find_file.adb, but the cursor is put on P_Bug_One
--  fixed in: ada-mode 3.1

--EMACSCMD: (setq ada-indent 2)
package Find_File is

  procedure P_Bug_One;

  --EMACSCMD: (progn (forward-line 1) (ff-find-other-file) (backward-word 1) (looking-at "P_Bug is"))
  procedure P_Bug;

  -- ein kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein kommentr ein
  -- kommentr ein kommentr ein kommentr ein kommentr ein Kommentr
end Find_File;
