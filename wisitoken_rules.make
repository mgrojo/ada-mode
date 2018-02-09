# rules for using wisitoken grammars in projects

%_re2c.c : %.re2c
	$(RE2C_HOME)/bin/re2c --no-generation-date --debug-output --input custom -W -Werror --utf-8 -o $@ $<

%.ads %.re2c : %.wy $(WISITOKEN)/wisi-generate.exe
	$(WISITOKEN)/wisi-generate.exe -v 1 --output_language Ada --lexer re2c $(<F) $(WISI_GENERATE_ARGS) > $(*F).parse_table
	dos2unix $(*F).ads $(*F).adb $(*F).re2c

$(WISITOKEN)/wisi-generate.exe : force
	$(MAKE) -C $(WISITOKEN) wisi-generate.exe

# end of file
