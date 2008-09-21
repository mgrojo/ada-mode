all:
	@mkdir -p lib obj
	@gnatmake -Popentoken_lib

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib
