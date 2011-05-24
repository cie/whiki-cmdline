run : whiki.pl
	gprolog --init-goal "consult('$<')"
debug : whiki.pl
	gprolog --init-goal "trace,consult('$<')"
