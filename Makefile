run : whiki.pl
	gprolog --init-goal "consult('$<')"
debug : whiki.pl
	gprolog --init-goal "spy(('?')/1),trace,consult('$<')"
