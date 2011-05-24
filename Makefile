run : whiki.pl
	gprolog --init-goal "consult('$<')"
