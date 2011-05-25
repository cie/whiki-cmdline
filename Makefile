run : whiki.pl
	gprolog --init-goal "consult('$<')"
debug : whiki.pl
	gprolog --init-goal "leash([fail]),spy('?'/1),debug,consult('$<')"
