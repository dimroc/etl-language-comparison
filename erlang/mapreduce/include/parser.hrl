-record(parser, {
	mode    = undefined :: undefined | binary | regex,
	newline = undefined :: undefined | binary:cp(),
	pattern = undefined :: undefined | binary:cp() | re:mp(),
	tab     = binary:compile_pattern(<< $\t >>) :: binary:cp()
}).