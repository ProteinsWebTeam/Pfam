##################################################################
# Package to transform a query in a regular expression
##################################################################
package Bio::Pfam::Web::PfamRegExp;

sub to_regexp
{
	my ($regexp, $general_regexp, $index, @argument);
	my $string = shift;
	$string =~ s/\(/ \( /g;
	$string =~ s/\)/ \) /g;
	$string =~ s/\^/ \^ /g; 
	$string =~ s/\$/ \$ /g;
	$string =~ s/^\s+//g;
	$string =~ s/\s+/ /g;
	$string = lc($string);
	@argument = split /\s/, $string;
	$index = 0;
	
	foreach (@argument)
	{
		CASE: 
		{
			if ($_ eq 'and')   { $regexp .= " && "; last CASE }
			if ($_ eq 'or' )   { $regexp .= " || "; last CASE }
			if ($_ eq 'not')   { $regexp .= " ! " ; last CASE }
			if ($_ eq '^'  )   { $regexp .= "^"   ; last CASE }
			if ($_ eq '('  )   { $regexp .= " ( "; $index++; last CASE }
			if ($_ eq ')'  )   { $regexp .= " ) "; $index--; last CASE }
			
			$_ = "\[^:\\s\]\+" if $_ eq '_';
			if ($regexp =~ /(&&|\|\||!|\()\s$/ || $regexp eq '')
			{
				$regexp .= "/\[\\s\(;,\]$_\[\\s:;\]/i";
			}
			elsif ($regexp =~ /\^$/)
			{
				chop $regexp;
				$regexp .= "/^\\($_\[\\s:;\]/i";
			}
			else
			{
				$regexp =~ s/\[\\s:;\]\/i$//;
				if ($_ eq '$')
					{ $regexp .= ":\/i" }
				else
					{ $regexp .= "\\s$_\[\\s:;\]/i" } 
			}
		}
	}
	die "Wrong number of brackets\n" if ( $index != 0 );
	return $regexp;
}

1;
