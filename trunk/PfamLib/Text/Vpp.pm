############################################################
#
# $Header: /lustre/pfam/pfam/Users/rdf/cvs2svn/pfamweb-public/PfamLib/Text/Vpp.pm,v 1.1.1.1 2006-03-15 09:56:27 rdf Exp $
#
# $Source: /lustre/pfam/pfam/Users/rdf/cvs2svn/pfamweb-public/PfamLib/Text/Vpp.pm,v $
# $Revision: 1.1.1.1 $
# $Locker:  $
# 
############################################################

package Text::Vpp;

use strict;
use vars qw($VERSION);
use FileHandle ;
use Carp ;

$VERSION = '0.2';

# Preloaded methods go here.

=head1 NAME

Text::Vpp - Perl extension for a versatile text pre-processor

=head1 SYNOPSIS

 use Text::Vpp ;

 $fin = Text::Vpp-> new('input_file_name') ;

 $fin->setVar('one_variable_name' => 'value_one', 
			  'another_variable_name' => 'value_two') ;

 $res = $fin -> substitute ; # or directly $fin -> substitute('file_out') 

 die "Vpp error ",$fin->getErrors,"\n" unless $res ;

 $fout = $fin->getText ;

 print "Result is : \n\n",join("\n",@$fout) ,"\n";

=head1 DESCRIPTION

This class enables to preprocess a file a bit like cpp. 

First you create a Vpp object passing the name of the file to process, then
you call setvar() to set the variables you need.

Finally you call substitute on the Vpp object. 

=head1 NON-DESCRIPTION

Note that it's not
designed to replace the well known cpp. Note also that if you think of
using it to pre-process a perl script, you're likely to shoot yourself
in the foot. Perl has a lot of built-in mechanism so that a pre-processor
is not necessary.

=head1 INPUT FILE SYNTAX

=head2 Comments

All lines beginning with '#' are skipped. (May be changed with 
setCommentChar())

=head2 in-line eval

Lines beginning with '@EVAL' (@ being pompously named the 'action char') 
are evaluated as small perl script. 

When setActionChar() is called with '#' as a parameter, Vpp doesn't 
skip lines beginning with '#'. In this case, there's no comment possible.

=head2 Multi-line input

Line ending with \ are concatenated with the following line.

=head2 Variables substitution

You can specify in your text varaibles beginning with $ (like in perl).
These variables can be set either by the setVar() method or by the 
"eval" capability of Vpp (See below).

=head2 Setting variables

Line beginning by @ are "evaled" using variables defined by setVar().
You can use only scalar variables. This way, you can also define variables in 
your text which can be used later.

=head2 Conditional statements

Text::Vpp understands
@IF, @ELSIF, @ENDIF,and so on.
@INCLUDES and @IF can be nested.

@IF and @ELSIF are followed by a string which will be evaled using
the variable you defined (either with setVar() or in the text).

=head2 Inclusion

Text::Vpp understands @INCLUDE

=head1 Constructor

=head2 new(file_name, optional_var_hash_ref, optional_action_char)

Create the file object. The second parameter can be a hash containing all
variables needed for the substitute method.

=cut

sub new
  {
	my $type = shift ;
	
	my $self = {} ;
	my $file = shift ;
	my $ref = shift ;
	my $action =shift ;
	my $comment = shift ;
	
	if (defined $ref && (ref($ref) eq "HASH"))
	  {
		$self->{var} = $ref ;
	  }

	$self->{action}  = defined $action  ? $action  : '@' ;
	$self->{comment} = defined $comment ? $comment : '#' ;
	$self->{'backslash'} = 1 ;

	$self->{fileDesc} = new FileHandle ;

	$self->{name} = $file ;
	$self->{fileDesc}->open($file) || die "can't open $file \n";
	
	bless $self,$type ;
  }


sub myEval 
  {
	my $self = shift ;
	my $expression = shift ;
	
	# transform each $xxx in $self->{var}{$xxx}
	$expression =~ s[\$(\w+)] [\$self->{var}{'$1'}]g ;
	
	my $return = eval($expression) ;
	
	if ($@ ne "") {
	  die "Error in eval : $@ \n",
	  "line : $expression \nfile: $self->{name} line $.\n";
	}
	return ($return);
  }

=head1 Methods

=head2 substitute([output_file])

Perform the substitute, inclusion, and so on and write the result in 
"output_file". 
Returns 1 on completion, 0 in case of an error.

If output_file is not specified this function store the subtitution result
in an internal variable. The result can be retrieved with getText()

=cut

#'

sub substitute
  {
	#return array ref made of new file
	my $self = shift ;

	my $fileOut = shift ;

	$self->{errorText} = [] ;
	$self->{error} = 0;

	$self->{fileDesc}->seek(0,0);
	my $res = $self->processBlock(1,1,-1) ;

	if (defined $fileOut)
	  {
		print "writing $fileOut\n";
		unless( open(SUBSTITUTEOUT,">$fileOut"))
		  {
			$self->snitch("cannot open $fileOut") ;
			return 0 ;
		  }
		print SUBSTITUTEOUT join("\n",@$res) ,"\n" ;
		close(SUBSTITUTEOUT) ;
	  }
	else
	  {
		$self->{result} = $res ;
	  }

	return  (not $self->{error} ) ;
  }

=head2 getText()

Returns an array ref containing the result.

=cut

sub getText
  {
	my $self = shift ;
	return $self->{result} ;
  }

=head2 getErrors()

Returns an array ref containing the errors.

=cut

sub getErrors
  {
	my $self = shift  ;
	return $self->{errorText} ;
  }

sub processBlock 
  {
	# three parameters :
	# GlobExpand : true if the whole ifdef to endif block can expanded
	# Expand : true if the calling ifdef is true
	# Level : the depth in the recusivity
	my ($self,$globExpand,$expand,$level)=@_ ;
	
	my $action = $self->{action} ;
	my $out = [] ;
	
	# Done is used to evaluate the elsif
	my ($done) = $expand ;
	
	$level++ ;
	
	# Stage is used for syntax check
	my ($stage) = ($level == 0) ? 0 : 1 ;
	
	my ($line,$keep) ;
	while (defined($line = $self->{fileDesc}->getline) ) 
	  {
		chomp($line);
		#skip commented lines
		next if (defined $self->{comment} and $line =~ /^\s*\Q$self->{comment}\E/);
		
		# get following line if the line is ended by \
		# (followed by tab or whitespaces)
		if ($self->{backslash} == 1 and $line =~ s/\\\s*$//) 
		  {
			$keep .= $line ;
			next ;
		  }
		
		my $lineIn;
		if (defined $keep)
		  {
			$lineIn = $keep.$line ;
			undef $keep ;
		  } 
		else
		  {
			$lineIn = $line ;
		  }
		
		my $ifPat      = $action."if" ;
		my $elsifPat   = $action."elsif" ;
		my $elsePat    = $action."else" ;
		my $endifPat   = $action."endif" ;
		my $includePat = $action."include" ;
		my $evalPat    = $action."eval" ;
		
		if ($lineIn =~ s/^\Q$ifPat\E\s*//i) 
		  {
			# process the lines after the ifdef, 
			my ($expandLoc) = $self->myEval($lineIn) ;
			push @$out, @{$self->processBlock($expand , $expandLoc ,$level)} ;
		  }
		elsif ($lineIn =~ s/^\Q$elsifPat\E\s*//i) 
		  {
			# process the lines after the ELSIF, done is set if the block
			# is expanded
			unless ($stage == 1 or $stage ==2) 
                          {
                            $self->snitch("unexpected elsif");
                          }
			$stage = 2 ;
			$expand = $self->myEval($lineIn) && !$done ;
			$done = $expand || $done ;
		  }
		elsif ($lineIn =~ /^\Q$elsePat\E/i) 
		  {
			if ($stage == 0 || $stage == 3 ) 
			  {
				$self->snitch("unexpected else");
			  }
			$stage = 3 ;
			$expand = !$done ;
		  } 
		elsif ($lineIn =~ /^\Q$endifPat\E/i) 
		  {
			if ($stage == 0) {$self->snitch("unexpected endif");}
			return $out ;
		  } 
		elsif ($lineIn =~ /^\Q$includePat\E/i)
		  {
			# look like we've got a new file to slurp
			my ($newFileName) = ($lineIn =~ /^\Q$includePat\E\s+(\S+)/i) ;
			my $newFile =  Text::Vpp-> new ($newFileName, $self->{var},
										   $self->{action},$self->{comment}) ;
			if ($newFile->substitute())
			  {
				my $res = $newFile->getText() ;
				push @$out, @$res ;
			  } 
			else
			  {
				# an error occured
				push @{$self->{errorText}}, @{$newFile->getErrors()} ;
				$self->{error} = 1;
				return $out  ;
			  }
			undef $newFile ;
		  }
		elsif ($lineIn =~ /^\Q$evalPat\E/i or $lineIn !~ /^\Q$action\E/)
		  {
			# process the line
			if ($expand && $globExpand) 
			  {
				if ($lineIn =~ s/^\Q$evalPat\E//i) 
				  {
					$self->myEval($lineIn) ;
				  }
				else 
				  {
					# substitute variables 
					$lineIn =~ s[\$\{?(\w+)\b\}?]
					  [ 
					   if (defined($self->{var}{$1})) {$self->{var}{$1} ;}
					   else {'$'.$1 ;}
					  ]ge ;
					push @$out, $lineIn ;
				  }
			  }
		  }
		else
		  {
                      # ignore
			# $self->snitch("Unknown command :$lineIn") ;
		  }
	  }
	
	if ($level > 0 ) 
	  {
		$self->snitch("Finished inside a conditionnal block");
	  }
	return $out ;
  }




# Autoload methods go after __END__ and are processed by the autosplit program.

=head2 setVar( key1=> value1, key2 => value2 ,...) or setVar(hash_ref)

Declare variables for the substitute.
Note that calling this function clobbers previously stored values.

=cut

sub setVar 
  {
	my $self = shift ;

	if (ref($_[0]) eq 'HASH')
	  {
		$self->{var} = shift ;
	  }
	else
	  {
		%{$self->{var}} = @_ ;
	  }
  }

=head2 setActionChar(char)

Enables the user to use another char as action char. (default @)

Example: setActionChar('#') will enable Vpp to understand #include, #ifdef ..

=cut

sub setActionChar
  {
	my $self =shift ;
	
	$self->{action} = shift ;
  }

=head2 setCommentChar(char)

Enables the user to use another char as comment char. (default #)
This value may be set to undef so that no comments are possible.

=cut

sub setCommentChar
  {
	my $self =shift ;
	
	$self->{comment} = shift ;
  }

=head2 ignoreBackslash()

By default, line ending with '\' are glued to the following line (like in
ksh). Once this method is called '\' will be left as is.

=cut

sub ignoreBackslash
  {
	my $self =shift ;
	
	$self->{'backslash'} = 0 ;
  }

sub snitch
  {
	my $self = shift ;
	my $msg = shift ;
	my $emsg = "Error in $self->{name} line ".
		  $self->{fileDesc}->input_line_number. " : $msg\n" ;

	push @{$self->{errorText}}, $emsg ;
	$self->{error} = 1;
	warn ($emsg);
  }

1;
__END__

=head1 AUTHOR

Dominique Dumont    Dominique_Dumont@grenoble.hp.com

Copyright (c) 1996-1998 Dominique Dumont. All rights reserved.  This
program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 VERSION

Version 0.1

=head1 SEE ALSO

perl(1),Text::Template(3).

=cut
