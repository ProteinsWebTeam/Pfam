package Tree;

###########################################################
# This package contains methods to deal with phylogenetic
# trees. The different methods allow to generate, print, 
# save as a string (in a pseudo new hampshire format), 
# retrive from a pseudo new hampshire string, and find
# unicity of a node.
#
# Writen by Lorenzo Cerutti (lmc@sanger.ac.uk) v1.0 11.09.2000
###########################################################

use strict;

# OBSOLETE METHODS! Eliminate from your scripts!!
sub clear_root {
	my $self = shift;
	return $self;
}

###########################################################
# Public methods
###########################################################
sub new {
	my ($name, $ptr_branches, $frequency) = @_;
	my $self = bless {
		name		=> $name,
		branches	=> $ptr_branches,
		frequency	=> $frequency };
	return $self;
}

sub grow_tree {
	my ($tree, $list, $delimiter) = @_;
	foreach my $taxonomy (@$list) {
		my @branch = split /$delimiter/, $taxonomy;
    $tree->add_branch(@branch);
	}
}

sub to_text {
	my ($tree, $depth, $tag) = @_;
	my $output;
	$depth += 2;
	$tree->convert_to_text($depth, $tag, \$output);
	return $output;
}

sub to_string {
	my ($tree) = @_;
	my $string;
	$tree->convert_to_pnh(\$string);
	return $string.";";
}

sub from_string {
	my ($unused,$string) = @_;
	my $tree = new();
	$tree->convert_from_pnh(\$string);
	return $tree;
}

sub get_ids_from_pnh {
	my ($unused, $pnhString, $nodeName) = @_;
	if ($pnhString =~ s/^.*$nodeName:\d+//i || $nodeName eq 'all') {
		my $tree = new();
		$tree->convert_from_pnh(\$pnhString);
		return $tree->get_leafs();
	}
	else {
		return ();
	}
}

sub get_ids {
	my ($tree, $node_name, $ptrList) = @_;
	foreach my $branch ($tree->get_branches()) {
		if ($branch->get_name() eq $node_name) {
			@{$ptrList} = $branch->get_leafs();
			last;
		}
		else {
			$branch->get_ids($node_name, $ptrList) if ($branch->has_childrens());
		}
	}
}
	
sub is_unique_organism {
	my ($tree, $organism) = @_;
  if ($tree->children_is_unique()) {
  	foreach my $node ($tree->get_branches()) {
			if (!$node->has_childrens() || $node->get_name() =~ /$organism/i) {
      	return 1;
    	}
    	else {
      		return $node->is_unique_organism($organism);
      }
    }
  } 
  else {
  	return undef;   
  }
}

###########################################################
# Private methods
###########################################################
sub add_branch {
	my ($tree, @branch) = @_;
	my $node = shift @branch;
	if ($tree->node_exist($node)) {
		($tree->get_node($node))->increment_frequency();
		($tree->get_node($node))->add_branch(@branch);
	}
	else {
		$tree->add_full_branch($node,@branch);
	}
}

sub add_full_branch {
	my ($tree, @branch) = @_;
	my $node = shift @branch;
	if ($node) {
		$tree->create_node($node);
		($tree->get_node($node))->add_full_branch(@branch);
	}
}

sub convert_to_text {
	my ($tree, $depth, $tag, $ptrOutput, $indent, $flag1, $flag2) = @_;
	my $isNotRoot = ($$ptrOutput eq '')? undef:1;
	$depth--;
	$indent .= (!$flag1 && $flag2)? "|   ":"    ";
	$flag1 = $tree->children_is_unique();
	my $lastNode = $tree->is_last_node();
	if (!$lastNode && $depth > 0) {
		foreach my $node ($tree->get_branches()) {
			if ($node->has_childrens()) {
				$flag2 = !$tree->is_last_children($node);
				$$ptrOutput .= $indent."|\n";
				$$ptrOutput .= $indent."+---";
				if ($node->children_is_unique() && !$node->is_last_node() && $isNotRoot) {
					$$ptrOutput .= $node->get_name()."(".$node->get_frequency().")"."\n";
				}
				else {
					$$ptrOutput .= "#".$node->get_name()."(".$node->get_frequency().")"."\n";
				}
				$node->convert_to_text($depth, $tag, $ptrOutput, $indent, $flag1, $flag2);
			}
		}
	}
	elsif (($depth <= 0 || $lastNode) && $tag) {
		foreach my $leaf ($tree->get_leafs()) {
			$$ptrOutput .= $indent."*".$leaf."\n";
		}
	}
}

sub convert_to_pnh {
	my ($tree, $ptrString) = @_;
	if ($tree->has_childrens()) {
		$$ptrString .= "(";
		foreach my $node ($tree->get_branches()) {
			if ($node->get_name()) {
				$$ptrString .= $node->get_name().":".$node->get_frequency();
				$node->convert_to_pnh($ptrString);
				$$ptrString .= ($tree->is_last_children($node))? ")":",";
			}
		}
	}
}

sub convert_from_pnh {
	my ($tree, $ptrString) = @_;
	$$ptrString =~ s/^[\(\,]([^:]+):(\d+)//;	
	my ($name,$frequency) = ($1,$2);
	#print STDERR ">($name,$frequency)\n"; #FOR DEBUG
	$tree->create_node($name,$frequency);
	if (substr($$ptrString,0,1) eq '(') { 
		($tree->get_node($name))->convert_from_pnh($ptrString);
	}
	elsif (substr($$ptrString,0,1) eq ',') {
		$tree->convert_from_pnh($ptrString);
		return;
	}
	if (substr($$ptrString,0,1) eq ')') {
		$$ptrString =~ s/^\)//;
		return;
	}
	elsif (substr($$ptrString,0,1) ne ';') { 
		$tree->convert_from_pnh($ptrString);
	}
}

###########################################################
# Atoms ... ;)
###########################################################
sub set_name {
	$_[0]->{name} = $_[1];
}

sub set_frequency {
	$_[0]->{frequency} = $_[1];
}

sub create_node {
	my ($tree, $node, $frequency) = @_;
	$frequency = 1 if (!$frequency);
	$_[0]->{branches}{$_[1]} = new($_[1],undef,$frequency);
}

sub get_name {
	return $_[0]->{name};
}

sub get_branches {
	return values(%{$_[0]->{branches}});
}

sub get_frequency {
	return $_[0]->{frequency};
}

sub get_node {
	return $_[0]->{branches}{$_[1]};
}

sub increment_frequency {
	$_[0]->{frequency}++;
}

sub node_exist {
	return 1 if ($_[0]->get_node($_[1]));
}

sub number_of_childrens {
	return scalar($_[0]->get_branches());
}

sub has_childrens {
	return (($_[0]->number_of_childrens()) > 0);
}

sub children_is_unique {
	return ($_[0]->number_of_childrens() == 1);
}

sub is_last_children {
	return (($_[0]->get_branches)[($_[0]->number_of_childrens())-1] eq $_[1]);
}

sub is_last_node {
	foreach ($_[0]->get_branches()) {
		return undef if $_->has_childrens();
	}
	return 1;
}

sub get_leafs {
	my @leafs;
	$_[0]->_get_leafs(\@leafs);
	return @leafs;
}

sub _get_leafs {
	if ($_[0]->has_childrens) {
		foreach ($_[0]->get_branches()) {
			$_->_get_leafs($_[1]);
		}
	}
	else {
		push(@{$_[1]}, $_[0]->get_name());
	}
}

1;
