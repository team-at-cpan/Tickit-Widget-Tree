package Tickit::Widget::Tree::AdapterTransformation;

use strict;
use warnings;

=pod

A transformation acts much like a regular adapter, with the addition
of two callbacks that can be used to convert the source data into something
suitable for displaying in the tree.

With an OrderedList:

 item => $code->($item, $idx, $adapter, $tree)

An UnorderedMap also has the ability to remap keys:

 key  => $code->($key, $item, $idx, $adapter, $tree)
 item => $code->($key, $item, $idx, $adapter, $tree)

=cut

sub new {
	my $class = shift;
	bless {
		item => sub { $_[1] },
		key => sub { $_[0] },
		@_
	}, $class
}

sub adapter { shift->{adapter} }
sub item { my $self = shift; $self->{item}->(@_) }
sub key { my $self = shift; $self->{key}->(@_) }

1;

