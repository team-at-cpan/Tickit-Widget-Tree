package Tickit::Widget::Tree;
# ABSTRACT: Simple tree widget for Tickit
use strict;
use warnings;
use parent qw(Tickit::Widget);
use List::Util qw(sum);
use Scalar::Util qw(blessed);
use Tickit::Utils qw(substrwidth);
use utf8;

our $VERSION = '0.002';

=head1 NAME

Tickit::Widget::Tree - support for an expandable tree widget

=head1 SYNOPSIS

 my $tree = Tickit::Widget::Tree->new(
 	is_open => 1,
 	last => 1,
 	label => 'Root',
 	line_style => 'single',
 );
 $tree->add(Tickit::Widget::Tree->new(
 	is_open => 1,
 	last => 1,
 	label => 'First',
 ));

=head1 DESCRIPTION

Simple tree widget. See examples in main source for more info.

=head1 METHODS

=cut

=head2 new

Instantiate new tree.

Takes the following named parameters:

=over 4

=item * root - the root widget for this tree (optional)

=item * parent - the parent widget (optional)

=item * line_style - type of line used, see L</LINE STYLES> (optional, defaults to single)

=item * label - the text label to use for this tree entry

=back

=cut

sub new {
	my $class = shift;
	my %args = @_;
	my $root = delete $args{root};
	my $parent = delete $args{parent};
	$root ||= $parent->root if $parent;
	my $self = $class->SUPER::new(%args);
	$self->{children} ||= [];

	$args{last} ||= 1;

	$self->{$_} = delete $args{$_} for grep exists $args{$_}, qw(label is_open last prev next line_style);
	$self->update_root_and_parent(
		root	=> $root,
		parent	=> $parent
	);
	if(my $children = delete $args{children}) {
		foreach (@$children) {
			$self->add(my $child = $class->new(%args, %$_) or die "fail?");
			my $prefix_len = length($self->prefix_text);
			# TODO - remember what was going to happen here
		}
		$self->reapply_windows;
	}
	return $self;
}

=head2 prev

The 'previous element' is determined as follows:

=over 4

=item * If we have a previous sibling:

=over 4

=item * If the previous sibling is open and has children, returns the last of those

=item * If the previous sibling is closed or empty, returns it (the previous sibling)

=back

=item * If we have a parent and no previous sibling(s):

=over 4

=item * Return the parent

=back

=item * Return ourselves

=back

=cut

sub prev {
	my $self = shift;
	if(my $prev = $self->prev_sibling) {
		return $prev if $prev->is_closed;
		my ($last_sibling_child) = ($prev->children)[-1];
		return $last_sibling_child if $last_sibling_child;
		return $prev;
	}
	return $self->parent if $self->parent;
	return $self;
}

sub prev_sibling { shift->{prev_sibling} }
sub next_sibling { shift->{next_sibling} }

=head2 next

The 'next element' is determined as follows:

=over 4

=item * If we're open and have children, next is the first child element.

=item * If we're closed or empty, next is the next sibling at our current level.

=item * If there's no next sibling, call the parent's L</next> method.

=item * If there's no parent, return $self.

=back

=cut

sub next {
	my $self = shift;
	return $self->first_child if $self->is_open && $self->has_children;
	return $self->next_ignore_children(@_);
}

sub next_ignore_children {
	my $self = shift;
	my $ns = $self->next_sibling;
	return $ns if $ns;
	my $parent = $self->parent or return $self;
	my $suggested = $parent->next_ignore_children(@_);
	return $self if $suggested == $self->parent;
	return $suggested;
}

=head2 lines

Returns the number of lines used by this node. Will be the label height
unless the widget is open.

=cut

sub lines {
	my $self = shift;
	my $lines = 1;
	return $lines unless $self->is_open;
	$lines += sum 0, map 0+$_->lines, $self->children;
	return $lines;
}

=head2 cols

Number of cols for autocalculation. We're flexible, so we just use the default 1.

=cut

sub cols { my $self = shift; 1; }

=head2 is_open

Returns true if this node is open, false if not.

If open state is undefined, then we inherit from the parent if we have one,
or return 0 as a fallback.

=cut

sub is_open {
	my $self = shift;
	return $self->{is_open} if defined $self->{is_open};
	return $self->parent->is_open if $self->parent;
	return 1;
}

sub is_closed { !shift->is_open }

=head2 window_gained

=cut

sub window_gained {
	my $self = shift;
	# Let parent set up focus
	$self->SUPER::window_gained(@_);

	my $win = $self->window;
	$self->rebuild_all;
#	return unless $self->is_open;
	return $self->reapply_windows;
}

=head2 reapply_windows

=cut

sub reapply_windows {
	my $self = shift;
	my $win = $self->window or return;
	unless($self->is_open) {
		$_->set_window(undef) for $self->children;
		return;
	}
	my $prefix_len = length $self->prefix_text;
	my $y = 1;
	my @tasks;
	CHILD:
	for my $child ($self->children) {
		my $height = $child->lines;
#		last CHILD unless ($y + $height) < ($self->window ? $self->window->lines : $self->lines);

		push @tasks, [ $child, ($child->window ? 'change' : 'create'), 
			$y,
			$prefix_len,
			$height,
			$win->cols - $prefix_len
		];
		$y += $height;
	}
	$win->resize($y, $win->cols);
	foreach (@tasks) {
		my ($child, $type, @args) = @$_;
		if($type eq 'create') {
			my $sub = $win->make_sub(@args);
			$child->set_window($sub);
		} else {
			$child->window->change_geometry(@args);
		}
	}
}

=head2 C<insert_after>

Adds $v after $self.

Takes over $self->next, including backlink from $self->next->prev.

=cut

sub insert_after {
	my $self = shift;
	my $v = shift;
	$self->{next_sibling} = $v;
	$v->{prev_sibling} = $self;

# Take a copy of the next in the chain
	my $next = $self->{next};

# Insert our new entry after us
	Scalar::Util::weaken($self->{next} = $v);
# Link them back to us
	Scalar::Util::weaken($v->{prev} = $self);
# Set their next entry to our next entry
	Scalar::Util::weaken($v->{next} = $next);
# Update backlink from next in chain back to the new value
	Scalar::Util::weaken($next->{prev} = $v);
	return $self;
}

=head2 add

=cut

sub add {
	my $self = shift;
	my $child = shift;
	die "bad child" unless $child->isa(__PACKAGE__);

	my $prev = $self->{children}[-1];
	$prev->insert_after($child) if $prev;

# Add to queue
	push @{$self->{children}}, $child;

# Try to send the on_add signal
	$_->($child, $self) for grep defined, map $child->can($_), qw(on_add);

# Update last child to let it know that there's another widget following it now
	$prev->{last} = 0 if $prev;
	$self->resized;
	$self;
}

=head2 rebuild

=cut

sub rebuild {
	my $self = shift;
	my $node = shift or die 'nodes';
	$self->{prev} = $node;
	$node->{next} = $self;
	return $self unless $self->is_open && $self->children;
	$node = $self;
	$node = $_->rebuild($node) for $self->children;
	return $node;
}

=head2 rebuild_all

=cut

sub rebuild_all {
	my $self = shift;
	my $root = $self->root;
	$root->{prev} = $root;
	($root->{next}) = $root->children;
	my $node = $root;
	$node = $_->rebuild($node) for $root->children;
	$node->{next} = $root;
	$root->{prev} = $node;
	$root->highlight unless $root->highlighted;
	$self->resized;
}

=head2 children

=cut

sub children { @{ shift->{children} || [] } }

sub has_children { @{ shift->{children} || [] } ? 1 : 0 }
sub first_child { shift->{children}[0] }
sub last_child { shift->{children}[-1] }

=head2 is_highlighted

=cut

sub is_highlighted { shift->{is_highlighted} }

=head2 prefix_text

=cut

sub prefix_text {
	my $self = shift;
	my $style = $self->line_style;
	if($style eq 'ascii') {
		return ' | ';
	} elsif($style eq 'compact_ascii') {
		return '|';
	} elsif($style eq 'single') {
		return '│ ';
	} elsif($style eq 'double') {
		return ' ║';
	} elsif($style eq 'thick') {
		return ' ┃';
	} else {
		die "No line_style found for $self\n";
	}
}

=head2 render

=cut

sub render {
	my $self = shift;
	return unless my $win = $self->window;

	$win->goto(0,0);
	my $txt = '';
	my $style = $self->line_style or die "No line style for $self";
	if($style eq 'ascii') {
		$txt = '[' . ($self->is_open ? '-' : '+') . ']';
	} elsif($style eq 'compact_ascii') {
		$txt = ($self->is_open ? '-' : '+');
	} elsif($style eq 'single') {
		$txt = $self->{last} ? '└' : '├'; # : '└');
		if($self->children && $self->is_open) {
			$txt .= '─┬ ';
		} elsif($self->children) {
			$txt .= '[' . ($self->is_open ? '-' : '+') . ']';
		} else {
			$txt .= '─  ';
		}
	} elsif($style eq 'double') {
		$txt = $self->{last} ? '╚' : '╠';
		if($self->children && $self->is_open) {
			$txt .= '═╦ ';
		} elsif($self->children) {
			$txt .= '[' . ($self->is_open ? '-' : '+') . ']';
		} else {
			$txt .= '═  ';
		}
	} elsif($style eq 'thick') {
		$txt = $self->{last} ? '┗' : '┣';
		if($self->children && $self->is_open) {
			$txt .= '━┳ ';
		} elsif($self->children) {
			$txt .= '[' . ($self->is_open ? '-' : '+') . ']';
		} else {
			$txt .= '   ';
		}
	} else {
		die 'Invalid style for tree element';
	}
	$win->print($txt);

	my $lbl = $self->{label} // 'undef';
	$lbl = substrwidth $lbl, 0, $win->cols;
	if($self->is_highlighted) {
		$win->print($lbl, bg => 4);
	} else {
		$win->print($lbl);
	}
	return unless $self->is_open;
	return unless $self->children;
	unless($self->{last}) {
		foreach my $l (1..($self->lines - 1)) {
			$win->goto($l, 0);
			$win->print($self->prefix_text);
		}
	}
	$_->render for $self->children;
}

=head2 highlighted

=cut

sub highlighted {
	my $self = shift;
	return $self->root->{highlighted} || $self->root;
}

=head2 label

=cut

sub label { shift->{label} }

=head2 highlight

=cut

sub highlight {
	my $self = shift;
	$self->highlighted->{is_highlighted} = 0;
	$self->{is_highlighted} = 1;
	Scalar::Util::weaken($self->root->{highlighted} = $self);
}

=head2 highlight_next

=cut

sub highlight_next {
	my $self = shift;
	$self->highlighted->next->highlight;
}

=head2 highlight_prev

=cut

sub highlight_prev {
	my $self = shift;
	$self->highlighted->prev->highlight;
}

=head2 rerender

=cut

sub rerender {
	my $self = shift;
	return unless $self->window;
	$self->window->clear if $self->root eq $self;
	$self->redraw;
}

=head2 root

=cut

sub root {
	my $self = shift;
	if(@_) {
		$self->update_root_and_parent(
			root	=> shift
		);
		return $self;
	}
	return $self->{root};
}

=head2 parent

=cut

sub parent {
	my $self = shift;
	if(@_) {
		$self->update_root_and_parent(
			parent	=> shift
		);
		return $self;
	}
	return $self->{parent};
}

=head2 update_root_and_parent

=cut

sub update_root_and_parent {
	my $self = shift;
	my %args = @_;
	my $old_root = $self->root;
	my $parent = delete $args{parent};
	my $root = delete $args{root};
	die "Parent does not match root" if $parent && $root && $parent->root != $root;

	$root ||= $parent->root if $parent;
	$parent ||= $root;

	unless($root) {
		$root = $self;
		$self->{highlighted} = $self;
		$args{prev} = $self;
		$args{next} = $self;
	}
	$self->{parent} = $parent;
	$self->{root} = $root;
	return $self;
}

=head2 on_key

=cut

sub on_key {
	my $self = shift;
	my ($type, $str, $key) = @_;
	if($type eq 'key' && $str eq 'Down') {
		$self->highlight_next($self);
		$self->rerender;
		return 1;
	} elsif($type eq 'key' && $str eq 'Up') {
		$self->highlight_prev($self);
		$self->rerender;
		return 1;
	} elsif($type eq 'text' && $str eq 'A') {
		my @pending = $self->root;
		$self->window->focus(0,0) if $self->window;
		while(@pending) {
			my $node = shift @pending;
			push @pending, $node->children;
			$node->{is_open} = 1;
		}
		$self->rebuild_all;
		$self->root->resized;
		return 1;
	} elsif($type eq 'text' && $str eq 'a') {
		my @pending = $self->root;
		$self->window->focus(0,0) if $self->window;
		while(@pending) {
			my $node = shift @pending;
			push @pending, $node->children;
			$node->{is_open} = 0;
		}
		$self->rebuild_all;
		$self->root->resized;
		return 1;
	} elsif($type eq 'text' && $str eq '+') {
		$self->highlighted->open;
		return 1;
	} elsif($type eq 'text' && $str eq '-') {
		$self->highlighted->close;
		return 1;
	} elsif($type eq 'text' && $str eq 'r') {
		$self->rebuild_all;
		$self->root->rerender;
		return 1;
	}
	if($type eq 'key' && $str eq 'Tab') {
		my $target;
		unless($target) {
			$target = $self;
			$target = $target->parent while $target->parent;
		}
		$target->window->focus(0,0) if $target->window;
		$target->children_changed if $target->can('children_changed');
		return 1;
	}
	return $self->_updated_on_key(@_);
	return 0;
}

=head2 open

=cut

sub open {
	my $self = shift;
	$self->{is_open} = 1;
	$self->resized;
	$self->root->rebuild_all;
	return $self;
}

=head2 close

=cut

sub close {
	my $self = shift;
	$self->{is_open} = 0;
	$self->resized;
	$self->root->rebuild_all;
	return $self;
}

=head2 child_resized

=cut

sub child_resized { }

=head2 bind_keys

=cut

sub bind_keys {
	my $self = shift;
	while(@_) {
		my $str = shift;
		my $value = shift;

		if(defined $value) {
			$self->{keybindings}{$str} = $value;
		} else {
			delete $self->{keybindings}{$str};
		}
	}
}


sub _updated_on_key {
	my $self = shift;
	my ($type, $str, $key) = @_;

	# If we have something bound for this key, delegate to the defined handler
	return $self->$_( $str, $key ) for map {
		$_ eq 'CODE'
		? $_->($str, $key)
		: $_ eq 'ARRAY'
		? @$_
		: $_ eq 'HASH'
		? values %$_
		: $_
	} grep defined, (
		($type eq "key")
		? ($self->{keybindings}{$str})
		: ()
	);

	if($type eq "text") {
		$_->($self, $str) for grep $_, $self->can('on_text');
		return 1;
	}

	return 0;
}

=head2 on_add

=cut

sub on_add {
	my $self = shift;
	my $parent = shift;
	$self->update_root_and_parent(parent => $parent);
	$self->{last} = 1;
	return $self;
}

=head2 line_style

=cut

sub line_style {
	my $self = shift;
	if(@_) {
		$self->{line_style} = shift;
		return $self;
	}
	return $self->{line_style} if defined $self->{line_style};
	return $self->parent->line_style if $self->parent && $self->parent->isa(__PACKAGE__);
	return $self->root->line_style if $self->root && $self->root != $self;

# Fallback default
	return 'single';
}

=head2 is_valid_tree_node

Returns 1 if this is a valid tree node, 0 if not.

A 'valid tree node' is defined as an object which is a subclass or instance
of the L<Tickit::Widget::Tree> class.

=cut

sub is_valid_tree_node {
	my $self = shift;
	my $node = shift;
	return +(blessed($node) && $node->isa(__PACKAGE__)) ? 1 : 0;
}

1;

__END__

=head1 SEE ALSO

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2011. Licensed under the same terms as Perl itself.

