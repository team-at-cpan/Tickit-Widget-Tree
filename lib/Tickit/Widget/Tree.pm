package Tickit::Widget::Tree;
# ABSTRACT: Simple tree widget for Tickit
use strict;
use warnings;
use parent qw(Tickit::Widget);
use List::Util qw(sum);
use Tickit::Utils qw(substrwidth);
use utf8;

our $VERSION = '0.001';

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

sub new {
	my $class = shift;
	my %args = @_;
	my $root = delete $args{root};
	my $parent = delete $args{parent};
	my $self = $class->SUPER::new(%args);
	$self->{children} ||= [];
	$args{line_style} ||= $parent->{line_style} if $parent;
	$args{line_style} ||= $root->{line_style} if $root;
	$args{line_style} ||= 'ascii';
	$self->{$_} = $args{$_} for qw(label is_open last prev next line_style);
	$self->update_root_and_parent(
		root	=> $root,
		parent	=> $parent
	);
	if(my $children = delete $args{children}) {
		foreach (@$children) {
			$self->add(my $child = $class->new(%args, %$_) or die "fail?");
			my $prefix_len = length($self->prefix_text);
		}
		$self->reapply_windows;
	}
	die "no line style" unless $self->{line_style};
	return $self;
}

sub lines {
	my $self = shift;
	my $lines = 1;
	return $lines unless $self->is_open;
	$lines += sum 0, map 0+$_->lines, $self->children;
	return $lines;
}
sub cols { my $self = shift; 1; }

sub is_open { shift->{is_open} }

sub window_gained {
	my $self = shift;
	# Let parent set up focus
	$self->SUPER::window_gained(@_);

	my $win = $self->window;
	$self->rebuild_all;
	return unless $self->is_open;
	return $self->reapply_windows;
}

sub reapply_windows {
	my $self = shift;
	my $win = $self->window or return;
	$_->set_window(undef) for $self->children;
	return unless $self->is_open;
	my $prefix_len = length $self->prefix_text;
	my $y = 1;
	my @tasks;
	CHILD:
	for my $child ($self->children) {
		my $height = ($child->is_open ? $child->lines : 1);
#		last CHILD unless ($y + $height) < ($self->window ? $self->window->lines : $self->lines);

		push @tasks, [ $child, ($child->window ? 'change' : 'create'), 
			$y,
			$prefix_len,
			$height,
			($self->window ? $self->window->cols : $self->cols) - $prefix_len
		];
		$y += $height;
	}
	$win->resize( $y, $win->cols);
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
}

sub add {
	my $self = shift;
	my $child = shift;
	die "bad child" unless $child->isa(ref $self);
	push @{$self->{children}}, $child;
	$child->{parent} ||= $self;
	$_->($child, $self) for grep defined, map $child->can($_), qw(on_add);
	$self->resized;
	$self;
}

sub rebuild {
	my $self = shift;
	my $node = shift;
	$self->{prev} = $node;
	$node->{next} = $self;
	return $self unless $self->is_open && $self->children;
	$node = $self;
	$node = $_->rebuild($node) for $self->children;
	return $node;
}

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

sub children { @{ shift->{children} || [] } }
sub is_highlighted { shift->{is_highlighted} }

sub prefix_text {
	my $self = shift;
	if($self->{line_style} eq 'ascii') {
		return ' | ';
	} elsif($self->{line_style} eq 'compact_ascii') {
		return '|';
	} elsif($self->{line_style} eq 'single') {
		return '│';
	} elsif($self->{line_style} eq 'double') {
		return '║';
	} elsif($self->{line_style} eq 'thick') {
		return '┃';
	} else {
		return 'ERR';
	}
}

sub render {
	my $self = shift;
	return unless my $win = $self->window;

	$win->goto(0,0);
	my $txt = '';
	if($self->{line_style} eq 'ascii') {
		$txt = '[' . ($self->is_open ? '-' : '+') . ']';
	} elsif($self->{line_style} eq 'compact_ascii') {
		$txt = ($self->is_open ? '-' : '+');
	} elsif($self->{line_style} eq 'single') {
		$txt = $self->{last} ? '└' : '├'; # : '└');
		if($self->children && $self->is_open) {
			$txt .= '┬';
		} elsif($self->children) {
			$txt .= '[' . ($self->is_open ? '-' : '+') . ']';
		} else {
			$txt .= '   ';
		}
	} elsif($self->{line_style} eq 'double') {
		$txt = $self->{last} ? '╚' : '╠';
		$txt .= '╗' if $self->children && $self->is_open;
	} elsif($self->{line_style} eq 'thick') {
		$txt = $self->{last} ? '┗' : '┣';
		$txt .= '┓' if $self->children && $self->is_open;
	} else {
		$txt = 'ERR';
	}
	$win->print($txt . ' ');

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

sub highlighted {
	my $self = shift;
	return $self->root->{highlighted} || $self->root;
}
sub label { shift->{label} }
sub highlight {
	my $self = shift;
#	$self->window->print( "Highlight " . $self->label);
	$self->highlighted->{is_highlighted} = 0;
	$self->{is_highlighted} = 1;
	Scalar::Util::weaken($self->root->{highlighted} = $self);
}


sub prev { shift->{prev} }
sub next { shift->{next} }
sub highlight_next {
	my $self = shift;
	$self->highlighted->next->highlight;
}
sub highlight_prev {
	my $self = shift;
	$self->highlighted->prev->highlight;
}
sub rerender {
	my $self = shift;
	return unless $self->window;
	$self->window->clear if $self->root eq $self;
	$self->redraw;
}

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
	unless($self->{line_style} eq $root->{line_style}) {
		$self->{line_style} = $root->{line_style};
		$self->redraw;
	}
	return $self;
}

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
		$self->highlighted->{is_open} = 1;
		$self->rebuild_all;
		$self->resized;
		return 1;
	} elsif($type eq 'text' && $str eq '-') {
		$self->highlighted->{is_open} = 0;
		$self->rebuild_all;
		$self->resized;
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

sub child_resized { }

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

sub on_add {
	my $self = shift;
	my $parent = shift;
	$self->update_root_and_parent(parent => $parent);
	return $self;
}

1;

__END__

=head1 SEE ALSO

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2011. Licensed under the same terms as Perl itself.

