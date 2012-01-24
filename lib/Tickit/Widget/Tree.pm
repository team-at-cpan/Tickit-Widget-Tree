package Tickit::Widget::Tree;
# ABSTRACT: Simple tree widget for Tickit
use strict;
use warnings;
use parent qw(Tickit::ContainerWidget);
use List::Util qw(sum);
use Scalar::Util qw(blessed);
use Tickit::Utils qw(substrwidth textwidth);
use Tickit::Widget::Static;
use utf8;

our $VERSION = '0.003';

=head1 NAME

Tickit::Widget::Tree - support for an expandable tree widget

=head1 SYNOPSIS

 my $tree = Tickit::Widget::Tree->new(
 	is_open => 1,
 	label => 'Root',
 	line_style => 'single',
 );
 $tree->add(Tickit::Widget::Tree->new(
 	is_open => 1,
 	label => 'First',
 ));

=head1 DESCRIPTION

Simple tree widget. See examples in main source for more info.

=cut

use constant CLEAR_BEFORE_RENDER => 0;

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
	my $self = $class->SUPER::new(%args);
	$self->{children} ||= [];

	$self->{$_} = delete $args{$_} for grep exists $args{$_}, qw(label is_open prev next line_style label_widget);
	$self->update_root_and_parent(
		root	=> $root,
		parent	=> $parent
	);
	$self->{pen_active} = delete $args{pen_active} || Tickit::Pen->new( fg => 2, bg => 4, b => 1 );
	$self->{pen_label} = delete $args{pen_label} || Tickit::Pen->new( bg => 16, fg => 7 );

	unless($self->{label_widget}) {
		$self->{label_widget} = Tickit::Widget::Static->new(
			text => defined($self->{label}) ? $self->{label} : ''
		);
		$self->{label_widget}->set_pen($self->pen_label);
	}

	if(my $children = delete $args{children}) {
		$self->add(my $child = $class->new(%args, %$_) or die "failed to add child to $self") for @$children;
	}
	return $self;
}

=head2 label

Set or retrieve the label for this widget.

By default all nodes use a L<Tickit::Widget::Static> widget as a label,
see the L</label_widget> method for more control over this.

=cut

sub label {
	my $self = shift;
	if(@_) {
		my $txt = shift;
		$self->{label} = $txt;
		$self->label_widget->set_text($txt)i if exists $self->{label_widget};
		$self->resized;
		return $self;
	}
	return $self->{label};
}

=head2 label_widget

Set or retrieve the label widget for this node. Typically a L<Tickit::Widget::Static>,
this must accept a pen which will be used for displaying the currently-highlighted node.

=cut

sub label_widget {
	my $self = shift;
	if(@_) {
		$self->{label_widget} = shift;
		$self->{label_widget}->set_pen($self->pen_label);
		$self->resized;
		return $self;
	}
	return $self->{label_widget};
}

# Is this still accurate?

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
		my $c = $prev;
		$c = $c->last_child while $c->has_children;
		return $c;
	}
	return $self->tree_parent if $self->tree_parent;
	return $self;
}

=head2 prev_sibling

Returns the previous sibling for this node. RO accessor.

=cut

sub prev_sibling { shift->{prev_sibling} }

=head2 next_sibling

Returns the next sibling for this node. RO accessor.

=cut

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
	return $self->_next_ignore_children(@_);
}

=head2 _next_ignore_children

Returns the next node, ignoring any children in the current node for the
purposes of the search.

=cut

sub _next_ignore_children {
	my $self = shift;
	my $ns = $self->next_sibling;
	return $ns if $ns;
	my $parent = $self->tree_parent or return $self;
	my $suggested = $parent->_next_ignore_children(@_);
	return $self if $suggested == $self->tree_parent;
	return $suggested;
}

=head2 lines

Returns the number of lines used by this node. Will be the label height
unless the widget is open.

=cut

sub lines {
	my $self = shift;
	my $lines = $self->label_widget->lines;
	return $lines unless $self->is_open;
	$lines += sum 0, map +$_->lines, $self->children;
	return $lines;
}

=head2 cols

Number of cols for autocalculation. We're flexible, so we just use the default 1.

=cut

sub cols { my $self = shift; textwidth($self->prefix_text) + $self->label_widget->cols; }

=head2 is_open

Returns true if this node is open, false if not.

If open state is undefined, then we inherit from the parent if we have one,
or return 0 as a fallback.

=cut

sub is_open {
	my $self = shift;
	return $self->{is_open} if defined $self->{is_open};
	return $self->tree_parent->is_open if $self->tree_parent;
	return 1;
}

=head2 is_closed

Returns true if we're closed, false if not.

=cut

sub is_closed { !(shift->is_open) }

=head2 window_lost

Callback to clear children and label when we lose our window.

=cut

sub window_lost {
	my $self = shift;

	$_->set_window(undef) for $self->label_widget, $self->children;
	return $self;
}

=head2 window_gained

Callback to adjust our widgets when we get a window.

=cut

sub window_gained {
	my $self = shift;
	# Let parent set up focus
	$self->SUPER::window_gained(@_);
	my $win = $self->window;
	$win->clear;
	$self->_reshape_label($win);
	$self->reapply_windows;
}

=head2 reshape

Callback to adjust the label window when our window is resized.

=cut

sub reshape {
	my $self = shift;
	$self->SUPER::reshape(@_);
	my $win = $self->window;
	$self->_reshape_label($win);
}

=head2 _reshape_label

Adjust our label window to fit the current window.

=cut

sub _reshape_label {
	my $self = shift;
	my $win = shift;
	die $self->label_widget->lines unless $self->label_widget->lines > 0;
	my $prefix = $self->style_map->{solo};
	my $prefix_len = textwidth $prefix;

	# TODO try change_geometry again
	$self->label_widget->set_window($win->make_sub(0, $prefix_len + 1, $self->label_widget->lines, $win->cols - ($prefix_len + 1)));
	return $self;
}

=head2 reapply_windows

Calculate windows for all widgets.

=cut

sub reapply_windows {
	my $self = shift;

	unless($self->is_open) {
		$_->set_window(undef) for $self->children;
		return $self->resized;
	}

	my $win = $self->window or return;

	my $prefix_len = textwidth $self->prefix_text;
	my $y = $self->label_widget->lines;
	my @tasks;
	CHILD:
	for my $child ($self->children) {
		my $height = $child->lines;

		push @tasks, [ $child, ($child->window ? 'change' : 'create'), 
			$y,
			$prefix_len,
			$height,
			$win->cols - $prefix_len
		];
		$y += $height;
	}

	foreach (@tasks) {
		my ($child, $type, @args) = @$_;
# TODO make this work again
#		if($type eq 'create') {
			my $sub = $win->make_sub(@args);
			$child->set_window($sub);
#		} else {
#			$child->window->change_geometry(@args);
#		}
	}
	return $self->resized;
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

Adds a child node after all other children.

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

	$self->resized;
	$self;
}

=head2 children

Returns a list of all children.

=cut

sub children { @{ shift->{children} || [] } }
sub has_children { @{ shift->{children} || [] } ? 1 : 0 }
sub first_child { shift->{children}[0] }
sub last_child { shift->{children}[-1] }
sub pen_label { shift->{pen_label} }
sub pen_active { shift->{pen_active} }

=head2 is_highlighted

Returns true if this node is highlighted.

Note that the highlighted widget itself is tracked through
the root node, see L</highlighted> for more information.

=cut

sub is_highlighted { shift->{is_highlighted} }

=head2 prefix_text

Returns the appropriate line prefix text for the current style.

=cut

sub prefix_text {
	my $self = shift;
	return $self->style_map->{prefix};
}

=head2 style_map

Returns hashref which maps a component to the character string used to represent it.

=cut

sub style_map {
	my $self = shift;
	return {
		ascii => {
			prefix      => '| ',
			is_open     => '[-]',
			is_closed   => '[+]',
			solo        => '-  ',
			open_parent => '-+ ',
			item_prefix => '+',
			item_last   => '\\',
		},
		single => {
			prefix      => '│ ',
			is_open     => '[-]',
			is_closed   => '[+]',
			solo        => '─  ',
			open_parent => '─┬ ',
			item_prefix => '├',
			item_last   => '└',
		},
		double => {
			prefix      => '║ ',
			is_open     => '[-]',
			is_closed   => '[+]',
			solo        => '═  ',
			open_parent => '═╦ ',
			item_prefix => '╠',
			item_last   => '╚',
		},
		thick => {
			prefix      => '┃ ',
			is_open     => '[-]',
			is_closed   => '[+]',
			solo        => '━  ',
			open_parent => '━┳ ',
			item_prefix => '┣',
			item_last   => '┗',
		},
	}->{$self->line_style};
}

=head2 render

Tickit::Window
=cut

sub render {
	my $self = shift;
	return unless my $win = $self->window;
	my %args = @_;
	my $rect = $args{rect} || $win->rect;
	my $style_map = $self->style_map;

	my $y = $rect->top;
	if($y < 1) {
		my $txt = $style_map->{$self->next_sibling ? 'item_prefix' : 'item_last'};
		if($self->children && $self->is_open) {
			$txt .= $style_map->{open_parent};
		} elsif($self->children) {
			$txt .= $style_map->{'is_closed'};
		} else {
			$txt .= $style_map->{solo};
		}
		if($rect->left <= textwidth $txt) {
			$win->goto($y, $rect->left);
			$win->print($txt);
		}
		++$y;
	}

	LINE:
	for ($y..$y+$rect->lines) {
		last LINE unless $_ < $win->lines;
		$win->goto($_, $rect->left);
		my $x = 0;
		$win->print($self->next_sibling ? $self->prefix_text : (' ' x textwidth($self->prefix_text))) if $rect->left <= textwidth($self->prefix_text);
		$x += textwidth($self->prefix_text);
		$win->erasech($rect->cols - $x, 1);
#		if($self->is_open && $self->children) {
#			$win->erasech($rect->cols - $x) unless $self->next_sibling;
#		} else {
#			$win->erasech($rect->cols - $x);
#		}
	}
}

=head2 highlighted

=cut

sub highlighted {
	my $self = shift;
	return $self->root->{highlighted} ||= $self->root;
}

=head2 highlight

=cut

sub highlight {
	my $self = shift;
	if(my $h = $self->highlighted) {
		$h->_unhighlight;
		$h->{is_highlighted} = 0;
	}

	$self->{is_highlighted} = 1;
	$self->_highlight;
	$self->root->{highlighted} = $self;
	return $self;
}

sub _highlight {
	my $self = shift;
	return $self unless my $w = $self->label_widget;

	$self->_set_pen_for_widget($w => 'active');
	return $self;
}

sub _unhighlight {
	my $self = shift;
	return $self unless my $w = $self->label_widget;

	$self->_set_pen_for_widget($w => 'label');
	return $self;
}

sub _set_pen_for_widget {
	my $self = shift;
	my $w = shift or die "no widget supplied";
	my $type = shift or die "no type supplied";

	my $method = 'pen_' . $type;
	$w->set_pen($self->$method);
	# TODO This seems wrong, but somehow I couldn't convince Tickit::Widget to pick up the new pen
	$w->window->set_pen($self->$method) if $w->window;
	$w->resized;
	return $self;
}

=head2 highlight_next

=cut

sub highlight_next {
	my $self = shift;
	my $next = $self->highlighted->next;
	$next->highlight;
	die "root variance? " . $next->root->label . ', ' . $self->root->label if $next->root != $self->root;
	die "Highlight failed? " . join ', ', $next->label, $self->highlighted->label, $next->highlighted->label, $next->is_highlighted unless $next->label eq $self->highlighted->label;
	die "Next does not match highlight: " . $next->tree_parent->label . ", " . $self->highlighted->tree_parent->label . ", $self" unless $next == $self->highlighted && $next eq $self->highlighted;
	die "Next item (" . $next->label . ") is not highlighted" unless $next->is_highlighted;
	die "Highlighted (" . $self->highlighted->label . ") is not highlighted, I am " . $self->label unless $self->highlighted->is_highlighted;
	return $self;
}

=head2 highlight_prev

=cut

sub highlight_prev {
	my $self = shift;
	$self->highlighted->prev->highlight;
	die "Highlighted (" . $self->highlighted->label . ") is not highlighted, I am " . $self->label unless $self->highlighted->{is_highlighted};
	return $self;
}

=head2 rerender

=cut

sub rerender {
	my $self = shift;
	return unless $self->window;
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
	return $_ for grep $_, $self->{root}, $self->tree_parent && $self->tree_parent->root, $self;
}

=head2 tree_parent

=cut

sub tree_parent {
	my $self = shift;
	if(@_) {
		$self->update_root_and_parent(
			parent	=> shift
		);
		return $self;
	}
	return $self->{tree_parent};
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
	die "Wrong root" if $old_root && $root && $root != $old_root;

	$root ||= $parent->root if $parent;
	$parent ||= $root;

	unless($root) {
		$root = $self;
		$self->{highlighted} = $self;
		$args{prev} = $self;
		$args{next} = $self;
	}
	$self->{tree_parent} = $parent;
	return $self;
}

=head2 on_key

=cut

sub on_key {
	my $self = shift;
	my ($type, $str, $key) = @_;
	if($type eq 'key' && $str eq 'Down') {
		$self->highlight_next;
		return 1;
	} elsif($type eq 'key' && $str eq 'Up') {
		$self->highlight_prev;
		return 1;
	} elsif($type eq 'text' && $str eq '+') {
		$self->highlighted->open;
		return 1;
	} elsif($type eq 'text' && $str eq '-') {
		$self->highlighted->close;
		return 1;
	} elsif($type eq 'text' && $str eq 'r') {
		$self->root->rerender;
		return 1;
	}
	return $self->_updated_on_key(@_);
	return 0;
}

=head2 open

Open this node. Triggers a redraw.

=cut

sub open {
	my $self = shift;
	$self->{is_open} = 1;
	$self->tree_parent->reapply_windows if $self->tree_parent;
	$self->next_sibling->reapply_windows if $self->next_sibling;
	$self->resized;
	return $self;
}

=head2 close

Close this node. Triggers a redraw.

=cut

sub close {
	my $self = shift;
	$self->{is_open} = 0;
	$self->next_sibling->reapply_windows if $self->next_sibling;
	$self->tree_parent->reapply_windows if $self->tree_parent;
	$self->resized;
	return $self;
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

=head2 on_add

Make sure everything has a consistent view of root and parent nodes
when adding a widget to a parent.

=cut

sub on_add {
	my $self = shift;
	my $parent = shift;
	$self->update_root_and_parent(parent => $parent);
	return $self;
}

=head2 line_style

Returns current line style. Inherits from parent, root or defaults to 'single'.

=cut

sub line_style {
	my $self = shift;
	if(@_) {
		$self->{line_style} = shift;
		return $self;
	}
	return $self->{line_style} if defined $self->{line_style};
	return $self->tree_parent->line_style if $self->tree_parent;
	return $self->root->line_style if $self->root && $self->root != $self;

# Fallback default
	return 'single';
}

1;

__END__

=head1 SEE ALSO

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2011. Licensed under the same terms as Perl itself.

