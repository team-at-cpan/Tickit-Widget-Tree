package Tickit::Widget::Tree;
# ABSTRACT: Terminal tree widget
use strict;
use warnings;
use parent qw(Tickit::Widget Mixin::Event::Dispatch);

our $VERSION = '0.102';

=head1 NAME

Tickit::Widget::Tree - tree widget implementation for L<Tickit>

=head1 SYNOPSIS

 use Tickit::Widget::Tree;
 my $tree = Tickit::Widget::Tree->new(root => Tree::DAG_Node->new);

=head1 DESCRIPTION

B<NOTE>: Versions 0.003 and below used a custom graph management
implementation which had various problems with rendering glitches
and performance. This version has been rewritten from scratch to
use L<Tree::DAG_Node> to handle the tree structure, and as such
is not backward compatible.

=begin HTML

<p><img src="http://tickit.perlsite.co.uk/cpan-screenshot/tickit-widget-tree1.gif" alt="Tree widget in action" width="480" height="403"></p>

=end HTML

=cut

use Term::TermKey qw(KEYMOD_CTRL);
use Tickit::RenderBuffer qw(LINE_SINGLE CAP_START CAP_END CAP_BOTH);
use Tree::DAG_Node;
use List::Util qw(max);
use Tickit::Utils qw(textwidth);
use Tickit::Style;

use constant CLEAR_BEFORE_RENDER => 0;
use constant WIDGET_PEN_FROM_STYLE => 1;
use constant KEYPRESSES_FROM_STYLE => 1;
use constant CAN_FOCUS => 1;
# Tickit::Widget::ScrollBox has the details
use constant CAN_SCROLL => 1;

=head1 STYLES

The following style keys are recognised, in addition to base styling
which will be applied to the tree lines:

=over 4

=item * line_style - which line type to use, default 'single', other
options include 'thick' or 'double'

=item * expand_style - 'boxed' is the only option for now, to select
a Unicode +/- boxed icon

=item * highlight_(fg|bg|b|rv) - highlight styling

=item * highlight_full_row - if true, will apply highlighting to the
entire width of the widget, rather than just the text

=back

Key bindings are currently:

=over 4

=item * previous_row - move up a line, stepping into open nodes, default C<Up>

=item * next_row - move down a line, stepping into open nodes, default C<Down>

=item * up_tree - move to the parent, default C<Left>

=item * down_tree - move to the first child, opening the current node if
necessary, default C<Right>

=item * open_node - opens the current node, default C<+>

=item * close_node - closes the current node, default C<->

=item * activate - activates the current node, default C<Enter>

=item * first_row - jump to the first node in the tree, default C<Home>

=item * last_row - jump to the last node in the tree, default C<End>

=back

=cut

BEGIN {
	style_definition 'base' =>
		fg                   => 'white',
		toggle_fg            => 'white',
		label_fg             => 'white',
		line_style           => 'single',
		expand_style         => 'boxed',
		highlight_fg         => 'yellow',
		highlight_bg         => 'blue',
		highlight_b          => 1,
		highlight_full_row   => 0;

	style_definition ':focus' =>
		'<Up>'               => 'previous_row',
		'<Down>'             => 'next_row',
		'<Left>'             => 'up_tree',
		'<Right>'            => 'down_tree',
		'<PageUp>'           => 'previous_page',
		'<PageDown>'         => 'next_page',
		'<Home>'             => 'first_row',
		'<End>'              => 'last_row',
		'<Enter>'            => 'activate',
		'<+>'                => 'open_node',
		'<->'                => 'close_node';
}

sub cols {
	my $self = shift;
	$self->calculate_size unless exists $self->{cols};
	return $self->{cols};
}

sub lines {
	my $self = shift;
	$self->calculate_size unless exists $self->{lines};
	return $self->{lines};
}

=head2 calculate_size

Calculate the minimum size needed to contain the full tree with all nodes expanded.

Used internally.

=cut

sub calculate_size {
	my $self = shift;
	my $w = 0;
	my $h = 0;
	my $code = sub {
		my ($code, $node, $depth, $y) = @_;

		my $has_children = $node->daughters ? 1 : 0;

		# Our label - root isn't shown, and we don't want a blank
		# line at the top either, so we don't update the pointer for root
		unless($node->is_root) {
			# We only need to draw this if we're inside the rendering area
			$w = max $w, 1 + 3 * $depth + textwidth($node->name);

			# ... but we always want to update our current row pointer
			++$y;
		}

		# We can stop here if we're empty
		return $y unless $has_children;

		# Recurse into each child node, updating our height as we go
		my @child = $node->daughters;

		$y = $code->($code, $_, $depth + 1, $y) for @child;
		return $y;
	};
	$h = $code->($code, $self->root, 0, 0);
	warn "result: $w, $h";
	$self->{lines} = $h + 1;
	$self->{cols} = $w;
	return $self;
}

=head2 new

Instantiate. Takes the following named parameters:

=over 4

=item * root - the root L<Tree::DAG_Node>

=item * on_activate - coderef to call when a node has been activated (usually
via 'enter' keypress)

=back

=cut

sub new {
	my $class = shift;
	my %args = @_;
	my $root = delete($args{root}) || Tree::DAG_Node->new({name => 'Root'});
	my $activate = delete $args{on_activate};
	my $self = $class->SUPER::new(%args);
	$self->{root} = $root;
	$self->{on_activate} = $activate;
	$self->take_focus;
	$self
}

=head2 root

Accessor for the root node. If given a parameter, will set the root node accordingly (and
mark the tree for redraw), returning $self.

Otherwise, returns the root node - or undef if we do not have one.

=cut

sub root {
	my $self = shift;
	if(@_) {
		$self->{root} = shift;
		return $self;
	}
	return $self->{root}
}

=head2 window_gained

Work out our size, when we have a window to fit in.

=cut

sub window_gained {
	my $self = shift;
	$self->calculate_size;
	$self->SUPER::window_gained(@_);
}

=head2 set_scrolling_extents

Called by L<Tickit::Widget::ScrollBox> or other scroll-capable containers to
set up the extent objects which determine the drawable viewport offset.

=cut

sub set_scrolling_extents {
	my $self = shift;
	my ($v, $h) = @_;
	$self->{scroll_hextent} = $h;
	$self->{scroll_vextent} = $v;
	$self
}

=head2 scrolled

Called by L<Tickit::Widget::ScrollBox> or other scroll-capable containers to
indicate when scroll actions have occurred.

=cut

sub scrolled {
	my $self = shift;
	# TODO We could be far more efficient here
	$self->redraw;
}

=head2 render_to_rb

Render method. Used internally.

=cut

sub render_to_rb {
	my $self = shift;
	my ($rb, $rect) = @_;
	my $win = $self->window;

	$rb->clear;
	my $y_offset = $self->{scroll_vextent} ? $self->{scroll_vextent}->start : 0;
	my $x_offset = $self->{scroll_hextent} ? $self->{scroll_hextent}->start : 0;
	$rb->translate(-$y_offset, -$x_offset) if $y_offset || $x_offset;

	my $top = $rect->top + $y_offset;
	my $bottom = $rect->bottom + $y_offset;
	my $highlight_node = $self->highlight_node;
	my $regular_label_pen = $self->get_style_pen;
	my $line_pen = $self->get_style_pen;
	my $toggle_pen = $self->get_style_pen('toggle');
	my $highlight_pen = $self->get_style_pen('highlight');
	my $full_highlight = $self->get_style_values('highlight_full_row');

	my $code = sub {
		my ($code, $node, $depth, $y) = @_;

		# Bail out immediately if we're out of range for the target rendering area
		return $y if $y > $bottom;

		my $start_y = $y;
		my $has_children = $node->daughters ? 1 : 0;
		my $is_open = $node->attributes->{open} ? 1 : 0;

		# Line segment to the first child node, needed for
		# the case where we have a single child
		$rb->vline_at(
			$y,
			$y + 1,
			1 + 3 * ($depth),
			LINE_SINGLE,
			$line_pen,
			CAP_START
		) if $has_children && $is_open && $y >= $top;

		++$y unless $node->is_root;

		if($has_children && ($node->is_root || $is_open)) {
			# Recurse into each child node, updating our height as we go
			my @child = $node->daughters;

			# The vertical connecting line stops at the *start* of the last child,
			# so we want to end up with:
			#  \- child
			#     + other child
			# rather than
			#  |- child
			#  |  + other child
			# so we record the position this last child starts at in $tree_y
			my $last = pop @child;
			$y = $code->($code, $_, $depth + 1, $y) for @child;
			my $tree_y = $y;
			$y = $code->($code, $last, $depth + 1, $y) if $last;

			# And now we render those connecting lines, if we only have a single child
			# we've done this already.
			if($y >= $top && $node->daughters > 1) {
				$rb->vline_at(
					$start_y,
					$tree_y,
					1 + 3 * ($depth),
					LINE_SINGLE,
					$line_pen,
					CAP_START
				);
			}
		}

		# Our label - root isn't shown, and we don't want a blank
		# line at the top either, so we don't update the pointer for root
		unless($node->is_root) {
			# We only need to draw this if we're inside the rendering area
			if($start_y >= $top) {
				$rb->hline_at(
					$start_y,
					1 + 3 * ($depth - 1),
					(3 * $depth) - ($has_children ? 1 : 0),
					LINE_SINGLE,
					$line_pen,
				) if $depth;
				$rb->text_at(
					$start_y,
					1 + 3 * $depth,
					$node->name,
					($highlight_node == $node) ? $highlight_pen : $regular_label_pen
				);
				if($full_highlight && $highlight_node == $node) {
					my $start = (1 + 3 * $depth) + textwidth($node->name);
					$rb->text_at(
						$start_y,
						$start,
						' ' x ($rect->right - $start),
						$highlight_pen
					);
				}
				$win->cursor_at($start_y - $y_offset, (2 + 3 * ($depth - 1)) - $x_offset) if ($highlight_node == $node) && delete $self->{move_cursor};
				if($has_children) {
					$rb->char_at(
						$start_y,
						2 + 3 * ($depth - 1),
						$is_open ? 0x229F : 0x229E,
						$toggle_pen
					);
					Scalar::Util::weaken($self->{toggle}{join ',', $start_y, 2 + 3 * ($depth - 1)} = $node);
				}
			}
		}

		return $y;
	};
	$code->($code, $self->root, 0, 0);
	$rb->goto(0,0);
}

=head2 reshape

Workaround to avoid warnings from L<Tickit::Window>. This probably shouldn't
be here, pretend you didn't see it.

=cut

sub reshape {
	my $self = shift;
	if(my $win = $self->window) {
		$win->cursor_at(0,0);
		$self->{move_cursor} = 1;
	}
	$self->SUPER::reshape(@_)
}

=head2 on_mouse

Mouse callback. Used internally.

=cut

sub on_mouse {
	my $self = shift;
	my $ev = shift;
	if($ev->type eq 'press') {
		if(my $hotspot = $self->{toggle}{join ',', $ev->line, $ev->col}) {
			# Ctrl-click recursively opens/closes all nodes from the given point
			my $new = $hotspot->attributes->{open} ? 0 : 1;
			if($ev->mod & KEYMOD_CTRL) {
				$hotspot->walk_down({
					callback => sub {
						my $node = shift;
						$node->attributes->{open} = $new;
						return 1;
					}
				});
			} else {
				$hotspot->attributes->{open} = $new;
			}
			$self->redraw;
		}
	}
}

=head2 key_first_row

Jump to the first row. Normally bound to the C<Home> key.

=cut

sub key_first_row {
	my $self = shift;
	my ($node) = $self->root->daughters;
	$self->highlight_node($node);
	$self->redraw;
}

=head2 key_last_row

Jump to the last row. Normally bound to the C<End> key.

=cut

sub key_last_row {
	my $self = shift;
	my ($node) = reverse $self->root->daughters;
	while($node->attributes->{open} && $node->daughters) {
		($node) = reverse $node->daughters;
	}
	$self->highlight_node($node);
	$self->redraw;
}

=head2 key_previous_row

Go up a node.

=cut

sub key_previous_row {
	my $self = shift;
	my $node = $self->highlight_node;
	# If there are nodes before this one in the tree,
	# then we want the leaf node going down from ->left_sister
	if($node->left_sister) {
		$node = $node->left_sister;
		while($node->attributes->{open} && $node->daughters) {
			($node) = reverse $node->daughters;
		}
	} else {
		$node = $node->mother;
	}

	# if we've gone past the start, we're at the top
	($node) = $node->daughters if $node->is_root;
	$self->highlight_node($node);
	$self->redraw;
}

=head2 key_next_row

Move down a node.

=cut

sub key_next_row {
	my $self = shift;
	my $node = $self->highlight_node;
	# If we're open and there are any nodes under us, that's easy -
	# just pick the first one and we're done
	if($node->attributes->{open} && $node->daughters) {
		($node) = $node->daughters;
	} else {
		# We chase up the tree looking for a suitable 'next' entry - either
		# the next node across from us, or from the parent, etc. We may not
		# be able to find anything - in that case, we'll end up at the root.
		while(!$node->is_root) {
			if($node->right_sister) {
				$node = $node->right_sister;
				last;
			}
			$node = $node->mother;
		}
	}

	# if we've gone past the start, we're already at the bottom so we don't
	# do anything - just bail out here
	return $self if $node->is_root;

	$self->highlight_node($node);
	$self->redraw;
}

=head2 key_up_tree

Going "up" the tree means the parent of the current node.

=cut

sub key_up_tree {
	my $self = shift;
	my $node = $self->highlight_node;
	return $self if $node->is_root || $node->mother->is_root;
	$self->highlight_node($node->mother);
	$self->redraw;
}

=head2 key_down_tree

Going "down" the tree means the first child node, if we have one
and we're open.

=cut

sub key_down_tree {
	my $self = shift;
	my $node = $self->highlight_node;
	return $self unless $node->daughters;
	$node->attributes->{open} = 1 unless $node->attributes->{open};
	($node) = $node->daughters;
	$self->highlight_node($node);
	$self->redraw;
}

=head2 highlight_node

Change the currently highlighted node.

=cut

sub highlight_node {
	my $self = shift;
	if(@_) {
		my $prev = $self->{highlight_node};
		$self->{highlight_node} = shift;
		$self->invoke_event(highlight_node => $self->{highlight_node}, $prev);
		$self->{move_cursor} = 1;
		$self->redraw;
		return $self
	}
	($self->{highlight_node}) = $self->root->daughters unless $self->{highlight_node};
	return $self->{highlight_node};
}

=head2 key_open_node

Open this node.

=cut

sub key_open_node {
	my $self = shift;
	$self->highlight_node->attributes->{open} = 1;
	$self->redraw;
}

=head2 key_close_node

Close this node.

=cut

sub key_close_node {
	my $self = shift;
	$self->highlight_node->attributes->{open} = 0;
	$self->redraw;
}

=head2 on_activate

Call the C<on_activate> coderef if we have it.

=cut

sub on_activate {
	my $self = shift;
	$self->{on_activate}->($self->highlight_node) if $self->{on_activate};
	$self->invoke_event(activate => $self->highlight_node);
}

1;

__END__

=head1 TODO

Plenty of features and bugfixes left on the list, in no particular order:

=over 4

=item * Avoid full redraw when moving highlight or opening/closing nodes

=item * Support nested widgets

=item * Node reordering

=item * Detect changes to the underlying L<Tree::DAG_Node> structure

=back

=head1 AUTHOR

Tom Molesworth <cpan@entitymodel.com>

=head1 LICENSE

Copyright Tom Molesworth 2011-2013. Licensed under the same terms as Perl itself.

