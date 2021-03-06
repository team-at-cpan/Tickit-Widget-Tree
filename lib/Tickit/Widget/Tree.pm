package Tickit::Widget::Tree;
# ABSTRACT: Terminal tree widget
use strict;
use warnings;

use parent qw(Tickit::Widget);

our $VERSION = '0.115';

=head1 NAME

Tickit::Widget::Tree - tree widget implementation for L<Tickit>

=head1 SYNOPSIS

 use Tickit::Widget::Tree;
 my $tree = Tickit::Widget::Tree->new(root => Tickit::Widget::Tree::Node->new);

=head1 DESCRIPTION

B<NOTE>: Versions 0.003 and below used a custom graph management
implementation which had various problems with rendering glitches
and performance. This version has been rewritten from scratch to
use L<Tree::DAG_Node> to handle the tree structure, and as such
is not backward compatible.

=begin HTML

<p><img src="http://tickit.perlsite.co.uk/cpan-screenshot/tickit-widget-tree1.gif" alt="Tree widget in action" width="480" height="403"></p>

=end HTML

=head2 Types of data

A basic tree would display a hierarchy of strings.

 [ first => [ qw(one two three four five) ], second => [ qw(red orange yellow blue) ] ]

If you want more interesting rendering, L<String::Tagged> can be used to apply attributes,
such as colours or bold/italic.

 [ map String::Tagged->new("$_", fg => $_), qw(red orange yellow green blue) ]

More complicated layouts can be achieved with nested widgets.

 [ { name => 'progress', widget => Tickit::Widget::Progressbar::Horizontal->new } ]

=head2 Data sources

There's two main ways of providing data.

=head3 Static data

A simple static tree layout can be constructed from Perl data structures
(arrayrefs, hashrefs and strings).

=head3 Adapter as data source

It is also possible to generate tree layouts which react to changes to the underlying
data. These "dynamic" trees use the L<Adapter::Async> to interact with a data source.

See L<Adapter::Async> for more information, and the notes in L<Tickit::Widget::Table>
may help as well.

=cut

use Tickit::RenderBuffer qw(LINE_SINGLE LINE_DOUBLE CAP_START CAP_END CAP_BOTH);
use Tickit::Widget::Tree::Node;
use Scalar::Util;
use List::Util qw(max);
use Tickit::Utils qw(textwidth);
use Tickit::Style;

use Variable::Disposition qw(retain_future);

use Mixin::Event::Dispatch::Bus;

use Log::Any qw($log);

use Adapter::Async::OrderedList::Array;
use Tickit::Widget::Tree::AdapterTransformation;

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

=item * highlight_full_row - if true, will apply highlighting to the
entire width of the widget, rather than just the text

=back

In addition, these styles define the attributes applied when rendering:

=over 4

=item * highlight - highlight styling

=item * selected - styling for items that have been marked as selected

=item * label - normal (not selected/highlighted) label styling

=item * toggle - normal (not selected/highlighted) open/close icon styling

=item * line - tree line characters

=back

Suffix list - the above base names can have any of the standard L<Tickit> attribute
names applied, e.g. C<highlight_fg>:

=over 4

=item * fg - foreground colour

=item * bg - foreground colour

=item * b - bold

=item * i - italic

=item * rv - reverse video

=back

=begin HTML

<p><img src="http://tickit.perlsite.co.uk/cpan-screenshot/tickit-widget-tree2.png" alt="Tree widget styles" width="302" height="249"></p>

=end HTML

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

=item * previous_page - up a full "page" (number of lines in widget), default C<PgUp>

=item * next_page - up a full "page" (number of lines in widget), default C<PgDown>

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

=head1 METHODS

=head2 new

Instantiate. Takes the following named parameters:

=over 4

=item * root - the root L<Tickit::Widget::Tree::Node>. Note that this will be silently upgraded if you
provide something else - thus a L<Tree::DAG_Node> can be used without needing to convert it manually, but
please note that this will *modify* your data structure, it won't clone it to create a new one.

=item * on_activate - coderef to call when a node has been activated (usually
via 'enter' keypress)

=item * data - if provided, this will be used as a data structure to build the initial tree.

=back

Example usage:

 Tickit:Widget::Tree->new(
  data => [
 	node1 => [
		qw(some nodes here)
	],
	node2 => [
		qw(more nodes in this one),
		and => [
			qw(this has a few child nodes too)
		]
	],
  ];
 );

You can get "live" nodes by attaching an L<Adapter::Async::OrderedList> instance, see
L</Data sources>:

 Tickit:Widget::Tree->new(
  data => [
    live => my $adapter = Adapter::Async::OrderedList::Array->new(data => [ ]),
 	static => [
		qw(some static nodes here that will not change)
	],
  ];
 );
 ( # and this is where the magic happens...
  Future::Utils::repeat {
   my $item = shift;
   $loop->delay_future(
    after => 0.5
   )->then(sub {
    $adapter->push([ $item ])
   })
  } foreach => [qw(live changes work like this)]
 )->get;

Normally the adapter would come from somewhere else - database cursor, L<Tangence> property,
etc. - rather than being instantiated in-place like this. See C< examples/adapter.pl > for
a simple example of a manually-driven adapter.

=cut

sub new {
	my $class = shift;
	my %args = @_;
	my $root = delete($args{root});
	my $data = delete $args{data};
	my $activate = delete $args{on_activate};
	my $self = $class->SUPER::new(%args);
	$root ||= $class->node_class->new({
		name => 'Root',
		attributes => {
			is_open => 1,
			tree => $self
		}
	});
	$root = $class->node_class->upgrade($self, $root) unless $root->isa($class->node_class);
	$self->add_item_under_parent($root, $data) if defined $data;

	$self->{root} = $root;
	$self->{on_activate} = $activate;
	$self->take_focus;
	$self
}

=head2 node_class

Name of the class we'll use for instantiating new nodes. Currently
hardcoded to L<Tickit::Widget::Tree::Node>.

=cut

sub node_class { 'Tickit::Widget::Tree::Node' }

=head2 bus

Event bus. An instance of L<Mixin::Event::Dispatch::Bus>.

=cut

sub bus { shift->{bus} //= Mixin::Event::Dispatch::Bus->new }

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

=head2 add_item_under_parent

Adds the given item under a parent node.

Takes the following parameters:

=over 4

=item * $parent - which L<Tickit::Widget::Tree::Node> to add this item to

=item * $item - a thing to add

=back

Currently this supports:

=over 4

=item * plain strings - will be used directly as the node label

=item * L<String::Tagged> instances - used as the node label, standard formatting (b/fg/bg)

=item * arrayrefs

=item * L<Adapter::Async::OrderedList> instances - "live" nodes that autoupdate

=back

Probably returns the $node that was just added, but don't count on it.

=cut

sub add_item_under_parent {
	my ($self, $parent, $item) = @_;

	# Adapters are special
	if(Scalar::Util::blessed($item)) {
		if($item->isa('Adapter::Async::OrderedList')) {
			$parent->adapter_for_node($self => $item);
			return $parent;
		} elsif($item->isa('Adapter::Async::UnorderedMap')) {
			$parent->adapter_for_node($self => $item);
			return $parent;
		} elsif($item->isa('Tickit::Widget::Tree::AdapterTransformation')) {
			# $log->debugf("We have a transformation %s", $item);
			$parent->adapter_for_node($self => $item->adapter, $item);
			return $parent;
		}
	}

	my @nodes = $self->nodes_from_data($item);
	$parent->add_daughter($_) for @nodes;
	$parent;
}

sub new_named_node {
	my ($self, $name) = @_;
	$self->node_class->new({
		name => "$name",
		attributes => {
			open => 1,
			tree => $self,
		},
	})
}

=head2 nodes_from_data

Given a scalar:

=over 4

=item * $item - a thing to add

=back

this will generate zero or more nodes that can be added to the tree.

Currently this supports:

=over 4

=item * plain strings - will be used directly as the node label

=item * L<String::Tagged> instances - used as the node label, standard formatting (b/fg/bg)

=item * arrayrefs - lists of things, probably recursive as well

=item * hashrefs - one text node will be created for each key, using the key as the name, and the content will be generated recursively using this method again

=item * L<Adapter::Async::OrderedList> instances - "live" nodes that autoupdate

=item * L<Adapter::Async::UnorderedMap> instances - like list, but with names as well

=back

Probably returns the $node that was just added, but don't count on it.


=cut

sub nodes_from_data {
	my ($self, $item) = @_;

	# Empty list for undef
	return unless defined $item;

	if(my $ref = ref $item) {
		if(Scalar::Util::blessed($item)) {
			return $self->new_named_node($item) if $item->isa('String::Tagged');

			die "Unknown blessed object - $item";
		} elsif($ref eq 'HASH') {
			# Expand this into one node per hash entry
			my @nodes;
			for my $k (sort keys %$item) {
				my $node = $self->new_named_node($k);
				$node->add_item_under_parent($node => $item->{$k});
				push @nodes, $node;
			}
			return @nodes;
		} elsif($ref eq 'ARRAY') {
			# We can recurse through these immediately
			my $prev;
			my @nodes;
			# $log->debugf("Starting loop for %d items", 0 + @$item);
			for(@$item) {
				if(!ref($_) || (Scalar::Util::blessed($_) && $_->isa('String::Tagged'))) {
					# $log->debugf("Had text thing - %s", "$_");
					$prev = $self->new_named_node($_);
					push @nodes, $prev;
				} else {
					if($prev) {
						# $log->debugf("Had ref, got label, adding under that - node %s gets %s", $prev, $_);
						$self->add_item_under_parent($prev => $_);
					} else {
						# $log->debugf("Had ref, no label, try to expand %s", $_);
						push @nodes, $self->nodes_from_data($_)
					}
				}
			}
			return @nodes
		} else {
			die 'This data was not in the desired format. Sorry.';
		}
	}
	return $self->new_named_node($item);
}

=head2 position_adapter

Returns the "position" adapter. This is an L<Adapter::Async::OrderedList::Array>
indicating where we are in the tree - it's a list of all the nodes leading to
the currently-highlighted one.

Note that this will return L<Tickit::Widget::Tree::Node> items. You'd probably
want the L<Tree::DAG_Node/name> method to get something printable.

Example usage:

 my $tree = Tickit::Widget::Tree->new(...);
 my $where_am_i = Tickit::Widget::Breadcrumb->new(
  item_transformations => sub {
   shift->name
  }
 );
 $where_am_i->adapter($tree->position_adapter);

=cut

sub position_adapter {
	shift->{position_adapter} ||= do {
		Adapter::Async::OrderedList::Array->new(
			data => []
		)
	}
}

=head2 highlight_node

Change the currently highlighted node.

=cut

sub highlight_node {
	my $self = shift;
	if(@_) {
		my $prev = delete $self->{highlight_node};
		my $node = $self->{highlight_node} = shift;
		$self->bus->invoke_event(
			highlight_node => $node, $prev
		);
		$self->{move_cursor} = 1;

		if($prev) {
			# If we had a previous item, we'll be wanting to update our
			# position adapter as well to indicate where we are in the
			# tree. Thankfully Tree::DAG_Node makes this relatively easy:
			# find common ancestor, splice new subtree over everything
			# from that ancestor downwards.
			my $ancestor = $prev->common(
				$self->{highlight_node}
			);
			my $node = $self->{highlight_node};
			my @extra = $node;
			while($node != $ancestor) {
				$node = $node->mother;
				unshift @extra, $node;
			}

			# Might be undef, for reasons I can't remember offhand.
			my $depth = $ancestor->ancestors // 0;
			$self->position_adapter->splice(
				0 + $depth,
				1 + ($prev->ancestors - $depth),
				\@extra
			);

			$self->expose_node($prev);
		}

		$self->expose_node($node);
		$self->scroll_to_node($node);
		return $self
	}
	($self->{highlight_node}) = $self->root->daughters unless $self->{highlight_node};
	return $self->{highlight_node};
}

=head2 scroll_to_node

Applies minimal adjustment to visible nodes to ensure that the given
node is within the window area.

=cut

sub scroll_to_node {
	my ($self, $node) = @_;

	# Tree::DAG_Node
	if($node->is_before($self->top_node)) {
		# We have to scroll up, because the target node is above the current top node
		$log->tracef("Node %s is above our current top %s, must scroll", map $_->to_string, $node, $self->top_node);
		$self->top_node($node);
		$self->window->expose;
		return $self;
	}
	if($node->is_after($self->bottom_node)) {
		# We have to scroll down, because the target node is below the current bottom node
		$log->tracef("Node %s is below our current bottom %s, must scroll", map $_->to_string, $node, $self->bottom_node);
		$self->bottom_node($node);
		$self->window->expose;
		return $self;
	}

	$self
}

sub top_node {
	my $self = shift;

	if(@_) {
		$self->{top_node} = shift;
		$self->update_visible_nodes_from_top if $self->window;
		return $self;
	}
	unless($self->{top_node}) {
		($self->{top_node}) = $self->root->daughters;
		$self->update_visible_nodes_from_top if $self->window;
	}
	return $self->{top_node};
}

sub bottom_node {
	my $self = shift;

	if(@_) {
		$self->{bottom_node} = shift;
		$self->update_visible_nodes_from_bottom if $self->window;
		return $self;
	}
	unless($self->{bottom_node}) {
		($self->{bottom_node}) = $self->root->daughters;
		$self->update_visible_nodes_from_bottom if $self->window;
	}
	return $self->{bottom_node};
}

sub update_visible_nodes_from_bottom {
	my ($self) = @_;
	# Walk down to the target node. Once we reach the target, we can determine
	# the top node by retracing lines so that target is within the visible area.
	# Note that this is a no-op if the target is already visible.

	my $win = $self->window or return $self;
	my $lines = $win->lines;
	my $node = $self->{bottom_node};
	while(--$lines) {
		$log->tracef("Lines %d, this node %s", $lines, $node->to_string);
		$node = $node->prev;
		$node->lines($lines);
		last if $node->is_root;
	}
	$log->tracef("Top node %s with %d lines remaining", $node->to_string, $lines);
	if($node->is_root) {
		($self->{top_node}) = $node->daughters;
	} else {
		$self->{top_node} = $node;
		$node->mother->start_offset($node->my_daughter_index)
	}
	$self
}

sub update_visible_nodes_from_top {
	my ($self) = @_;
	# Walk down to the target node. Once we reach the target, we can determine
	# the top node by retracing lines so that target is within the visible area.
	# Note that this is a no-op if the target is already visible.

	my $win = $self->window or return $self;
	my $lines = $win->lines;
	my $node = $self->{top_node};
	$node->mother->start_offset($node->my_daughter_index) unless $node->is_root;
	while(--$lines) {
		$node = $node->next or last;
		$node->lines($lines);
		last if $node->is_root;
	}
	$node ||= $self->root;
	$log->tracef("Bottom node %s with %d lines remaining", $node->to_string, $lines);
	if($node->is_root) {
		($self->{bottom_node}) = $node->daughters;
	} else {
		$self->{bottom_node} = $node;
	}
	$self
}

=head2 cols

Widget columns.

=cut

sub cols {
	my $self = shift;
	$self->calculate_size unless exists $self->{cols};
	return $self->{cols};
}

=head2 lines

Widget lines.

=cut

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
	$self->{lines} = $h + 1;
	$self->{cols} = $w;
	return $self;
}

=head2 window_gained

Work out our size, when we have a window to fit in.

=cut

sub window_gained {
	my $self = shift;
	$self->calculate_size;
	$self->window->cursor_visible(0);
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

=head2 expose_node

Cause a redraw for the given node.

=cut

sub expose_node {
	my ($self, $node) = @_;
	$self->window->expose
}

=head2 render_scrollbar

Render the scrollbar.

=cut

sub render_vertical_scrollbar {
	my ($self, $rb, $rect) = @_;
	return $self unless my $win = $self->window;

	# Trim to far-right edge
	$rect = $rect->intersect(
		Tickit::Rect->new(
			top    => $rect->top,
			bottom => $rect->bottom,
			left   => $rect->right,
			right  => $rect->right,
		)
	) or return $self;

	my $cols = 1;
	my $h = $win->lines;

	# Need to clear any line content first, since we may be overwriting part of
	# the previous scrollbar rendering here
	$rb->eraserect($rect);
	if(my ($min, $max) = $self->scroll_rows) {
		# Scrollbar should be shown, since we don't have all rows visible on the screen at once
		$rb->vline_at($self->header_lines, $min - 1, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH) if $min > 1;
		$rb->vline_at($min, $max, $cols, LINE_DOUBLE, $self->get_style_pen('scroll'), CAP_BOTH);
		$rb->vline_at($max + 1, $h, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH) if $max < $h;
	} else {
		# Placeholder scrollbar - just render it as empty
		$rb->vline_at($self->header_lines, $h, $cols, LINE_SINGLE, $self->get_style_pen('scrollbar'), CAP_BOTH);
	}
}

=head2 sb_height

Current scrollbar height.

=cut

sub sb_height {
	my $self = shift;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return 1 unless $max;
	return floor(0.5 + ($ext * $ext / $max));
}

=head2 scroll_rows

Positions of the scrollbar indicator.

=cut

sub scroll_rows {
	my $self = shift;
	my $cur = $self->scroll_position;
	my $ext = $self->scroll_dimension;
	my $max = $self->row_count - $ext;
	return unless $max;
	my $y = floor(0.5 + ($cur * ($ext - $self->sb_height) / $max));
	return $y, $y + $self->sb_height;
}

=head2 active_scrollbar_rect

Rectangle representing the area covered by the current scrollbar.

=cut

sub active_scrollbar_rect {
	my $self = shift;
	return unless my ($start, $end) = $self->scroll_rows;
	Tickit::Rect->new(
		top    =>$start,
		bottom => 2 + $end,
		left   => $self->window->cols - 1,
		cols   => 1,
	);
}

=head2 scroll_dimension

Size of the vertical scrollbar.

=cut

sub scroll_dimension {
	my $self = shift;
	return 1 unless my $win = $self->window;
	$win->lines;
}

sub render_to_rb {
	my $self = shift;
	my ($rb, $rect) = @_;
	my $win = $self->window;
	$rb->clip($rect);

	my $erase_pen = $self->get_style_pen;
	my $regular_label_pen = $self->get_style_pen('label');
	my $highlight_pen = $self->get_style_pen('highlight');
	my $line_pen = $self->get_style_pen;
	my $toggle_pen = $self->get_style_pen('toggle');

	my $highlight_node = $self->highlight_node;

	$log->debugf("Rendering all lines from %s", "$rect");
	my @labels;
	my %vert;
	my %line;
	my $line = $self->iterate_nodes(
		$self->top_node,
		sub {
			my ($node, $line, $depth) = @_;
			$log->tracef("Looking at %s for %d depth %d", $node->to_string, $line, $depth);
			$line{$node} = $line unless $node->is_root;
			return 1 unless $line >= $rect->top;
			return 0 if $line > $rect->bottom;

			# Since we may be rendering the label from other methods, let's just start
			# by erasing everything so we don't have to chase around trying to work out
			# who drew what where when why and how.
			$rb->erase_at($line, $rect->left, $rect->cols, $regular_label_pen);

			# If we've run out of nodes, erase the rest of the rendering area and bail out early
			unless($node && !$node->is_root) {
				$rb->eraserect(
					Tickit::Rect->new(
						top    => $line,
						left   => $rect->left,
						right  => $rect->right,
						bottom => $rect->bottom,
					),
					$erase_pen
				);
				return 0
			}
			$vert{$node->mother} = $node->mother;#  unless $node->mother->is_root;

			push @labels, [
				$line,
				1 + 3 * $depth,
				$node
			];

			$rb->hline_at(
				$line,
				1 + 3 * ($depth - 1),
				(3 * $depth) - 1, # ($has_children ? 1 : 0),
				LINE_SINGLE,
				$line_pen,
			) if $depth;

			$rb->char_at(
				$line,
				2 + 3 * ($depth - 1),
				$node->is_open ? 0x229F : 0x229E,
				$toggle_pen
			);
			Scalar::Util::weaken($self->{toggle}{join ',', $line, 2 + 3 * ($depth - 1)} = $node);

			return 1;
		}
	);
	$rb->eraserect(
		Tickit::Rect->new(
			top    => $line,
			bottom => $rect->bottom,
			left   => $rect->left,
			right  => $rect->right,
		)
	);

	for my $node (values %vert) {
		my $last = ($node->daughters)[-1];
		$rb->vline_at(
			$line{$node} // ($rect->top - 1),
			$line{$last} // $rect->bottom,
			1 + 3 * ($last->depth - 1),
			LINE_SINGLE,
			$line_pen,
		);
	}
	# $rb->vline_at(-1, $line{$self->{root}}, 1, LINE_SINGLE, $line_pen) if exists $line{$self->{root}};

	# Render our cached label information
	for (@labels) {
		my ($line, $col, $node) = @$_;
		$rb->text_at(
			$line,
			$col,
			$node->name,
			($highlight_node == $node)
			? $highlight_pen
			: $regular_label_pen
		);
	}
}

sub iterate_nodes {
	my ($self, $node, $code) = @_;
	my $depth = $node->depth;
	my $line = 0;
	while($code->($node, $line++, $depth)) {
		# Tree::DAG_Node
		# Iterate into node if we're open, but only if there's any nodes under it
		$node = $node->next or return $line;
		$depth = $node->depth;
	}
	return $line;
}

=head2 render_to_rb

Render method. Used internally.

=cut

sub render_to_rb_old {
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
	my $regular_label_pen = $self->get_style_pen('label');
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
		if($node->is_root) {
			# Bring the initial line down from the top of the window, so we don't start with
			# an isolated line segment
			$rb->vline_at(-1, 0, 1, LINE_SINGLE, $line_pen);
		} else {
			# We only need to draw this if we're inside the rendering area
			if($start_y >= $top) {
				$rb->hline_at(
					$start_y,
					1 + 3 * ($depth - 1),
					(3 * $depth) - 1, # ($has_children ? 1 : 0),
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
				$win->cursor_at($start_y - $y_offset, 2 + (2 + 3 * ($depth - 1)) - $x_offset) if ($highlight_node == $node) && delete $self->{move_cursor};
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

=head1 METHODS - Input handling

These methods are related to input handling (keyboard, mouse).

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
			if($ev->mod_is_ctrl) {
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
	1
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
	1
}

=head2 key_previous_row

Go up a node.

=cut

sub key_previous_row {
	my $self = shift;
	my $node = $self->highlight_node->prev;

	# if we've gone past the start, we're at the top
	($node) = $node->daughters if $node->is_root;
	$self->highlight_node($node);
	$self->redraw;
	1
}

=head2 key_previous_page

Go up a node.

=cut

sub key_previous_page {
	my $self = shift;
	my $win = $self->window or return 1;
	my $node = $self->highlight_node;
	$node = $node->prev || $node for 2..$self->window->lines;

	# if we've gone past the start, we're at the top
	($node) = $node->daughters if $node->is_root;
	$self->highlight_node($node);
	$self->redraw;
	1
}

=head2 key_next_row

Move down a node.

=cut

sub key_next_row {
	my $self = shift;
	my $node = $self->highlight_node->next or return 1;

	# if we've gone past the start, we're already at the bottom so we don't
	# do anything - just bail out here
	return 1 if $node->is_root;

	$self->highlight_node($node);
	$self->redraw;
	1
}

=head2 key_next_page

Move down a node.

=cut

sub key_next_page {
	my $self = shift;
	my $win = $self->window or return 1;
	my $node = $self->highlight_node;
	$node = $node->next || $node for 2..$win->lines;

	# if we've gone past the start, we're already at the bottom so we don't
	# do anything - just bail out here
	return 1 if $node->is_root;

	$self->highlight_node($node);
	$self->redraw;
	1
}

=head2 key_up_tree

Going "up" the tree means the parent of the current node.

=cut

sub key_up_tree {
	my $self = shift;
	my $node = $self->highlight_node;
	return 1 if $node->is_root || $node->mother->is_root;
	$self->highlight_node($node->mother);
	$self->redraw;
	1
}

=head2 key_down_tree

Going "down" the tree means the first child node, if we have one
and we're open.

=cut

sub key_down_tree {
	my $self = shift;
	my $node = $self->highlight_node;
	return 1 unless $node->daughters;
	$node->open unless $node->is_open;
	($node) = $node->daughters;
	$self->highlight_node($node);
	1
}

=head2 key_open_node

Open this node.

=cut

sub key_open_node {
	my $self = shift;
	$self->highlight_node->open unless $self->highlight_node->is_open;
	$self->redraw;
	1
}

=head2 key_close_node

Close this node.

=cut

sub key_close_node {
	my $self = shift;
	$self->highlight_node->close if $self->highlight_node->is_open;
	$self->redraw;
	1
}

=head2 key_activate

Call the C<on_activate> coderef if we have it.

=cut

sub key_activate {
	my $self = shift;
	$self->{on_activate}->($self->highlight_node) if $self->{on_activate};
	$self->invoke_event(activate => $self->highlight_node);
	1
}

=head1 METHODS - Legacy

=cut

=head2 subscribe_to_event

Subscribes to an event. Newer code would call this method on L</bus> instead.

=cut

sub subscribe_to_event { shift->bus->subscribe_to_event(@_) }

=head2 unsubscribe_from_event

Unsubscribes from an event. Newer code would call this method on L</bus> instead.

=cut

sub unsubscribe_from_event { shift->bus->unsubscribe_from_event(@_) }

=head2 invoke_event

Invokes an event. Newer code would call this method on L</bus> instead.

=cut

sub invoke_event { shift->bus->invoke_event(@_) }

=head2 adapter_for_node

Calls L<Tickit::Widget::Tree::Node/adapter_for_node>.

=cut

sub adapter_for_node {
	my $self = shift;
	my $node = shift;
	$node->adapter_for_node($self => @_);
}

1;

__END__

=head1 TODO

Plenty of features and bugfixes left on the list, in no particular order:

=over 4

=item * Avoid full redraw when moving highlight or opening/closing nodes

=item * Support nested widgets

=item * Node reordering

=back

=head1 AUTHOR

Tom Molesworth <cpan@perlsite.co.uk>

=head1 LICENSE

Copyright Tom Molesworth 2011-2015. Licensed under the same terms as Perl itself.

