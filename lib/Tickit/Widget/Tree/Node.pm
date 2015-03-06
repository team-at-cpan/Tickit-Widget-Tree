package Tickit::Widget::Tree::Node;

use strict;
use warnings;

use parent qw(Tree::DAG_Node);

#use overload
#	'""' => 'to_string',
#	bool => sub { 1 },
#	fallback => 1;

=head1 NAME

Tickit::Widget::Tree::Node - subclass of L<Tree::DAG_Node> for L<Tickit::Widget::Tree>

=cut

use Log::Any qw($log);
use List::Util qw(min max);
use Variable::Disposition qw(retain_future);
use Future::Utils qw(repeat);
use List::UtilsBy qw(sort_by);

sub new {
	my ($class, $args) = @_;
	my $self = $class->SUPER::new($args);
	Scalar::Util::weaken($self->attributes->{tree});
	$self
}

=head2 recurse:method

Recurse through all child nodes.

=cut

sub recurse:method {
	my ($self, $code) = @_;
	$self->walk_down({
		callback => $code
	});
	$self
}

=head2 open

Opens this node. Will activate the adapter if we have one.

Returns C<$self>.

=cut

sub open {
	$_[0]->attributes->{open} = 1;
	$_[0]->prepare_adapter;
	$_[0]->tree->expose_node($_[0]);
	$_[0]
}

=head2 close

Closes this node. If we have an adapter attached, this will unsubscribe from
notifications.

Returns C<$self>.

=cut

sub close {
	$_[0]->attributes->{open} = 0;
	$_[0]->clear_old_adapter;
	$_[0]->tree->expose_node($_[0]);
	$_[0]
}

=head2 is_open

Returns true if open, false if not.

=cut

sub is_open {
	$_[0]->attributes->{open} ? 1 : 0
}

=head2 adapter_for_node

Returns or sets an L<Adapter::Async::OrderedList> for the given node.

This is the primary mechanism for making a node "live" - once it has been
attached to an adapter, the child nodes will update according to events on
the adapter.

 $node = $tree->node;
 $node->adapter_for_node->push([1,2,3]);

=cut

sub adapter_for_node {
	my $self = shift;
	my $tree = shift;
	return $self->attributes->{adapter} if $self->attributes->{adapter} && !@_;

	# We previously had an adapter, and as such may have stashed some event handlers,
	# so detach gracefully before proceeding any further.
	$self->clear_old_adapter;

	# Allow setting an adapter manually - we also allowing clearing an existing adapter.
	# When removing an adapter, we'll return this node to 'static' state.
	if(@_) {
		$self->attributes->{adapter} = shift;
		return undef unless $self->attributes->{adapter};
	}

	$self->attributes->{adapter} //= do {
		my $adapter = Adapter::Async::OrderedList::Array->new(
			# TODO should populate from existing child nodes? meh, probably
			# not - child nodes don't have enough metadata, what if our adapter
			# source is providing full objects rather than text strings?
			data => []
		);
	};

	# transformations probably apply to non-adapter nodes as well but for now we
	# combine adapter+transformation config. TODO improve this
	$self->attributes->{transformation} = shift if @_;

	# Okay, now we have an adapter, we need to subscribe to all the events, applying
	# each change to the tree and requesting a refresh in the process.
	$self->prepare_adapter($tree) if $self->is_open;

	$self->attributes->{adapter}
}

=head2 prepare_adapter

Attaches events and prepopulates node children when a new adapter is added.

=cut

sub prepare_adapter {
	my ($self) = @_;
	my $adapter = $self->attributes->{adapter} or return Future->done;

	# Things could get mighty confusing if we have entries already. Let's not do that.
	$self->clear_daughters;

	return $self->prepare_adapter_list($adapter) if $adapter->isa('Adapter::Async::OrderedList');
	return $self->prepare_adapter_map($adapter) if $adapter->isa('Adapter::Async::UnorderedMap');
	die "unhandled adapter type $adapter";
}

sub prepare_adapter_map {
	my ($self, $adapter) = @_;
	my $tree = $self->tree;
	Scalar::Util::weaken(my $n = $self);
	Scalar::Util::weaken(my $widget = $tree);
	my $lines = $self->lines;
	my $add = sub {
		my ($k, $v) = @_;
		eval {
			if(my $trans = $n->attributes->{transformation}) {
				my $old_key = $k;
				$k = $trans->key( $k, $v, 0, $adapter, $tree);
				$v = $trans->item($k, $v, 0, $adapter, $tree);
			}
			my @nodes = $n->daughters;
			my $new = $tree->nodes_from_data($k);
			$new->close;
			# $log->tracef("Had %s after adding %s", $new, $k);
			$new->set_daughters($tree->nodes_from_data($v));
			@nodes = sort_by { $_->name } $new, @nodes;
			splice @nodes, $lines if @nodes > $lines;
			$n->set_daughters(@nodes);
			$widget->redraw; 1
		} or do {
			$log->errorf("Exception on add - $@");
		}
	};
	$self->attributes->{updating} = 1;
	$adapter->bus->subscribe_to_event(
		my @ev = (
			clear => sub {
				$n->set_daughters();
				# FIXME slow
				$widget->redraw;
			},
			set_key => sub {
				my ($ev, $k, $v) = @_;
				return if $self->attributes->{updating};
				return $add->($k => $v) if $n->daughters < $lines;
				return $add->($k => $v) if ($n->daughters)[-1] gt $k;
			},
			delete_key => sub {
				my ($ev, $k, $v) = @_;
				return if $self->attributes->{updating};
				eval {
					my @nodes = $n->daughters;
					List::UtilsBy::extract_by { $_ == $v } @nodes;
					$n->set_daughters(@nodes);
					$widget->redraw; 1
				} or do {
					$log->errorf("Exception on splice - $@");
				}
			},
		)
	);
	push @{$self->attributes->{adapter_events}}, $adapter->bus, @ev;
	my $count = 0;
	retain_future(
		$adapter->each(sub {
			my ($k, $v) = @_;
			if($count < $lines) {
				++$count;
				return $add->($k => $v);
			}
			if(($n->daughters)[-1] gt $k) {
				return $add->($k => $v);
			}
		})->on_ready(sub {
			$self->attributes->{updating} = 0;
		})
	);
}

sub prepare_adapter_list {
	my ($self, $adapter) = @_;
	my $tree = $self->tree;
	my $lines = $self->lines;
	Scalar::Util::weaken(my $n = $self);
	Scalar::Util::weaken(my $widget = $tree);
	$adapter->bus->subscribe_to_event(
		my @ev = (
			clear => sub {
				$n->clear_daughters;
				# FIXME slow
				$tree->expose_node($n);
				$widget->redraw;
			},
			splice => sub {
				my ($ev, $start, $length, $added, $removed) = @_;
				$n->splice_handler($tree, $start, $length, $added);
			},
			move => sub {
				# warn "move!"
				# FIXME uh...
			},
		)
	);
	push @{$self->attributes->{adapter_events}}, $adapter->bus, @ev;
	# Initial population
	$self->fill_missing_nodes;
}

=head2 splice_handler

Manages updates caused by splice events.

=cut

sub splice_handler {
	my ($self, $tree, $start, $length, $items) = @_;
	my $lines = $self->lines;
	my $delta = max $length, scalar(@$items);
	return unless my $int = _intersect(
		$self->start_offset,
		$self->start_offset + $lines,
		$start,
		$start + $delta
	);
	eval {
		return $self->update_pending($start, scalar(@$items), $length) if $self->attributes->{updating};

		my @nodes = $self->daughters;
		# add_item_under_parent
		# $log->debugf("Splice in [%s]", $added);
		my @add = map $tree->nodes_from_data($_), @$items;
		# $log->debugf("* [%s]", $_) for @add;
		splice @nodes, $start, $length, @add;
		splice @nodes, $lines if @nodes > $lines;
		$self->set_daughters(@nodes);
		$self->fill_missing_nodes($lines) if @nodes < $lines;
		# FIXME slow
		$tree->redraw;
		1
	} or do {
		$log->errorf("Exception on splice - $@");
	}
}

=head2 update_pending

Updates the pending list of changes to make to this node.

=cut

sub update_pending {
	my ($self, $start, $added, $removed) = @_;

	# Update in progress? Stash this info and bail out early
	$log->debugf("splice event while updating, deferring");
	my $pending = $self->attributes->{pending_update} ||= {
		idx => $start,
		len => $added,
	};
	$pending->{idx} = min $pending->{idx}, $start;
	$pending->{len} = max $pending->{len}, $removed, $added;
	return;
}

=head2 adapter

Returns the adapter.

=cut

sub adapter { shift->attributes->{adapter} }

=head2 fill_missing_nodes

Populates any missing child nodes from the adapter.

Will mark this node as updating while the missing nodes are added.

=cut

sub fill_missing_nodes {
	my ($self, $lines) = @_;

	# We'll be removing more nodes than we add, which means that items past the
	# end of the list should be re-added.
	my $topup = 0 + $self->daughters;

	# This may take a while, and it's quite possible for other splice events to
	# arrive in the meantime. We cheat and set a flag so we don't apply any other
	# updates until this one is done.
	$self->attributes->{updating} = 1;

	my $needed = $self->start_offset + $lines - $topup;

	# We can't just rely on a single update, since we don't have transactions for
	# adapter updates. This means that we could receive an update while we were
	# waiting for data from the adapter. So, we maintain a single-item list of
	# the pending update area, this will be refreshed on every splice event, and
	# repeatedly request data from the adapter until the response comes in before
	# we've had an intervening splice event, at which point we go back to our normal
	# update mechanism using event triggers only.
	my @pending = [ $topup, $needed ];
	retain_future(
		(repeat {
			my ($subset_start, $subset_count) = @{ $_[0] };
			$log->debugf("Retrieving %d missing elements from %d", $subset_start, $subset_count);
			$self->adapter->range(
				start => $subset_start,
				count => $subset_count,
				on_item => sub {
					my ($idx, $item) = @_;
					$log->debugf("Should add %s at %d", $item, $idx);
					$self->add_daughter(ref($self)->new({name => $item }));
				}
			)->on_done(sub {
				if(my $pending = delete $self->attributes->{pending_update}) {
					push @pending, [
						$pending->{idx},
						$pending->{len},
					]
				}
			})
		} foreach => \@pending)->on_ready(sub {
			delete $self->attributes->{updating}
		})
	)
}

sub clear_old_adapter {
	my ($self) = @_;
	$self->attributes->{adapter_events} ||= [];
	if($self->attributes->{adapter}) {
		my ($bus, @ev) = splice @{$self->attributes->{adapter_events}}, 0;
		$bus->unsubscribe_from_event(@ev) if $bus && @ev;
	}
}

=head2 start_offset

Returns the current offset from the start of the adapter. Used when this node is partially
visible - i.e. the first child nodes are not in the rendered area. If the top of the node
is visible then this will be zero.

=cut

=head2 depth

Tree depth (= number of ancestors) for this node.

=cut

sub depth { 0 + (shift->ancestors // 0) }

=head2 tree

Returns the L<Tickit::Widget::Tree> instance.

=cut

sub tree { shift->attributes->{tree} }

sub lines {
	my $self = shift;
	return $self->{lines} //= ($self->tree->window ? $self->tree->window->lines : 100) unless @_;
	$self->{lines} = shift;
	$self
}

sub start_offset {
	my $self = shift;
	return $self->{start_offset} //= 0;
	$self->{start_offset} = shift;
	$self
}

sub prev {
	my ($self) = @_;

	return $self->mother unless $self->left_sister;

	my $node = $self->left_sister;
	while($node->is_open && $node->daughters) {
		($node) = reverse $node->daughters;
	}
	return $node
}

sub next {
	my ($self) = @_;

	if($self->is_open && $self->daughters) {
		my ($node) = $self->daughters;
		return $node;
	} else {
		# We chase up the tree looking for a suitable 'next' entry - either
		# the next node across from us, or from the parent, etc. We may not
		# be able to find anything - in that case, we'll end up at the root.
		my $node = $self;
		while(!$node->is_root) {
			if($node->right_sister) {
				$node = $node->right_sister;
				last;
			}
			$node = $node->mother;
		}
		return $node;
	}
}

=head2 is_before

Returns true if this node is before the given node in the tree.

=cut

sub is_before {
	my ($self, $node) = @_;
	my @src = reverse $self->ancestors;
	my @dst = reverse $node->ancestors;
	while(@src && @dst && $src[0] == $dst[0]) {
		shift @src;
		shift @dst;
	}
	return 0 unless @src && @dst;

	# We now have first diverging ancestor at start of each array.

	return $src[0]->my_daughter_index < $dst[0]->my_daughter_index;
}

=head2 is_after

Returns true if this node is after the given node in the tree.

=cut

sub is_after {
	my ($self, $node) = @_;
	my @src = reverse $self->ancestors;
	my @dst = reverse $node->ancestors;
	while(@src && @dst && $src[0] == $dst[0]) {
		shift @src;
		shift @dst;
	}
	return 0 unless @src && @dst;

	# We now have first diverging ancestor at start of each array.

	return $src[0]->my_daughter_index > $dst[0]->my_daughter_index;
}

=head2 _intersect

Helper function for determining the intersection between two ranges.

=cut

sub _intersect {
	my $start = shift;
	my $end = shift;
	while(my ($l, $r) = splice @_, 0, 2) {
		$start = max $start, $l;
		$end = min $end, $r;
	}
	return undef if $start > $end;
	[ $start, $end ]
}

sub to_string {
	my ($self) = @_;
	"[" . $self->name . "@" . $self->address . "]";
}

1;

__END__

=head1 AUTHOR

Tom Molesworth <cpan@perlsite.co.uk>

=head1 LICENSE

Copyright Tom Molesworth 2011-2015. Licensed under the same terms as Perl itself.

