use strict;
use warnings;

use Test::More;
use Test::Deep;
use Tree::DAG_Node;

use Adapter::Async::OrderedList::Array;
use Adapter::Async::UnorderedMap::Hash;
use Variable::Disposition qw(retain_future);
use Future::Utils qw(repeat);

sub tree(&;$) {
	my $code = shift;
	my %args = $code->();
}

{ # Flat list that fills visible area
	my $tree = tree {
		height => 10,
		data => [ 1..10 ],
	};
}

use List::Util qw(min max);
{
	my $root = Tree::DAG_Node->new;
	my $adapter = Adapter::Async::OrderedList::Array->new;
	my $intersect = sub {
		my $start = shift;
		my $end = shift;
		note "Intersect check between $start and $end:";
		while(my ($l, $r) = splice @_, 0, 2) {
			$start = max $start, $l;
			$end = min $end, $r;
			note "* after $l, $r have $start and $end:";
		}
		note "final was $start and $end:";
		return undef if $start > $end;
		[ $start, $end ]
	};
	my $build = sub {
		my ($adapter, $count) = @_;
		$root->attributes->{adapter} = $adapter;

		my $start = 4;
		my $end = $start + $count - 1;
		my @data;
		$adapter->range(
			start => $start,
			count => $count,
			on_item => sub {
				my ($idx, $item) = @_;
				push @data, $item;
			}
		)->then(sub {
			$adapter->bus->subscribe_to_event(
				splice => sub {
					my ($ev, $idx, $len, $data, $removed) = @_;
					note "$idx, $len => " . @$data;
					my $delta = max $len, scalar(@$data);
					return unless my $int = $intersect->($start, $start + $count, $idx, $idx + $delta);
					if($root->attributes->{updating}) {
						# Update in progress? Stash this info and bail out early
						my $pending = $root->attributes->{pending_update} ||= {
							idx => $idx,
							len => $delta
						};
						$pending->{idx} = min $pending->{idx}, $idx;
						$pending->{len} = max $pending->{len}, $len, scalar(@$data);
						return;
					}
					eval {
						my @child = $root->daughters;
						splice @child, $idx, $len, map Tree::DAG_Node->new({name => $_ }), @$data;
						splice @child, $count if @child > $count;
						$root->set_daughters(@child);
						if(@child < $count) {
							# We'll be removing more nodes than we add, which means that items past the
							# end of the list should be re-added.
							my $topup = @child;

							# This may take a while, and it's quite possible for other splice events to
							# arrive in the meantime. We cheat and set a flag so we don't apply any other
							# updates until this one is done.
							$root->attributes->{updating} = 1;

							my $needed = 1 + $end - $topup;
							my @pending = [ $topup, $needed ];
							retain_future(
								(repeat {
									my ($subset_start, $subset_count) = @{ $_[0] };
									note "Retrieving missing elements from $subset_start for $subset_count";
									$adapter->range(
										start => $subset_start,
										count => $subset_count,
										on_item => sub {
											my ($idx, $item) = @_;
											note "Should add $item at $idx";
											$root->add_daughter(Tree::DAG_Node->new({name => $item }));
										}
									)->on_done(sub {
										if(my $pending = delete $root->attributes->{pending_update}) {
											push @pending, [
												$pending->{idx},
												$pending->{len},
											]
										}
									})
								} foreach => \@pending)->on_ready(sub {
									delete $root->attributes->{updating}
								})
							)
						}
						1
					} or note "error => $@";
				}
			);
			Future->done;
		})->get;
		\@data
	};
	$build->($adapter, 10);
	my $render = sub {
		[ map $_->name, $root->daughters ]
	};

	cmp_deeply($render->(), [ ], 'empty');
	$adapter->push(['x'])->get;
	cmp_deeply($render->(), ['x'], 'single element');
	$adapter->push([1..10])->get;
	cmp_deeply($render->(), ['x', 1..9], 'overflow by one');
	$adapter->splice(2, 1, [])->get;
	cmp_deeply($render->(), ['x', 1, 3..10], 'remove element');
	$adapter->push(['y'])->get;
	cmp_deeply($render->(), ['x', 1, 3..10], 'push element past end of list');
	$adapter->shift->get;
	cmp_deeply($render->(), [1, 3..10, 'y'], 'remove element');
}

done_testing;

