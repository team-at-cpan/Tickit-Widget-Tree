use strict;
use warnings;

use Test::More;
use Test::Deep;
use Tree::DAG_Node;

use Adapter::Async::OrderedList::Array;
use Adapter::Async::UnorderedMap::Hash;

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

{
	my $root = Tree::DAG_Node->new;
	my $adapter = Adapter::Async::OrderedList::Array->new;
	$root->attributes->{adapter} = $adapter;
	my $render = sub {
		my $count = shift;
		my @data = @{ $adapter->all->get };
		splice @data, $count if @data > $count;
		\@data
	};

	cmp_deeply($render->(10), [ ], 'empty');
	$adapter->push(['x']);
	cmp_deeply($render->(10), ['x'], 'single element');
	$adapter->push([1..10]);
	cmp_deeply($render->(10), ['x', 1..9], 'overflow by one');
}

done_testing;

