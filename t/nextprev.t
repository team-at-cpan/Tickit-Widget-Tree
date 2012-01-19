use strict;
use warnings;

use Test::More tests => 2;
use Tickit::Widget::Tree;
use Tickit::Test;

sub check_by_method {
	foreach my $tp (@_) {
		my $w = $tp->[0];
		my $t = $tp->[1];
		my $method = $tp->[2];
		isa_ok($w, 'Tickit::Widget::Tree');
		isa_ok($t, 'Tickit::Widget::Tree');
		ok($method, 'have a method');
		is($w->$method, $t, $method . " from " . $w->label . ' is ' . $t->label) or note $w->$method->label;
	}
}

# single creation since we appear to leave terminal in a bad state when calling this twice in one script?
my ($term, $win) = mk_term_and_window;
subtest 'Single level' => sub {
	plan tests => 32;
	my $widget = Tickit::Widget::Tree->new(
		label => 'Root'
	);
	my @child = map Tickit::Widget::Tree->new(
		label => 'child ' . $_,
	), 1..3;
	$widget->add($_) for @child;

	note "Root: " . $widget->label;
	for my $c ($widget->children) {
		note $c->label . ' - ' . (eval { $c->prev_sibling->label } // 'undef') . ', ' . (eval { $c->next_sibling->label } // 'undef');
	}

	$widget->set_window($win);
	flush_tickit;
	check_by_method(
		[$widget,   $widget,   'prev'],
		[$widget,   $child[0], 'next'],
		[$child[0], $widget,   'prev'],
		[$child[0], $child[1], 'next'],
		[$child[1], $child[0], 'prev'],
		[$child[1], $child[2], 'next'],
		[$child[2], $child[1], 'prev'],
		[$child[2], $child[2], 'next'],
	);
	flush_tickit;
	done_testing;
};

subtest 'Two levels' => sub {
	plan tests => 80;
	# Root
	my $widget = Tickit::Widget::Tree->new(
		label => 'Root'
	);
	# First level
	my @child = map Tickit::Widget::Tree->new(
		label => 'child ' . $_,
	), 1..3;
	$widget->add($_) for @child;

	# Second level
	my @subchild = map +[ map Tickit::Widget::Tree->new(
		label => 'sub child ' . $_,
	), 1..2 ], @child;
	foreach my $idx (0..$#child) {
		$child[$idx]->add($_) for @{$subchild[$idx]};
	}
	for my $c ($widget->children) {
		note $c->label . ' - ' . (eval { $c->prev_sibling->label } // 'undef') . ', ' . (eval { $c->next_sibling->label } // 'undef');
		for my $sc ($c->children) {
			note " " . $sc->label . ' - ' . (eval { $sc->prev_sibling->label } // 'undef') . ', ' . (eval { $sc->next_sibling->label } // 'undef');
		}
	}
#	@child = map +[ $_->children ], @child;

	$widget->set_window($win);
	flush_tickit;
	check_by_method(
		[$widget,      $widget,      'prev'],
		[$widget,      $child[0],    'next'],
		[$child[0],    $widget,      'prev'],
		[$child[0],    $subchild[0][0], 'next'],
		[$subchild[0][0], $child[0],    'prev'],
		[$subchild[0][0], $subchild[0][1], 'next'],
		[$subchild[0][1], $subchild[0][0], 'prev'],
		[$subchild[0][1], $child[1],    'next'],
		[$child[1],    $subchild[0][1],    'prev'],
		[$child[1],    $subchild[1][0], 'next'],
		[$subchild[1][0], $child[1],    'prev'],
		[$subchild[1][0], $subchild[1][1], 'next'],
		[$subchild[1][1], $subchild[1][0], 'prev'],
		[$subchild[1][1], $child[2],    'next'],
		[$child[2],    $subchild[1][1],    'prev'],
		[$child[2],    $subchild[2][0], 'next'],
		[$subchild[2][0], $child[2],    'prev'],
		[$subchild[2][0], $subchild[2][1], 'next'],
		[$subchild[2][1], $subchild[2][0], 'prev'],
		[$subchild[2][1], $subchild[2][1], 'next'],
	);
	flush_tickit;
	done_testing;
};

$win->{tickit}->stop;
done_testing;

