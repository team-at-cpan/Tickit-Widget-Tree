use strict;
use warnings;

use Test::More tests => 3;
use Tickit::Widget::Tree;
use Tickit::Test;

sub check_by_method {
	foreach my $tp (@_) {
		my $w = $tp->[0];
		my $t = $tp->[1];
		isa_ok($w, 'Tickit::Widget::Tree');
		isa_ok($t, 'Tickit::Widget::Tree');
		note "==> " . $w->label . " with " . $t->label;
		foreach my $method ('next', 'prev') {
			can_ok($w, $method);
			is($w->$method, $t, $method . " from " . $w->label . ' is ' . $t->label) or note $w->$method->label;
			($w, $t) = ($t, $w);
		}
		$w->highlight;
		ok($w->is_highlighted, 'highlight ' . $w->label);
		is($w->highlighted->label, $w->label, 'highlight object is ' . $w->highlighted->label);
		note 'move to next';
		$w->highlight_next;
		ok(!$w->is_highlighted, 'no longer highlight ' . $w->label);
		isnt($w->highlighted->label, $w->label, 'highlight object is not ' . $w->highlighted->label);
		ok($t->is_highlighted, 'correctly highlight ' . $t->label);
		is($t->highlighted->label, $t->label, 'highlight object is ' . $t->highlighted->label);
		note 'move to prev';
		$t->highlight_prev;
		ok(!$t->is_highlighted, 'no longer highlight ' . $t->label);
		isnt($t->highlighted->label, $t->label, 'highlight object is not ' . $t->highlighted->label);
		ok($w->is_highlighted, 'correctly highlight ' . $w->label);
		is($w->highlighted->label, $w->label, 'highlight object is ' . $w->highlighted->label);
	}
}

# single creation since we appear to leave terminal in a bad state when calling this twice in one script?
my ($term, $win) = mk_term_and_window;
subtest 'Single level' => sub {
	plan tests => 48;
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
		[$widget,   $child[0]],
		[$child[0], $child[1]],
		[$child[1], $child[2]],
	);
	flush_tickit;
	done_testing;
};

subtest 'Two levels' => sub {
	plan tests => 144;
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
		[$widget,      $child[0],    ],
		[$child[0],    $subchild[0][0], ],
		[$subchild[0][0], $subchild[0][1], ],
		[$subchild[0][1], $child[1],    ],
		[$child[1],    $subchild[1][0], ],
		[$subchild[1][0], $subchild[1][1], ],
		[$subchild[1][1], $child[2],    ],
		[$child[2],    $subchild[2][0], ],
		[$subchild[2][0], $subchild[2][1], ],
	);
	flush_tickit;
	done_testing;
};

subtest 'Three levels' => sub {
	plan tests => 432;
	# Root
	my $widget = Tickit::Widget::Tree->new(
		label => 'Root'
	);
	# First level
	my @child = map Tickit::Widget::Tree->new(
		label => 'child ' . $_,
	), 0..2;
	$widget->add($_) for @child;

	# Second level
	my @subchild = map +[ map Tickit::Widget::Tree->new(
		label => 'sub child ' . $_,
	), 0..1 ], @child;
	foreach my $idx (0..$#child) {
		$child[$idx]->add($_) for @{$subchild[$idx]};
	}
	my @subsubchild = map +[
		map +[ map Tickit::Widget::Tree->new(
			label => 'sub sub child ' . $_,
		), 0..2 ], @$_
	], @subchild;
	foreach my $c_idx (0..$#subchild) {
		my $c = $subchild[$c_idx];
		foreach my $idx (0..$#$c) {
			$c->[$idx]->add($_) for @{$subsubchild[$c_idx][$idx]};
		}
	}
	for my $c ($widget->children) {
		note $c->label . ' - ' . (eval { $c->prev_sibling->label } // 'undef') . ', ' . (eval { $c->next_sibling->label } // 'undef');
		for my $sc ($c->children) {
			note " " . $sc->label . ' - ' . (eval { $sc->prev_sibling->label } // 'undef') . ', ' . (eval { $sc->next_sibling->label } // 'undef');
			for my $ssc ($sc->children) {
				note "  " . $ssc->label . ' - ' . (eval { $ssc->prev_sibling->label } // 'undef') . ', ' . (eval { $ssc->next_sibling->label } // 'undef');
			}
		}
	}

	$widget->set_window($win);
	flush_tickit;
	check_by_method(
		[$widget,               $child[0] ],
		[$child[0],             $subchild[0][0] ],
		[$subchild[0][0],       $subsubchild[0][0][0] ],
		[$subsubchild[0][0][0], $subsubchild[0][0][1] ],
		[$subsubchild[0][0][1], $subsubchild[0][0][2] ],
		[$subsubchild[0][0][2], $subchild[0][1] ],
		[$subchild[0][1],       $subsubchild[0][1][0] ],
		[$subsubchild[0][1][0], $subsubchild[0][1][1] ],
		[$subsubchild[0][1][1], $subsubchild[0][1][2] ],
		[$subsubchild[0][1][2], $child[1] ],
		[$child[1],             $subchild[1][0] ],
		[$subchild[1][0],       $subsubchild[1][0][0] ],
		[$subsubchild[1][0][0], $subsubchild[1][0][1] ],
		[$subsubchild[1][0][1], $subsubchild[1][0][2] ],
		[$subsubchild[1][0][2], $subchild[1][1] ],
		[$subchild[1][1],       $subsubchild[1][1][0] ],
		[$subsubchild[1][1][0], $subsubchild[1][1][1] ],
		[$subsubchild[1][1][1], $subsubchild[1][1][2] ],
		[$subsubchild[1][1][2], $child[2] ],
		[$child[2],             $subchild[2][0] ],
		[$subchild[2][0],       $subsubchild[2][0][0] ],
		[$subsubchild[2][0][0], $subsubchild[2][0][1] ],
		[$subsubchild[2][0][1], $subsubchild[2][0][2] ],
		[$subsubchild[2][0][2], $subchild[2][1] ],
		[$subchild[2][1],       $subsubchild[2][1][0] ],
		[$subsubchild[2][1][0], $subsubchild[2][1][1] ],
		[$subsubchild[2][1][1], $subsubchild[2][1][2] ],
	);
	flush_tickit;
	done_testing;
};

$win->{tickit}->stop;
done_testing;

