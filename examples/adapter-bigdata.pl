#!/usr/bin/env perl
use strict;
use warnings;
use Tickit::Async;
use Tickit::Widget::Tree;
use Tickit::Widget::HBox;
use Tickit::Widget::LogAny;

# Start with a simple, empty tree
my $tree = Tickit::Widget::Tree::Node->new;
$tree->name('Root');
$tree->open;

# Set up the widget...
my $w = Tickit::Widget::Tree->new(root => $tree);
my $hbox = Tickit::Widget::HBox->new;
$hbox->add($w, expand => 1);
my $logpanel = Tickit::Widget::LogAny->new;
$hbox->add($logpanel, expand => 4);
my $tickit = Tickit::Async->new(
	root => do {
		$w->take_focus;
		$hbox
	}
);

# defer initial adapter population...
$tickit->later(sub {
	$tree->add_daughter(my $ab = Tickit::Widget::Tree::Node->new({name => 'adapterized'}));
	my $adapter = $ab->adapter_for_node($w);
	my $item = 'aaaaa';
	$adapter->push([$item++]) for 1..500;
	my $code;
	Scalar::Util::weaken(my $wt = $tickit);
	$code = sub {
		$adapter->push([$item++])->on_done(
			$adapter->curry::shift
		);
		$wt->timer(
			after => 0.5,
			$code
		);
	};
	$code->();
	$tree->open;
	$ab->open;
});

# then kick things off
$tickit->run;

