#!/usr/bin/perl 
use strict;
use warnings;
package Layout;
use parent qw(Tickit::Async);

use Tickit::Widget::VBox;
use Tickit::Widget::Entry;
use Tickit::Widget::Static;
use Tickit::Widget::HBox;
use Tickit::Widget::Tree;

sub new {
	my $class = shift;
	my $self = $class->SUPER::new(@_);
	$self->{current_line} = 0;


# Set up a sub and widget to hold some basic log messages
	my $out;
	my $messages = Tickit::Widget::VBox->new;
	my $report = sub {
		my $msg = shift;
		$messages->remove(0) while $messages->children >= $self->rootwin->lines;
		$messages->add(Tickit::Widget::Static->new(text => $msg, align => 'left', valign => 'top'));
	};

# Top-level holder
	my $holder = $self->{holder} = Tickit::Widget::HBox->new;
	$holder->set_window($self->rootwin); # should really set_root_widget perhaps
	my $tree = Tickit::Widget::Tree->new(
		is_open => 1,
		last => 1,
		label => 'Root',
		line_style => 'single',
	);
	$tree->add(Tickit::Widget::Tree->new(
		parent => $tree,
		is_open => 1,
		last => 0,
		label => 'First',
	));
	$tree->add(Tickit::Widget::Tree->new(
		parent => $tree,
		is_open => 1,
		last => 0,
		label => 'Next entry',
	));
	$tree->add(Tickit::Widget::Tree->new(
		parent => $tree,
		is_open => 0,
		last => 1,
		label => 'First child',
	));

	$holder->add($tree, expand => 0.5);
	$holder->add($messages, expand => 0.5);
	$report->('Added widgets');
	$tree->resized;
	return $self;
}

package MenuLayout;
use utf8;
use IO::Async::Loop;

sub new { bless { }, shift }

sub run {
	my $self = shift;
	$self->{loop} = IO::Async::Loop->new;
	$self->{ui} = Layout->new;
	$self->loop->add($self->ui);
	$self->ui->run;
}

sub loop { shift->{loop} }
sub ui { shift->{ui} }

package main;
MenuLayout->new->run;

