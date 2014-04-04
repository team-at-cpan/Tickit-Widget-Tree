requires 'parent', 0;
requires 'Tree::DAG_Node', '>= 1.22';
requires 'Mixin::Event::Dispatch', '>= 1.001';
requires 'Tickit', '>= 0.37';

on 'test' => sub {
	requires 'Test::More', '>= 0.98';
	requires 'Test::Fatal', '>= 0.010';
	requires 'Test::Refcount', '>= 0.07';
};

