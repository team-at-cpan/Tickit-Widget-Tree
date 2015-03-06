requires 'parent', 0;
requires 'Adapter::Async', '>= 0.012';
requires 'Tree::DAG_Node', '>= 1.22';
requires 'Mixin::Event::Dispatch', '>= 1.006';
requires 'Tickit', '>= 0.48';
requires 'Tickit::Widget', 0;
requires 'Log::Any', 0;
requires 'List::UtilsBy', 0;
requires 'Future', '>= 0.30';
requires 'Variable::Disposition', '>= 0.003';

on 'test' => sub {
	requires 'Test::More', '>= 0.98';
	requires 'Test::Fatal', '>= 0.010';
	requires 'Test::Refcount', '>= 0.07';
};

