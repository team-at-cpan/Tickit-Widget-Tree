
# This file was automatically generated by Dist::Zilla::Plugin::MakeMaker v5.029.
use strict;
use warnings;

use 5.010001;

use ExtUtils::MakeMaker 6.48;



my %WriteMakefileArgs = (
  "ABSTRACT" => "Terminal tree widget",
  "AUTHOR" => "Tom Molesworth <cpan\@perlsite.co.uk>",
  "CONFIGURE_REQUIRES" => {
    "ExtUtils::MakeMaker" => "6.48"
  },
  "DISTNAME" => "Tickit-Widget-Tree",
  "EXE_FILES" => [],
  "LICENSE" => "perl",
  "MIN_PERL_VERSION" => "5.010001",
  "NAME" => "Tickit::Widget::Tree",
  "PREREQ_PM" => {
    "Adapter::Async" => "0.012",
    "Log::Any" => 0,
    "Mixin::Event::Dispatch" => "1.006",
    "Tickit" => "0.46",
    "Tickit::Widget" => 0,
    "Tree::DAG_Node" => "1.22",
    "parent" => 0
  },
  "TEST_REQUIRES" => {
    "File::Spec" => 0,
    "IO::Handle" => 0,
    "IPC::Open3" => 0,
    "Test::CheckDeps" => "0.010",
    "Test::Fatal" => "0.010",
    "Test::More" => "0.98",
    "Test::Refcount" => "0.07"
  },
  "VERSION" => "0.114",
  "test" => {
    "TESTS" => "t/*.t"
  }
);


my %FallbackPrereqs = (
  "Adapter::Async" => "0.012",
  "ExtUtils::MakeMaker" => "6.48",
  "File::Spec" => 0,
  "IO::Handle" => 0,
  "IPC::Open3" => 0,
  "Log::Any" => 0,
  "Mixin::Event::Dispatch" => "1.006",
  "Test::CheckDeps" => "0.010",
  "Test::Fatal" => "0.010",
  "Test::More" => "0.98",
  "Test::Refcount" => "0.07",
  "Tickit" => "0.46",
  "Tickit::Widget" => 0,
  "Tree::DAG_Node" => "1.22",
  "parent" => 0
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.63_03) } ) {
  delete $WriteMakefileArgs{TEST_REQUIRES};
  delete $WriteMakefileArgs{BUILD_REQUIRES};
  $WriteMakefileArgs{PREREQ_PM} = \%FallbackPrereqs;
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



