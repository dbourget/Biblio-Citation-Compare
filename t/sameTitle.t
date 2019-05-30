use lib '../lib';
use Biblio::Citation::Compare 'sameTitle';
use Test::More;

my @sameTitleYes = (
  ['Title', 'Title'],
  ['Title', 'Title: '],
  ['A History of Philosophy. Vol. I: Greece and Rome', 'A History of Philosophy. Vol. I : Greece and Rome'],
);

my @sameTitleNo = (
  ['Title', 'Wow'],
  ['A History of Philosophy. Vol. I: Greece and Rome', 'A History of Philosophy. Vol. IV: Descartes to Leibniz'],
  ['Book Review of: "Do We Really Understand Quantum Mechanics?" by Franck Laloë', 'Do We Really Understand Quantum Mechanics?']
);

ok(sameTitle($_->[0], $_->[1]), "$_->[0] == $_->[1]") for @sameTitleYes;
ok(!sameTitle($_->[0], $_->[1]), "$_->[0] != $_->[1]") for @sameTitleNo;
