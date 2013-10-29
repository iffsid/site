---
Title: Learning Rules through Game Play
category: game-rule-learning
extra_scripts: <link rel="stylesheet" href="/css/syntax.css"/>
---

## Tic-Tac-Toe ##

Initial Rule Specification:

<!-- tic-tac-toe-rules -->
~~~~~~ {.code}
Every cache square for every player in the initial state has some piece of
that player.

A player moves by moving some piece of that player from some cache square for
that player to some empty board square.

A player wins when every square in some row has some piece of that player.
A player wins when every square in some column has some piece of that player.
A player wins when every square in some diagonal has some piece of that
player.

A player draws when no player wins and that player has no move.
~~~~~~

§youtube(GECpF_Mq9yY)§

Learned Rules:

<!-- the syntax is set to haskell because the prolog one doesn't handle -->
<!-- underscores properly-->
<!-- tic-tac-toe-rules.pl -->
~~~~~~ {.haskell .code}
initial_board([[none,none,none],[none,none,none],[none,none,none]], player_x).

legal_move(A,B,C) :- row(D), col(E), owns(A,F), empty(G), at(D,E,B,G,H),
	at(D,E,C,F,I), frame_obj(I,I,H,H,C,B).

outcome(A,B,C) :- opponent(A,D), owns_outcome(D,C), owns_piece(C, E),
	at(F,G,B,E,H), at(I,J,B,E,K), at(L,M,B,E,N), linear_obj(K, H,N).
~~~~~~

§youtube(mx0JSPUl4aU)§

## Hexapawn ##

Initial Rule Specification:

<!-- hexapawn rules -->
~~~~~~ {.code}
Every square in the close row for every player in the initial state has some
piece of that player.

A player moves by moving some piece of that player from some square to some
empty forward-adjacent square for that player of that square.
A player moves by moving some piece of the opponent of that player from some
forward-diagonal square for that player of some square to some cache square
for that opponent then moving the piece of that player from that square to
that forward-diagonal square.

A player wins when some square in the distant row for that player has some
piece of that player.

A player draws when no player wins and that player has no move.
~~~~~~

§youtube(FpOeJ-An6Io)§

Learned Rules:

<!-- the syntax is set to haskell because the prolog one doesn't handle -->
<!-- underscores properly-->
<!-- hexapawn-rules.pl -->
~~~~~~ {.haskell .code}
initial_board([[x,x,x],[none,none,none],[o,o,o]],player_x).

legal_move(A,B,C) :- row(D), col(E), owns(A,F), empty(G), forward(A,H,D),
	at(H,E,B,F,I), at(H,E,C,G,J), at(D,E,B,G,K), at(D,E,C,F,L),
	frame_obj(I,K,J,L,B,C).
legal_move(A,B,C) :- row(D), col(E), opponent(A,F), owns(A,G), empty(H),
	forward(F,D,I), owns(F,J), sideways(E,K), at(I,K,B,G,L), at(I,K,C,H,M),
	at(D,E,C,G,N), at(D,E,B,J,O), frame_obj(L,O,M,N,B,C).

outcome(A,B,C) :- row(D), opponent(A,E), forward(E,D,F), forward(E,F,G),
	owns_outcome(E,C), owns_piece(C,H), at(G,I,B,H,J).
outcome(A,B,C) :- opponent(A,D), has_no_move(A,B), owns_outcome(D,C).
~~~~~~

§youtube(TC7kQsCmELE)§
