% D7012E Prolog Lab 1
% By Jonathan Brorsson
% state( Room, BrassKey, SteelKey, Package )

invalid( state(_, A, A, A) ) :- 1 = 1.
invalid( state(_, _, _, _) ) :- 1 = 0.

% Pickup BrassKey
move( state( Room, Room, SteelKey, Package ),
        pickup(brasskey, Room),
        state( Room, robot, SteelKey, Package )).

% Pickup SteelKey
move( state( Room, BrassKey, Room, Package ),
        pickup(steelkey, Room),
        state( Room, BrassKey, robot, Package )).

% Pickup Package
move( state( Room, BrassKey, SteelKey, Room ),
        pickup(package, Room),
        state( Room, BrassKey, SteelKey, robot )).

% Move from room 1 to room 2
move(  state( r1, BrassKey, robot, Package ), 
        move( r1, r2),
        state( r2, BrassKey, robot, Package )).

% Move from room 2 to room 1
move(  state( r2, BrassKey, robot, Package ), 
        move( r2, r1),
        state( r1, BrassKey, robot, Package )).

% Move from room 1 to room 3
move(   state( r1, robot, SteelKey, Package ), 
        move( r1, r3),
        state( r3, robot, SteelKey, Package )).

% Move from room 3 to room 1
move(   state( r3, robot, SteelKey, Package ), 
        move( r3, r1),  
        state( r1, robot, SteelKey, Package )).

% Drop BrassKey
move( state( Room, robot, SteelKey, Package ),
        drop(brasskey, Room),
        state( Room, Room, SteelKey, Package )).

% Drop SteelKey
move( state( Room, BrassKey, robot, Package ),
        drop(steelkey, Room),
        state( Room, BrassKey, Room, Package )).

% Drop Package
move( state( Room, BrassKey, SteelKey, robot ),
        drop(package, Room),
        state( Room, BrassKey, SteelKey, Room )).



solveR(state( _ , _ , _ , r2 ), N, [done| []]).

solveR(State, N, [Trace| Trace2]) :-
	N > 0,
    move( State, Trace, NewState),
	\+(invalid(NewState)),
    solveR(NewState, N-1, Trace2).

startState(state(r1, r2, r1, r3)).
start(Trace) :- startState(S), solveR(S, 15, Trace).