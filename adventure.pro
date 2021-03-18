% Created by    -  C2C Hampton  Ford
% Documentation -  See README.
% Game          -  NANI SEARCH

main:- nani_search.       % main entry point

nani_search:-
  init_dynamic_facts,     % predicates which are not compiled

  write('NANI SEARCH - A Sample Adventure Game'),nl,
  write('You are an outstanding cadet at the USAF Academy (it''s like Westpoint but better).'),nl,
  write('You wake up with plenty of time before your Software Design GR and'),nl,
  write('realize you''re missing several things you need before you can leave the dorm.'),nl,
  write('It''s cold, and you''re hungry so you must grab some other items as well.'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" to see your commands and their usage.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,
  write('Enjoy the hunt.'),nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
do(drop(X)):-drop(X),!.
do(look):-look,!.
do(sleep):-sleep,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound:-
  here(exit),        
  write('Congratulations, you''re going to make it to the GR on time!'),nl,
  write('Good luck!'),nl,nl.

sleep:-
  write('You are no longer a good cadet.'),nl,
  write('Enjoy your 0 and negative paperwork.'),nl,nl.

% The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to location          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   take item        (ex. take apple)'),nl,
  write('   drop item        (ex. drop apple)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

% Initial facts describing the world.  Rooms and doors do not change,
% so they are compiled.

room('your room').
room('academic room').
room('squad store').
room(hall).
room(exit).

door('your room',hall).
door('academic room',hall).
door('squad store',hall).
door(exit,hall).

connect(X,Y):-
  door(X,Y).
connect(X,Y):-
  door(Y,X).

% These facts are all subject to change during the game, so rather
% than being compiled, they are "asserted" to the listener at
% run time.  This predicate is called when "nanisrch" starts up.

init_dynamic_facts:-
  assertz(location(trash,'academic room')),
  assertz(location(backpack,'academic room')),
  assertz(location(laptop,'academic room')),

  assertz(location(fleece,'your room')),
  assertz(location(gloves,'your room')),

  assertz(location(apple,'squad store')),
  assertz(location('granola bar','squad store')),
  assertz(location(skittles,'squad store')),
  assertz(location(crackers,'squad store')),
  assertz(location(ramen,'squad store')),
  assertz(location(pizza,'squad store')),

  assertz(here('your room')).

nonperishable(trash).
nonperishable(backpack).
nonperishable(laptop).
nonperishable(fleece).
nonperishable(gloves).

edible(apple).
edible('granola bar').
edible(skittles).
edible(crackers).
edible(ramen).
edible(pizza).

%%%%%%%% COMMANDS %%%%%%%%%%%%%%%%%%%%%%%%%%

% goto moves the player from room to room.

goto(Room):-
  can_go(Room),                 % check for legal move
  puzzle(goto(Room)),           % check for special conditions
  moveto(Room),                 % go there and tell the player
  look.
goto(_):- look.

can_go(Room):-                  % if there is a connection it 
  here(Here),                   % is a legal move.
  connect(Here,Room),!.
can_go(Room):-
  respond(['You can''t get to ',Room,' from here']),fail.

moveto(Room):-                  % update the logicbase with the
  retract(here(_)),             % new room
  asserta(here(Room)).

% look lists the things in a room, and the connections

look:-
  here(Here),
  respond(['You are in the ',Here]),
  write('You can see the following things:'),nl,
  list_things(Here),
  write('You can go to the following rooms:'),nl,
  list_connections(Here).

list_things(Place):-
  location(X,Place),
  tab(2),write(X),nl,
  fail.
list_things(_).

list_connections(Place):-
  connect(Place,X),
  tab(2),write(X),nl,
  fail.
list_connections(_).

% look_in allows the player to look inside a thing which might
% contain other things

% take allows the player to take something.  As long as the thing is
% contained in the room it can be taken, even if the adventurer hasn't
% looked in the the container which contains it.  Also the thing
% must not be furniture.

take(Thing):-
  is_here(Thing),
  is_takable(Thing),
  move(Thing,have),
  respond(['You now have the ',Thing]).

is_here(Thing):-
  here(Here),
  contains(Thing,Here),!.          % don't backtrack
is_here(Thing):-
  respond(['There is no ',Thing,' here']),
  fail.

contains(Thing,Here):-             % recursive definition to find
  location(Thing,Here).            % things contained in things etc.
contains(Thing,Here):-
  location(Thing,X),
  contains(X,Here).

is_takable(Thing):-                % you can't take the furniture
  furniture(Thing),
  respond(['You can''t pick up a ',Thing]),
  !,fail.
is_takable(_).                     % not furniture, ok to take

move(Thing,have):-
  retract(location(Thing,_)),      % take it from its old placef
  asserta(have(Thing)).            % and add to your possessions

% drop - allows the player to transfer a possession to a room

drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)).
drop(Thing):-
  respond(['You don''t have the ',Thing]).

% inventory list your possesions

inventory:-
  have(X),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.

% The only special puzzle in Nani Search has to do with going to the
% cellar.  Puzzle is only called from goto for this reason.  Other
% puzzles pertaining to other commands could easily be added.

puzzle(goto(exit)):-
  have(backpack),
  have(laptop),
  have(fleece),
  have(gloves),
  have(apple) ; have('granola bar') ; have(skittles) ; have(crackers) ; have(ramen) ; have(pizza),
  write('Congrats! You can take your GR now!'),n1.
puzzle(goto(exit)):-
  write('You can''t go to your GR yet. You''re still missing things.'),nl,
  !,fail.
puzzle(_).

% respond simplifies writing a mixture of literals and variables
 
respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C):-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a 
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].

tran_verb(take) --> [take].
tran_verb(take) --> [pick,up].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(look_in) --> [look].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [end].
intran_verb(nshelp) --> [help].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'your room') --> [your,room].
noun(go_place,'academic room') --> [academic,room].
noun(go_place,'squad store') --> [squad,store].


noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,'granola bar') --> [granola,bar].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !, 
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until 
  restword(C1,Cs,C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom
readword(C,W,C2) :-         % otherwise
  get0(C1),       
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :- 
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).

% Relationships
relation(R, X, Y) :-
  relations(Rs),
  member(R,Rs),
  Q =.. [R,X,Y],
  call(Q).


add(Name,Gender,Mother,Father,Spouse) :-
  assert(person(Name,Gender,Mother,Father,Spouse)).
add(Name,_,_,_,_) :-
  delete(Name),
  fail.

open(FileName) :-
  consult(FileName).

close :-
  retractall(person(_,_,_,_,_)).

save(FileName) :-
  tell(FileName),
  listing(person),
  told.

ask(Attribute,Value):-
  known(yes,Attribute,Value),       % succeed if we know its true
  !.                                % and dont look any further
ask(Attribute,Value):-
  known(_,Attribute,Value),         % fail if we know its false
  !, fail.

ask(Attribute,_):-
  known(yes,Attribute,_),           % fail if we know its some other value.
  !, fail.                          % the cut in clause #1 ensures that if
                                    % we get here the value is wrong.
ask(A,V):-
  write(A:V),                       % if we get here, we need to ask.
  write('? (yes or no): '),
  read(Y),                          % get the answer
  asserta(known(Y,A,V)),            % remember it so we dont ask again.
  Y = yes.                          % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer. In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,Value,_):-
  known(yes,Attribute,Value),       % succeed if we know
  !.
menuask(Attribute,_,_):-
  known(yes,Attribute,_),           % fail if its some other value
  !, fail.

menuask(Attribute,AskValue,Menu):-
  nl,write('What is the value for '),write(Attribute),write('?'),nl,
  display_menu(Menu),
  write('Enter the number of choice> '),
  read(Num),nl,
  pick_menu(Num,AnswerValue,Menu),
  asserta(known(yes,Attribute,AnswerValue)),
  AskValue = AnswerValue.           % succeed or fail based on answer

display_menu(Menu):-
  disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]):-        % recursively write the head of
  write(N),write(' : '),write(Item),nl, % the list and disp_menu the tail
  NN is N + 1,
  disp_menu(NN,Rest).

pick_menu(N,Val,Menu):-
  integer(N),                       % make sure they gave a number
  pic_menu(1,N,Val,Menu), !.        % start at one
  pick_menu(Val,Val,_).             % if they didn't enter a number, use
                                    % what they entered as the value

pic_menu(_,_,none_of_the_above,[]). % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).      % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]):-
  NextCtr is Ctr + 1,               % try the next one
  pic_menu(NextCtr, N, Val, Rest).