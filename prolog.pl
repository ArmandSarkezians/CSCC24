% Prolog defines facts and relations

% These are facts
isANumber(1).
isANumber(2).
isANumber(3).
isANumber(4).
isANumber(5).
isANumber(6).
isANumber(7).
isANumber(8).
isANumber(9).
isANumber(10).

% These are relations

both(X, Y) :- isANumber(X), isANumber(Y).
one(X, Y) :- isANumber(X); isANumber(Y).