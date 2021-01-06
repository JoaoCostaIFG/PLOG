:-use_module('info.pl').

% EX1

short(Flight) :-
  flight(Flight, _, _, _, Dur, _),
  Dur < 90.

% EX2

shorter(Flight1, Flight2, ShorterFlight) :-
  flight(Flight1, _, _, _, Dur1, _),
  flight(Flight2, _, _, _, Dur2, _),
  shorterAux(Flight1, Flight2, Dur1, Dur2, ShorterFlight).

shorterAux(Flight1, _, Dur1, Dur2, Flight1) :- Dur1 > Dur2.
shorterAux(_, Flight2, Dur1, Dur2, Flight2) :- Dur1 < Dur2.

% EX3

hourPlusMin(StartT, Dur, EndT) :-
  StartMin is StartT mod 100,
  TotMin is StartMin + Dur,
  Mins is TotMin mod 60,
  Hours is TotMin div 60,
  EndT is StartT + 100 * Hours - StartMin + Mins.

arrivalTime(Flight, ArrivalTime) :-
  flight(Flight, _, _, StartT, Dur, _),
  hourPlusMin(StartT, Dur, ArrivalTime).

% EX4

countries(Company, ListOfCountries) :-
  countriesAux(Company, ListOfCountries, []).

countriesAux(Company, [C|ListOfCountries], Visited) :-
  operates(Company, C),
  \+ member(C, Visited), !,
  countriesAux(Company, ListOfCountries, [C|Visited]).
countriesAux(_, [], _).

operates(Company, Country) :-
  flight(_, Origin, _, _, _, Company),
  airport(_, Origin, Country).
operates(Company, Country) :-
  flight(_, _, Origin, _, _, Company),
  airport(_, Origin, Country).

% EX5

pairableFlights :-
  flight(D1, _, Stop, _, _, _),
  arrivalTime(D1, StopT), % get time of arrival for flight 1
  flight(D2, Stop, _, StartT, _, _),
  D1 \= D2,
  DiffT is StartT - StopT, % time window until next flight
  DiffT >= 30, DiffT =< 130, % 30 mins e 1h30mins
  format('~w - ~w \\ ~w', [Stop, D1, D2]), nl,
  fail.
pairableFlights.

% EX6

tripDays(Trip, Time, FlightTimes, Days) :-
  tripDaysAux(Trip, Time, FlightTimes, Days).

tripDaysAux([Country|Trip], Time, [DepTime|FlightTimes], Days) :-
  flight(D1, _, Stop, DepTime, Dur, _),
  DepTime >= Time,
  airport(_, Stop, Country),
  hourPlusMin(DepTime, Dur + 30, NewTime),
  tripDaysAux(Trip, NewTime, FlightTimes, Days).
tripDaysAux([], _, FlightTimes, Days) :-
  % countInversions(FlightTimes, Invs),
  % Days is Invs + 1.
  Days is 0.

countInversions([H1,H2|T], Ret) :-
  H1 < H2,
  countInversions([H2|T], Ret).
countInversions([H1,H2|T], Ret) :-
  countInversions([H2|T], Ret1),
  Ret is Ret1 + 1.

% EX7

:-use_module(library(lists)).

avgFlightLengthFromAirport(Airport, AvgLength) :-
  findall(Dur, flight(_, Airport, _, _, Dur, _), Durs),
  length(Durs, L),
  sumlist(Durs, Tot),
  AvgLength is Tot/L.

% EX8
mostInternational(Companies) :-
  findall(Company, company(Company, _, _, _), Comps),
  mostInternationalAux(Comps, 0, Max),
  findall(Company, (setof(Country, operates(Company, Country), Countries), length(Countries, Max)), Companies).

mostInternationalAux([Company|CompList], CurrMax, Max) :-
  setof(Country, operates(Company, Country), Countries),
  length(Countries, MyMax),
  mostInternationalAuxChoose(CurrMax, MyMax, NewMax), !,
  mostInternationalAux(CompList, NewMax, Max).
mostInternationalAux([Company|CompList], CurrMax, Max) :-
  % setof failed
  mostInternationalAux(CompList, CurrMax, Max).
mostInternationalAux([], CurrMax, CurrMax).

mostInternationalAuxChoose(CurrMax, MyMax, CurrMax) :- CurrMax >= MyMax.
mostInternationalAuxChoose(CurrMax, MyMax, MyMax) :- CurrMax < MyMax.

% EX9
% é preciso remover o cut do make_pairs
