:-use_module(library(clpfd)).

% posição e a casa no quadrado
% valor e o numero nessa casa do quadrado
magic_quad(Quad) :-
  Quad = [A, B, C, D, E, F, G, H, I],
  domain(Quad, 1, 9),
  all_distinct(Quad),
  A + B + C #= S,
  D + E + F #= S,
  G + H + I #= S,
  A + D + G #= S,
  B + E + H #= S,
  C + F + I #= S,
  A + E + I #= S,
  C + E + G #= S,
  % retirar simetrias (solucoes iguais)
  A #< C, A #< G, C #< G,
  labeling([], Quad).
