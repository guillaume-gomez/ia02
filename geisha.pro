%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Projet IA02
%%
%%	Lefevre Victor / Gomez Guillaume
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% affiche le plateau sous forme de tableau
affiche_plateau([]) :- nl.
affiche_plateau([T|Q]) :-
%
write('---------+---------+---------+---------+'),nl,
	affiche_ligne(T),
	
	affiche_plateau(Q),!.	

%Parcours une ligne de la matrice 4*4
affiche_ligne([A,B,C,D]) :-
    write('| '),
	affiche_element(A),
	affiche_element(B),
	affiche_element(C),
	affiche_element(D),nl,
	write('---------+---------+---------+---------+'),nl.
	
% gestion de l'affichage d'un element
% A premiere valeur d'un tuile
% B seconde valeur de la tuile 
affiche_element([A,B]) :-
	trad(A),
	write(' - '),
	trad(B),
	print('  |  ').

% Permet de sauvegarder l'allure generale de l'affichage du plateau lorsqu'un joueur prend une case 	
affiche_element(E) :-
	print('  '),
	trad(E),
	print('   |  ').

%transforme les chaines de caracteres en lettre	
trad(oiseau) :- print('O').
trad(pluie) :- print('P').
trad(tanzaku) :- print('T').
trad(soleil) :- print('S').
trad(erable) :- print('E').
trad(cerisier) :- print('C').
trad(pin) :- print('P').
trad(iris) :- print('I').
trad(j1) :- print('J1').
trad(j2) :- print('J2').
	
% Genere un plateau
% On charge un tableau de 16 cases contenant des nombre de 1 à 16
% On change les numeros de place dans la matrice
% On convertit les nombres en chaines (Iris,Oiseau).
% Enfin, on transforme le tableau de taille 16 en matrice 4*4

plateau_depart(R) :-
	nl,
	list_init(L),
	aleat(L,T),
	convert_number_to_case(T,Q),
	list_to_plateau(Q,R).

% Itere sur les lignes et les colonnes pour choisir un nombre aléatoirement
aleat(L,[T|Q]) :-
	aleat1(L,T,H),!,
	aleat(H,Q).
	
aleat(_,[]).
	
aleat1(L,R,H) :-
	length(L,Y),
	W is Y + 1,
	random(1,W,X),
	nth(X,L,R),
	delete(L,R,H).

% Prédicat qui permet de generer un tableau de 16 cases	
list_init([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]).

% convertit un tableau en matrice
list_to_plateau([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P],[[A,B,C,D],[E,F,G,H],[I,J,K,L],[M,N,O,P]]).

% convertit un nombre en chaine
convert_number_to_case([T|Q],[R|S]) :-
	case(R,T),!,
	convert_number_to_case(Q,S).
	
convert_number_to_case([],[]).
	

% liste des cases à transformer
case([oiseau,erable],1).
case([oiseau,iris],2).
case([oiseau,pin],3).
case([oiseau,cerisier],4).
case([tanzaku,erable],5).
case([tanzaku,iris],6).
case([tanzaku,pin],7).
case([tanzaku,cerisier],8).
case([pluie,erable],9).
case([pluie,iris],10).
case([pluie,pin],11).
case([pluie,cerisier],12).
case([soleil,erable],13).
case([soleil,iris],14).
case([soleil,pin],15).
case([soleil,cerisier],16).

%renvoit vrai ou faux si le coup passé en parametre est possible ou non
% P plateau en parametre
% [T|Q] La tuile jouée par le joueur precedent
% [J(X,Y)] Coup que l'on souhaite tester 
coup_possible(_,[],[_,(_,_)]).
coup_possible(P,[T,Q],[_,(X,Y)]) :-
	nth(X,P,V),
	nth(Y,V,U),
	match([T,Q],U),!.

%Renvoit la liste des coups possibles
% voir coup_possible2	
coups_possibles(Plateau,[],ListeCoup,Joueur) :-
	findall([Joueur,(X,Y)], coups_possibles2(Plateau,[],[Joueur,(X,Y)]), ListeCoup),!.
	
coups_possibles(Plateau,[T,Q],ListeCoup,Joueur) :-
	findall([Joueur,(X,Y)], coups_possibles2(Plateau,[T,Q],[Joueur,(X,Y)]), ListeCoup).
	
coups_possibles2(_,[],[_,(X,Y)]) :-
	member(X,[1,2,3,4]),
	member(Y,[1,2,3,4]).
	
coups_possibles2(Plateau,[T,Q],[_,(X,Y)]) :-
	nth(X,Plateau,V),
	nth(Y,V,U),
	match([T,Q],U).

% Predicat qui permet de savoir si deux tuiles correspondent ( sur X ou Y)
match([T,_],[T,_]).
match([_,Q],[_,Q]).


%
% P le plateau
% [Joueur,(X,Y)] le coup à réaliser par un joueur(1 ou 2)
% PlateauModif le plateau modifié apres le coup
% TuileModif la tuille jouée
jouer_coup(P,[Joueur,(X,Y)],PlateauModif,TuileModif) :-
	nth(X,P,V),
	nth(Y,V,TuileModif),
	select(TuileModif,V,W),
	insert(Joueur,W,Y,Z),
	select(V,P,A),
	insert(Z,A,X,PlateauModif),!.
	
% Insere un élement dans un tableau
% X element à inserer
% [T|Q] Liste dans laquelle insérer
% P indice auquel insérer
% [T|R]	liste résultat
insert(X,[T|Q],P,[T|R]):- 
	P > 1, !, 
    P1 is P - 1, 
	insert(X,Q,P1,R). 

insert(X, Q, 1, [X|Q]).

% Détermine le joueur suivant
joueur_suivant(j2,j1).
joueur_suivant(j1,j2).




% Verifie que la partie n'est pas terminé		
continuer([_|_],[],_).		

% Predicat qui verifie si la partie est terminé

continuer([T|Q],[I,K],J) :-
	coups_possibles([T|Q],[I,K],L,J),
	L \= [],
	not(diagonale([T|Q],J,_)),
	not(antidiagonale([T|Q],J,_)),
	not(ligne([T|Q],J,_)),
	not(colonne([T|Q],J,_)),
	not(carre([T|Q],J,_)),!.

%continuer([[j1,j1,j1,j1],[j1,j1,j1,j1],[j1,j1,j1,j1],[j1,j1,j1,j1]],j1).	

continuer_heuristique([T|Q],J,X) :-
	diagonale([T|Q],J,X),
	antidiagonale([T|Q],J,X),
	ligne([T|Q],J,X),
	colonne([T|Q],J,X),
	carre([T|Q],J,X),
	print(X).
	
%Detecte si le jeu possede une diagonale
diagonale(P,J,X) :-
	nth(1,P,Q),nth(1,Q,J),
	nth(2,P,R),nth(2,R,J),
	nth(3,P,S),nth(3,S,J),
	nth(4,P,T),nth(4,T,J),
	X is 1.
	
%Detecte si le jeu possède une anti diagonale
antidiagonale(P,J,X) :-
	nth(1,P,Q),nth(4,Q,J),
	nth(2,P,R),nth(3,R,J),
	nth(3,P,S),nth(2,S,J),
	nth(4,P,T),nth(1,T,J),
	X is 1.

%Detecte si le jeu possède une ligne	
ligne([[J,J,J,J],_,_,_],J,1).
ligne([_,[J,J,J,J],_,_],J,2).
ligne([_,_,[J,J,J,J],_],J,3).
ligne([_,_,_,[J,J,J,J]],J,4).

%Detecte si le jeu possède une colonne		
colonne([[J,_,_,_],[J,_,_,_],[J,_,_,_],[J,_,_,_]],J,1).
colonne([[_,J,_,_],[_,J,_,_],[_,J,_,_],[_,J,_,_]],J,2).
colonne([[_,_,J,_],[_,_,J,_],[_,_,J,_],[_,_,J,_]],J,3).
colonne([[_,_,_,J],[_,_,_,J],[_,_,_,J],[_,_,_,J]],J,4).

%Detecte si le jeu possède une colonne		
carre([[J,J,_,_],[J,J,_,_],[_,_,_,_],[_,_,_,_]],J,1).
carre([[_,_,_,_],[J,J,_,_],[J,J,_,_],[_,_,_,_]],J,2).
carre([[_,_,_,_],[_,_,_,_],[J,J,_,_],[J,J,_,_]],J,3).
carre([[_,J,J,_],[_,J,J,_],[_,_,_,_],[_,_,_,_]],J,4).
carre([[_,_,_,_],[_,J,J,_],[_,J,J,_],[_,_,_,_]],J,5).
carre([[_,_,_,_],[_,_,_,_],[_,J,J,_],[_,J,J,_]],J,6).
carre([[_,_,J,J],[_,_,J,J],[_,_,_,_],[_,_,_,_]],J,7).
carre([[_,_,_,_],[_,_,J,J],[_,_,J,J],[_,_,_,_]],J,8).
carre([[_,_,_,_],[_,_,_,_],[_,_,J,J],[_,_,J,J]],J,9).

not(P) :- (call(P) -> fail ; true).

	

% Renvoit le meilleur coup possible (Unfinished)
% P le plateau
% 
% Le coup souhaité du joueur
meilleur_coup(P,[],[J,(X,Y)]) :-
	
	coups_possibles(P,[],L,J),!,
	print(L),
	length(L,Z),
	W is Z + 1,
	random(1,W,N),
	nth(N,L,[J,(X,Y)]).

meilleur_coup(P,[T,Q],[J,(X,Y)]) :-
	coups_possibles(P,[T,Q],L,J),!,
	meilleur_coup2(P,L,[J,(X,Y)],R),
	max1(R,W),
	nth(Z,R,W),
	nth(Z,L,[J,(X,Y)]).
	
meilleur_coup2(_,[],[_,(_,_)],[]).
	
meilleur_coup2(P,[T|Q],[J,(X,Y)],[R|S]) :-
	jouer_coup(P,T,Psuiv,Tsuiv),!,
	joueur_suivant(J,Jsuiv),
	(continuer(Psuiv,Tsuiv,Jsuiv),coups_possibles(Psuiv,Tsuiv,L,Jsuiv) -> 
		meilleur_coup3(Psuiv,L,[Jsuiv,(X,Y)],A),
		min1(A,R),
		meilleur_coup2(P,Q,[J,(X,Y)],S);
		print('egalite'),nl,affiche_plateau(PSuiv),break).
	
meilleur_coup3(_,[],[_,(_,_)],[]).
	
meilleur_coup3(P,[T|Q],[J,(X,Y)],[R|S]) :-
	jouer_coup(P,T,Psuiv,Tsuiv),!,
	evaluer(Psuiv,J,R,_,_),
	meilleur_coup3(P,Q,[J,(X,Y)], S).

% Evalue la plateau en cours
% plateau
% Joueur
% Resultat

% Rempli le plateau pour analyser le nombres de ligne, colonnes, et diagonale qu'on peut faire dans la configuration d'un plateau à l'etat t
remplir_plateau([A,B,C,D],J,Jsuiv,[E,F,G,H]) :-
	remplir_ligne(A,J,Jsuiv,E),
	remplir_ligne(B,J,Jsuiv,F),
	remplir_ligne(C,J,Jsuiv,G),
	remplir_ligne(D,J,Jsuiv,H).
	
remplir_ligne([A,B,C,D],J,Jsuiv,[E,F,G,H]) :-
	remplir_case(A,J,Jsuiv,E),
	remplir_case(B,J,Jsuiv,F),
	remplir_case(C,J,Jsuiv,G),
	remplir_case(D,J,Jsuiv,H).

% Rempli une case du plateau
remplir_case(T,J,Jsuiv,J) :-
	T\=Jsuiv.
remplir_case(T,J,Jsuiv,T).
	
min1([Only], Only).
min1([Head|Tail], Minimum) :-
    min1(Tail, TailMin),
    Minimum is min(Head, TailMin). 
	
max1([Only], Only).
max1([Head|Tail], Maximum) :-
    max1(Tail, TailMax),
    Maximum is max(Head, TailMax). 
	
feuille(_,0).
feuille([],_).

min_max(_,_,_,_,[],[],[]).
min_max(Plateau,DerT,DerC,Prof,[[Joueur,(X,Y)]|ResteCouP],MeCoup,MeVal) :-
	jouer_coup(Plateau,[Joueur,(X,Y)],Nouv_Plateau,NouvTuile),
	Prof2 is Prof - 1,
	joueur_suivant(Joueur,JSuiv),
	meilleur_coupIA(Nouv_Plateau,NouvTuile,[JSuiv,(X,Y)],Prof2,MeCoup1,MeVal1),
	min_max(Plateau,DerT,[Joueur,(X,Y)],Prof,ResteCouP,MeCoup2,MeVal2),
	choisirMinMax(DerC,MeVal1,[Joueur,(X,Y)],MeVal2,MeCoup,MeVal,Prof2).
	
meilleur_coupIA(Plateau,DerT,[Joueur,(X,Y)],Prof,MeCoup,MeVal) :-
	coups_possibles(Plateau,DerT,ListCp,Joueur),
	(feuille(ListCp,Prof) ->
		evaluer(Plateau,Joueur,MeVal,MeCoup,[Joueur,(X,Y)])
	;
		min_max(Plateau,DerT,[],Prof,ListCp,MeCoup,MeVal)
	).

	
choisirMinMax([],_,MeCoup2,_,MeCoup2,_,_).
choisirMinMax(MeCoup1,MeVal1,_,[],MeCoup1,MeVal1,_).
choisirMinMax(_,[],MeCoup2,MeVal2,MeCoup2,MeVal2,_).

choisirMinMax(MeCoup1,MeVal1,MeCoup2,MeVal2,MeCoup,MeVal,Prof):-
	%print(MeCoup1),
	(Prof mod 2 =:= 1 ->
		max(MeCoup1,MeVal1,MeCoup2,MeVal2,MeCoup,MeVal)
	;
		min(MeCoup1,MeVal1,MeCoup2,MeVal2,MeCoup,MeVal)
	).
	
max(X,Y,Z,W,X,Y) :- Y>=W.
max(X,Y,Z,W,Z,W) :- Y<W.
min(X,Y,Z,W,X,Y) :- Y<W.
min(X,Y,Z,W,Z,W) :- Y>=W.

	
	
evaluer([],_,0,T,T).

evaluer(Plateau,Joueur,Resultat,T,T):-
	joueur_suivant(Joueur,Jsuiv),
	remplir_plateau(Plateau,Joueur,Jsuiv,PRempli),
	(findall(X1,diagonale(PRempli,Joueur,X1), L1) ->
		length(L1,Z1);
		Z1 is 0),
	(findall(X2,antidiagonale(PRempli,Joueur,X2), L2) ->
		length(L2,Z2);
		Z2 is 0),
	(findall(X3,ligne(PRempli,Joueur,X3), L3) ->
		length(L3,Z3);
		Z3 is 0),
	(findall(X4,colonne(PRempli,Joueur,X4), L4) ->
		length(L4,Z4);
		Z4 is 0),
	(findall(X5,carre(PRempli,Joueur,X5), L5) ->
		length(L5,Z5);
		Z5 is 0),
	Resultat is Z1 + Z2 + Z3 + Z4 + Z5.
	

%Lancer le jeu entre 2 personnes
% J Joueur
% T Tuile jouée précedemment
% P Plateau
jouerHumain(J,T,P) :-
	print('Joueur '),print(J),nl,
	affiche_plateau(P),
	print('Coup Possible -->'),
	coups_possibles(P,T,ListeCoup,J),
	print(ListeCoup),
	nl,
	print('valeur x'),nl,
	read(X), 
	print('valeur y'),nl,
	read(Y),
	( coup_possible(P,T,[J,(X,Y)]) ->
		true ;
		print('Erreur Veuillez recommencer: '),nl,
		jouerHumain(J,T,P)
	),
	jouer_coup(P,[J,(X,Y)],Psuiv,Tsuiv),
	(continuer(Psuiv,Tsuiv,J) -> 
		joueur_suivant(J,Jsuiv),
		jouerHumain(Jsuiv,Tsuiv,Psuiv) 
	; 
		affiche_plateau(Psuiv),
		print('fin')
	),!.
	
	
%Lancer le jeu IA version 1
%Joueur
%Tuile jouée précedemment
%Plateau	
jouerIA(Joueur,Tuile,Plateau) :-
	print('Joueur '),print(Joueur),nl,
	affiche_plateau(Plateau),
	meilleur_coup(Plateau,Tuile,[Joueur,(X,Y)]),!,
	jouer_coup(Plateau,[Joueur,(X,Y)],Psuiv,Tsuiv),	
	(continuer(Psuiv,Tsuiv,Joueur) -> 
		joueur_suivant(Joueur,Jsuiv),
		jouerIA(Jsuiv,Tsuiv,Psuiv) 
	; 
		affiche_plateau(Psuiv),
		print('fin')
	).
		
		
%Lancer le jeu IA version 2
%Joueur
%Tuile jouée précedemment
%Plateau		
jouerIA2(Joueur,Tuile,Plateau) :-
	print('Joueur '),print(Joueur),nl,
	affiche_plateau(Plateau),
	meilleur_coupIA(Plateau,Tuile,[Joueur,(0,0)],2,[_,(X,Y)],MeVal),!,
	jouer_coup(Plateau,[Joueur,(X,Y)],Psuiv,Tsuiv),	
	(continuer(Psuiv,Tsuiv,Joueur) -> 
		joueur_suivant(Joueur,Jsuiv),
		jouerIA2(Jsuiv,Tsuiv,Psuiv) 
	; 
		affiche_plateau(Psuiv),
		print('fin')
	).
	

%Lancer le jeu contre l'IA
% J Joueur
% T Tuile jouée précedemment
% P Plateau
jouerVS_J(J,T,P) :-
	print('Joueur '),print(J),nl,
	affiche_plateau(P),
	print('Coup Possible -->'),
	coups_possibles(P,T,ListeCoup,J),
	print(ListeCoup),
	nl,
	print('valeur x'),nl,
	read(X), 
	print('valeur y'),nl,
	read(Y),
	( coup_possible(P,T,[J,(X,Y)]) ->
		true ;
		print('Erreur Veuillez recommencer: '),nl,
		jouerVS_J(J,T,P)
	),
	jouer_coup(P,[J,(X,Y)],Psuiv,Tsuiv),
	(continuer(Psuiv,Tsuiv,J) -> 
		joueur_suivant(J,Jsuiv),
		jouerVS_IA(Jsuiv,Tsuiv,Psuiv) ; 
		affiche_plateau(Psuiv),
		print('fin')
	),!.
	
jouerVS_IA(Joueur,Tuile,Plateau) :-
	print('Joueur '),print(Joueur),nl,
	affiche_plateau(Plateau),
	meilleur_coupIA(Plateau,Tuile,[Joueur,(0,0)],2,[_,(X,Y)],MeVal),!,
	jouer_coup(Plateau,[Joueur,(X,Y)],Psuiv,Tsuiv),	
	(continuer(Psuiv,Tsuiv,Joueur) -> 
		joueur_suivant(Joueur,Jsuiv),
		jouerVS_J(Jsuiv,Tsuiv,Psuiv) 
	; 
		affiche_plateau(Psuiv),
		print('fin')
	).
	
	

%Menu Principal
menu :-
print('--------------------------------'),nl,
print('1 : Mode 2 Joueurs'),nl,
print('2 : Mode 1 Joueur'),nl,
print('3 : Mode 2 Joueurs IA version 1'),nl,
print('4 : Mode 2 Joueurs IA version 2'),nl,
print('Entrer votre choix --> '),
read(Choix),
nl,
appel(Choix),Choix = 4,nl,
print('--------------------------------'),nl.

%sous Menu
appel(1):- plateau_depart(P),jouerHumain(j1,[],P),!.
appel(2):- plateau_depart(P),jouerVS_J(j1,[],P),!.
appel(3):- plateau_depart(P),jouerIA(j1,[],P),!.
appel(4):- plateau_depart(P),jouerIA2(j1,[],P),!.
appel(_):- print('Erreur, vous avez mal choisi').
	


