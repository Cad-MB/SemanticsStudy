/* quelques morceaux de code inspiré de (github.com/AdelTechCrafter/APS) --- /*! pas de copie coller car ils ont travaillé différemment */ */
chercher(X, [(X,V)|_], V).
chercher(X, [_|XS], V) :- chercher(X, XS, V).

/* ChatGPT nous a aidé à faire juste (get_type/recupererTypeArgs/check_args) -- le reste, nous l'avons fait */
get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	type_expr([],A,T),
	get_type(ARGS,TYPES).

recupererTypeArgs([],[]).
recupererTypeArgs([(_,T)|ARGS],[T|RES]) :-
	recupererTypeArgs(ARGS,RES).
		
check_args(_,[],[]).
check_args(G,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	type_expr(G,ARG,ARGTYPE),
	check_args(G,ARGS,ARGSTYPE).


/* Contexte initial: G0 */
g0([
    (true, bool),
    (false, bool),
    (not, funType([bool], bool)),
    (eq, funType([int* int], bool)),
    (lt, funType([int* int], bool)),
    (add, funType([int* int], int)),
    (sub, funType([int* int], int)),
    (mul, funType([int* int], int)),
    (div, funType([int* int], int))
]).

/* Programme */
    /* (PROG) */
type_prog(G, prog(X), void):-
    type_cmds(G, X, void).

/* Suite de commandes */
    /* (DEFS) */
type_cmds(G,[cmds(X)|Y],void) :-
	type_def(G,X,G2),
	type_cmds(G2,Y,void).
    /* (END)  */
type_cmds(_,[],void).
type_cmds(G,[X|Y],void) :-
	type_stat(G,X,void),
	type_cmds(G,Y,void).

/* Definitions */
    /* (CONST) */
type_def(G,const(X,T,E),[(X,T)|G]):-
	type_expr(G,E,T).	
    /* (FUN) */
        /* La règle `type_def` définit le type d'une fonction dans un environnement de types G. */
        /* Elle fusionne les arguments avec G, détermine le type de l'expression de la fonction, */
        /* extrait les types des arguments, puis met à jour l'environnement avec le nom de la fonction et son type.*/
type_def(G,fun(FUN,T,ARGS,E),GI):-
	append(ARGS,G,G2), 
	type_expr(G2,E,T),
	recupererTypeArgs(ARGS,RES),
	GI=[(FUN,funcType(RES,T))|G]. 
    /* (FUNREC) */
        /* La règle `type_def` définit le type d'une fonction récursive dans un environnement de types G. */
        /* Elle extrait d'abord les types des arguments, puis fusionne les arguments avec G. */
        /* Ensuite, elle met à jour l'environnement temporaire avec le type de la fonction, */
        /* analyse le type du corps de la fonction, */
        /* et enfin met à jour l'environnement final avec le nom de la fonction et son type.*/
type_def(G,funRec(FUN,ARGS,T,E),GI):-
	recupererTypeArgs(ARGS,RES),
	append(ARGS,G,G2), 
    G3 = [(ID,funcType(RES,T))|G2],
	type_expr(G3,E,T),
	GI=[(FUN,funcType(RES,T))|G]. 

/* Intruction */
    /* (ECHO) */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).	

/* Expressions */
    /* (NUM) */
type_expr(_,num(N),int) :-
 	integer(N).
    /* (ID) */
type_expr(G,id(X),T):-
    chercher(X,G,T).
    /* (IF)  */
type_expr(G, if(E1, E2, E3), T):-
    type_expr(G, E1, bool),
    type_expr(G, E2, T),
    type_expr(G, E3, T).
    /* (AND) */
type_expr(G, and(E1, E2), bool):-
    type_expr(G, E1, bool),
    type_expr(G, E2, bool).
    /* (OR)  */
type_expr(G, or(E1, E2), bool):-
    type_expr(G, E1, bool),
    type_expr(G, E2, bool).
    /* (APP) */
type_expr(G,app(E1,ARGS), T):- 
    type_expr(G,E1, funcType(ARGSTYPE, T)),
    check_args(G,ARGS,ARGSTYPE).
    /* (ABS) */
type_expr(G,lambda(ARGS,E),T):- 
    append(ARGS,G,GI), 
    chercher(GI,E,T).
