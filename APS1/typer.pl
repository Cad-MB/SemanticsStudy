/* quelques morceaux de code inspiré de (github.com/AdelTechCrafter/APS) --- /*! pas de copie coller car ils ont travaillé différemment */ */
chercher(X,[(X,V)|_],V).
chercher(X,[_|XS],V):- 
    chercher(X,XS,V).

/* ChatGPT nous a aidé à faire juste (get_type/recupererTypeArgs/check_args) -- le reste,nous l'avons fait */
get_type([],[]).
get_type([A|ARGUMENTS],[T|TYPES]):-
	type_expr([],A,T),
	get_type(ARGUMENTS,TYPES).

recupererTypeArgs([],[]).
recupererTypeArgs([(_,T)|ARGUMENTS],[T|TYPEIS]):-
	recupererTypeArgs(ARGUMENTS,TYPEIS).
		
check_args(_,[],[]).
check_args(G,[ARG|ARGUMENTS],[ARGTYPE|ARGSTYPE]):-
	type_expr(G,ARG,ARGTYPE),
	check_args(G,ARGUMENTS,ARGSTYPE).


/* Contexte initial: G0 */
g0([
    (true,bool),
    (false,bool),
    (not,funType([bool],bool)),
    (eq,funType([int, int],bool)),
    (lt,funType([int, int],bool)),
    (add,funType([int, int],int)),
    (sub,funType([int, int],int)),
    (mul,funType([int, int],int)),
    (div,funType([int, int],int))
]).

/* Programme */
    /* (PROG) */
type_prog(G,prog(X),void):-
    type_bloc(G,X,void).

/* Bloc */
type_bloc(G,block(X),void):-
    type_cmds(G,X,void).

/* Suite de commandes */
    /* (DEFS) */
type_cmds(G,[cmds(X)|Y],void):-
	type_def(G,X,G2),
	type_cmds(G2,Y,void).
    /* (END)  */
type_cmds(_,[],void).
type_cmds(G,[X|Y],void):-
	type_stat(G,X,void),
	type_cmds(G,Y,void).

/* Definitions */
    /* (CONST) */
type_def(G,const(X,T,E),[(X,T)|G]):-
	type_expr(G,E,T).
    /* (FUN) */
        /* La règle `type_def` définit le type d'une fonction dans un environnement de types G. */
        /* Elle fusionne les arguments avec G,détermine le type de l'expression de la fonction,*/
        /* extrait les types des arguments,puis met à jour l'environnement avec le nom de la fonction et son type.*/
type_def(G,fun(FUN,T,ARGUMENTS,E),GI):-
	append(ARGUMENTS,G,G2),
	type_expr(G2,E,T),
	recupererTypeArgs(ARGUMENTS,TYPEIS),
	GI=[(FUN,funType(TYPEIS,T))|G]. 
    /* (FUNREC) */
        /* La règle `type_def` définit le type d'une fonction récursive dans un environnement de types G. */
        /* Elle extrait d'abord les types des arguments,puis fusionne les arguments avec G. */
        /* Ensuite,elle met à jour l'environnement temporaire avec le type de la fonction,*/
        /* analyse le type du corps de la fonction,*/
        /* et enfin met à jour l'environnement final avec le nom de la fonction et son type.*/
type_def(G,funRec(FUN,T,ARGUMENTS,E),GI):-
	recupererTypeArgs(ARGUMENTS,TYPEIS),
	append(ARGUMENTS,G,G2),
    G3 = [(FUN,funType(TYPEIS,T))|G2],
	type_expr(G3,E,T),
	GI=[(FUN,funType(TYPEIS,T))|G]. 
    /* (VAR) */
type_def(G,var(X,T),[(X,T)| G]).
    /* (PROC) */
type_def(G,proc(PROCEDURE,ARGUMENTS,E),GI):-
    append(ARGUMENTS,G,G2),
    type_bloc(G2,E,void),
    recupererTypeArgs(ARGUMENTS,TYPEIS),
    GI=[(PROCEDURE,funType(TYPEIS,void))|G].
    /* (PROCREC) */
type_def(G,procRec(PROCEDURE,ARGUMENTS,E),GI):-
    recupererTypeArgs(ARGUMENTS,TYPEIS),
	append(ARGUMENTS,G,G2), 
    G3 =[(PROCEDURE,typeFunc(TYPEIS,T))|G2],
	type_bloc(G3,E,T),
	GI=[(PROCEDURE,typeFunc(TYPEIS,T))|G].

/* Intruction */
    /* (ECHO) */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).	
    /* (SET) */
type_stat(G,set(id(X),E),void):-
    type_expr(G,id(X),T),
    type_expr(G,E,T).
    /* (IF) */
type_stat(G,if(E1,E2,E3),void) :-
    type_expr(G,E1,bool),
    type_bloc(G,E2,void),
    type_bloc(G,E3,void).
    /* (WHILE) */
type_stat(G,while(C,E),void) :-
    type_expr(G,C,bool),
    type_bloc(G,E,void).
    /* (CALL) */
type_stat(G,call(X,ARGUMENTS),void) :-
    type_expr(G,X,funType(ARGSTYPE,void)),
    check_args(G,ARGUMENTS,ARGSTYPE).
/* Expressions */
    /* (NUM) */
type_expr(_,num(N),int) :-
 	integer(N).
    /* (ID) */
type_expr(G,id(X),T):-
    chercher(X,G,T).
    /* (IF)  */
type_expr(G,if(E1,E2,E3),T):-
    type_expr(G,E1,bool),
    type_expr(G,E2,T),
    type_expr(G,E3,T).
    /* (AND) */
type_expr(G,and(E1,E2),bool):-
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).
    /* (OR)  */
type_expr(G,or(E1,E2),bool):-
    type_expr(G,E1,bool),
    type_expr(G,E2,bool).
    /* (APP) */
type_expr(G,app(E1,ARGUMENTS),T):- 
    type_expr(G,E1,funType(ARGSTYPE,T)),
    check_args(G,ARGUMENTS,ARGSTYPE).
    /* (ABS) */
type_expr(G,lambda(ARGUMENTS,E),funType(TYPEIS,T)):- 
    append(ARGUMENTS,G,GI),
    recupererTypeArgs(ARGUMENTS,TYPEIS),
    type_expr(GI,E,T).


main_stdin :-
	read(user_input,T),
    g0(L),
	type_prog(L,T,R),
	print(R).