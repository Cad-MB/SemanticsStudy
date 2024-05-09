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
type_bloc(G,block(X),T):-
    type_cmds(G,X,T).

/* Suite de commandes */
    /* (DEFS) */
type_cmds(G,[cmds(X)|Y],T):-
	type_def(G,X,G2),
	type_cmds(G2,Y,T).
    /* (END)  */
type_cmds(_,[],void).
type_cmds(G,X,T):-
	type_stat(G,X,T),
    /* RETURN */
type_cmds(G,[return(E)],T):-
    type_expr(G,E,T),
    T\=void.
    /* STAT0  -- (stat0) pour tout type t, si Γ `Stat s : void et Γ `Cmds cs : t alors Γ `Cmds (s;cs) : t*/ 
type_cmds(G,[X|Y],T):-
    type_stat(G,X,void),
    type_cmds(G,Y,T).
    /* STAT1 (stat1) si t != void, si Γ `Stat s : t + void et Γ `Cmds cs : t alors Γ `Cmds (s;cs) : t */
type_cmds(G,[X|Y],T):-
    type_stat(G,X,union(T,void)),
    type_cmds(G,Y,T).
type_cmds(Ctx, [Stat], T) :- 
    T\=void, 
    type_stat(Ctx, Stat, T).


/* Definitions */

getTypeArgsProc([],[]).
getTypeArgsProc([(arg(_),T)|ARGS],[T|RES]) :-
	getTypeArgsProc(ARGS,RES).
getTypeArgsProc([(argVar(_),T)|ARGS],[ref(T)|RES]) :-
	getTypeArgsProc(ARGS,RES).

editProcsType([],[]).
editProcsType([(argVar(IDF),T)|ARGS],[(IDF,ref(T))|RES]) :-
	editProcsType(ARGS,RES).
editProcsType([(arg(IDF),T)|ARGS],[(IDF,T)|RES]) :-
	editProcsType(ARGS,RES).

verifier_argumentsp(_,[],[]).
verifier_argumentsp(G,[ARGP|ARGSP],[ARGPTYPE|ARGSPTYPE]) :-
	type_proPars(G,ARGP,ARGPTYPE),
	verifier_argumentsp(G,ARGSP,ARGSPTYPE).

    /* (CONST) */
type_def(G,const(X,T,E),[(X,T)|G]):-
	type_expr(G,E,T).
    /* (FUN) */
type_def(G,fun(FUN,T,ARGUMENTS,E),GI):-
	append(ARGUMENTS,G,G2),
	type_expr(G2,E,T),
	recupererTypeArgs(ARGUMENTS,TYPEIS),
	GI=[(FUN,funType(TYPEIS,T))|G]. 
    /* (FUNREC) */
type_def(G,funRec(FUN,T,ARGUMENTS,E),GI):-
	recupererTypeArgs(ARGUMENTS,TYPEIS),
	append(ARGUMENTS,G,G2),
    G3 = [(FUN,funType(TYPEIS,T))|G2],
	type_expr(G3,E,T),
	GI=[(FUN,funType(TYPEIS,T))|G]. 
    /* (VAR) */
type_def(G,var(X,T),[(X,ref(T))|G]).
    /* (PROC) */
type_def(G,proc(PROCEDURE,ARGUMENTS,E),GI):-
    editProcsType(ARGUMENTS,ARGUMENTS2),
    append(ARGUMENTS2,G,G2),
    type_bloc(G2,E,void),
    getTypeArgsProc(ARGUMENTS,TYPEIS),
    GI=[(PROCEDURE,funType(TYPEIS,void))|G].

    /* (PROCREC) */
type_def(G,procRec(PROCEDURE,ARGUMENTS,E),GI):-
    editProcsType(ARGUMENTS,ARGUMENTS2),
    getTypeArgsProc(ARGUMENTS,TYPEIS),
	append(ARGUMENTS2,G,G2), 
    G3 =[(PROCEDURE,funType(TYPEIS,T))|G2],
	type_bloc(G3,E,T),
	GI=[(PROCEDURE,funType(TYPEIS,T))|G].

    /* (funBlock) si Γ[x1 : t1; ...; xn : tn] `Block bk : t alors Γ `Def (FUN x t [x1:t1, ...,xn:tn] bk) : Γ[x : (t1 * ... * tn -> t)] */
type_def(G,funBlock(FUN,T,ARGUMENTS,BLOC),GI):-
    recupererTypeArgs(ARGUMENTS,TYPEIS),
    append(ARGUMENTS,G,G2),
    G3 = [(FUN,funType(TYPEIS,T))|G2],
    type_bloc(G3,BLOC,T),
    GI=[(FUN,funType(TYPEIS,T))|G].

    /* funRecBlock */
type_def(G,funRecBlock(FUN,T,ARGUMENTS,BLOC),GI):-
    recupererTypeArgs(ARGUMENTS,TYPEIS),
    append(ARGUMENTS,G,G2),
    G3 = [(FUN,funType(TYPEIS,T))|G2],
    type_bloc(G3,BLOC,T),
    GI=[(FUN,funType(TYPEIS,T))|G].

/* Intruction */
    /* (ECHO) */
type_stat(G,echo(E),void) :-
	type_expr(G,E,int).	
    /* (SET) */
type_stat(G,set(LVALUE,E),void):-
    type_lvalue(G,LVALUE,T),
    type_expr(G,E,T).
    /* (IF) */
type_stat(G,if(E1,E2,E3),T) :-
    type_expr(G,E1,bool),
    type_bloc(G,E2,T),
    type_bloc(G,E3,T).
type_stat(G,if(E1,E2,E3),union(T, void)) :-
    type_expr(G,E1,bool),
    type_bloc(G,E2,void),
    type_bloc(G,E3,T).
type_stat(G,if(E1,E2,E3),union(T, void)) :-
    type_expr(G,E1,bool),
    type_bloc(G,E2,T),
    type_bloc(G,E3,void).
    /* (WHILE) */
type_stat(G,while(C,E),union(T, void)) :-
    type_expr(G,C,bool),
    type_bloc(G,E,T).
    /* (CALL) */ 
type_stat(G,call(X,ARGUMENTS),void) :-
    type_expr(G,X,funType(ARGSTYPE,void)),
    verifier_argumentsp(G,ARGUMENTS,ARGSTYPE).
/* RETURN */
type_stat(_, return(E), T):-
    type_expr(_, E, T),
    T \= void.

/* lValue */
    /* (LVAR) */
type_lvalue(G,id(X),T):-
    type_expr(G,id(X),ref(T)).
    /* (LNTH) */
type_lvalue(G,vectorValue(E1,E2),T):-
    type_expr(G,E1,vectorType(T)),
    type_expr(G,E2,int).

/* Parametres d'appel */
    /* (REF) */
type_proPars(G,exprProcAdr(ID),ref(T)):-
    type_expr(G,id(ID),ref(T)).
    /* (VAL) */
type_proPars(G,E,T):-
    type_expr(G,E,T).

/* Expressions */
    /* (NUM) */
type_expr(_,num(N),int) :-
 	integer(N).
    /* (IDV) */
type_expr(G,id(X),T):-
    chercher(X,G,T).
    /* (IDR) */
type_expr(G,id(X),T):-
    chercher(X,G,ref(T)).
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
    verifier_argumentsp(G,ARGUMENTS,ARGSTYPE).
    /* (ABS) */
type_expr(G,lambda(ARGUMENTS,E),funType(TYPEIS,T)):- 
    append(ARGUMENTS,G,GI),
    recupererTypeArgs(ARGUMENTS,TYPEIS),
    type_expr(GI,E,T).
    /* (ALLOC) */
type_expr(G,alloc(E),vectorType(_)):-
    type_expr(G,E,int).
    /* (LEN) */
type_expr(G,len(E),int):-
    type_expr(G,E,vectorType(_)).
    /* (NTH) */
type_expr(G,nth(E1,E2),T):-
    type_expr(G,E1,vectorType(T)),
    type_expr(G,E2,int).
    /* (VSET) */
type_expr(G,vset(E1,E2,E3),vectorType(T)):-
    type_expr(G,E1,vectorType(T)),
    type_expr(G,E2,int),
    type_expr(G,E3,T).


main_stdin :-
	read(user_input,T),
    g0(L),
	type_prog(L,T,R),
	print(R).