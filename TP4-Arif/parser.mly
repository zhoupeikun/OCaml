%{

  open Ast

  (* Fonction pour aider à la localisation des erreurs. *)
  let current_pos () =
    Parsing.symbol_start_pos (),
    Parsing.symbol_end_pos ()

  (* Fonction de construction d'un [node_expr], qui renseigne correctement
     la localisation de l'expression. *)
  let mk_node e = { expr = e; pos = current_pos() }

%}


  
/* Liste des lexèmes, par ordre alphabétique. */
/* Il faut y ajouter les nouveaux lexèmes apportés par le fragment Arif. */
/* Remarque : on a enlevé ici le lexème IDENT qui était présent mais pas
   utilisé dans le fragment Arith. */    
%token <int> CONST_INT
%token CONST_UNIT
%token EOF
%token EOI
%token LPAREN
%token MINUS
%token PLUS
%token PRINT_INT
%token PRINT_NEWLINE
%token RPAREN
%token SLASH
%token STAR

%token <Ast.binop> COMP
%token IF THEN ELSE
%token BOR
%token BAND
%token UNOT
%token <bool>CONST_BOOL


/* Associativités et priorités. */
/* À compléter de manière à régler les conflits de la grammaire. */
%left PLUS
%left MINUS 
%left STAR
%left SLASH 

%left BOR
%left BAND
%left UNOT
%left COMP
%left THEN ELSE
%left IF
/* Déclaration de la règle principale et de son type. */
%start prog
%type <Ast.prog> prog

%%



/* Début des règles d'analyse lexicale. */


/* Un programme est une séquence de zéro ou plusieurs instructions, qui est
   représentée par une liste. Pour gérer la répétition (l'étoile dans la
   grammaire), on utilise un non-terminal supplémentaire [instr_seq].
*/
prog:
| instr_seq EOF { $1 }
;

instr_seq:
| /* empty */   
    { [] }
| instr instr_seq      
    { $1 :: $2 }
;



/* Une instruction est une expression suivie par un ;; (EOI). */
instr:
| expr EOI
    { Icompute $1 }
;



/* Les expressions sont de trois sortes :
   1/ Des "expressions simples", qui sont soit des constantes soit des
      expressions délimitées par des parenthèses.
   2/ La combinaisons d'une ou plusieurs expressions par des opérateurs
      arithmétiques.
   3/ L'application d'une fonction primitive [print_int] ou [print_newline]
      à une expression simple.
*/
      
expr:
/* Cadeau : l'inclusion des expressions simples dans l'ensemble des expressions
   et l'application de la fonction [print_newline].
*/
| simple_expr
    { $1 }
| PRINT_NEWLINE simple_expr
    { mk_node (Eprint_newline $2) }

/* À compléter avec les autres formes d'expressions. */
|PRINT_INT simple_expr
    { mk_node (Eprint_int $2) }
|MINUS expr
    {mk_node (Eunop(Uminus, $2))}

|expr SLASH expr
    { mk_node (Ebinop(Bdiv , $1 , $3)) }

|expr STAR expr
    { mk_node (Ebinop(Bmul , $1 , $3)) }

|expr MINUS expr
    { mk_node (Ebinop(Bsub , $1 , $3)) }

|expr PLUS expr
    { mk_node (Ebinop(Badd , $1 , $3)) }

|expr COMP expr
    { mk_node (Ebinop($2, $1, $3))}

|expr BOR expr
    { mk_node (Ebinop(Bor,$1,$3))}

|expr BAND expr
    { mk_node (Ebinop(Band,$1,$3))}

|UNOT expr
    {mk_node (Eunop(Unot,$2))}

|IF expr THEN expr ELSE expr
    {mk_node (Eif ($2,$4,$6))}
;



simple_expr:
| CONST_UNIT
    { mk_node (Econst Cunit) }

/* À compléter avec les autres formes d'expressions simples. */
| CONST_INT
    { mk_node (Econst (Cint $1))}

| CONST_BOOL
    { mk_node (Econst (Cbool $1))} 

|LPAREN expr RPAREN
    {$2}
;

