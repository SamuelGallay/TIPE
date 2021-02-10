%token EOF
%token <string> VAR
%token <string> PRE
%token IF
%token AND
%token STOP
%token LPAR
%token RPAR
%token LBRA
%token RBRA
%token TAIL
%token ASK
%token NVAR

%start <Types.clause list> programme

%start <Types.term list> requete

%%

requete:
  | r = liste_termes ASK EOF { r }

programme:
  | c = clause EOF { [c] }
  | c = clause p = programme { c :: p}

clause:
  | t = terme STOP { Types.Clause (t, []) }
  | t = terme IF l = liste_termes STOP { Types.Clause (t, l) }

var:
  | v = VAR { Types.Id (v, 0) }
  | NVAR { Utilitary.newvar () }

terme:
  | v = var { Types.Var v}
  | p = PRE { Types.Predicate (p, [])}
  | p = PRE LPAR l = liste_termes RPAR { Types.Predicate (p, l)}
  | t = table { Types.Table t }

liste_termes:
  | t = terme { [t] }
  | t = terme AND l = liste_termes { t :: l }

table:
  | LBRA RBRA { Types.Empty }
  | LBRA l = liste_termes RBRA { Utilitary.table_of_termlist Types.Empty l }
  | LBRA l = liste_termes TAIL t = table RBRA { Utilitary.table_of_termlist t l }
  | LBRA l = liste_termes TAIL v = var RBRA { Utilitary.table_of_termlist (Types.TVar v) l }
