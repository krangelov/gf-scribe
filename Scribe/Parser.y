-- -*- haskell -*-
{
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module Scribe.Parser
         ( P, runP, runPartial
         , pDocument
         ) where

import PGF
import Scribe.AbsSyn
import Scribe.Lexer
import qualified Data.ByteString.Char8 as BS
}

%name pDocument Document

-- no lexer declaration
%monad { P } { >>= } { return }
%lexer { lexer } { T_EOF }
%tokentype { Token }


%token 
 '!'          { T_exclmark  }
 '#'          { T_patt      }
 '$'          { T_int_label }
 '('          { T_oparen    }
 ')'          { T_cparen    }
 '~'          { T_tilde     }
 '*'          { T_star      }
 '**'         { T_starstar  }
 '+'          { T_plus      }
 '++'         { T_plusplus  }
 ','          { T_comma     }
 '-'          { T_minus     }
 '->'         { T_rarrow    }
 '.'          { T_dot       }
 ':'          { T_colon     }
 ';'          { T_semicolon }
 '<'          { T_less      }
 '='          { T_equal     }
 '=>'         { T_big_rarrow}
 '>'          { T_great     }
 '</'         { T_end_tag_L }
 '/>'         { T_end_tag_R }
 '?'          { T_questmark }
 '@'          { T_at        }
 '['          { T_obrack    }
 ']'          { T_cbrack    }
 '{'          { T_ocurly    }
 '}'          { T_ccurly    }
 '\\'         { T_lam       }
 '\\\\'       { T_lamlam    }
 '_'          { T_underscore}
 '|'          { T_bar       }
 '::='        { T_cfarrow   }
 'PType'      { T_PType     }
 'Str'        { T_Str       }
 'Strs'       { T_Strs      }
 'Tok'        { T_Tok       }
 'Type'       { T_Type      }
 'abstract'   { T_abstract  }
 'case'       { T_case      }
 'cat'        { T_cat       }
 'concrete'   { T_concrete  }
 'data'       { T_data      }
 'def'        { T_def       }
 'flags'      { T_flags     }
 'fold'       { T_fold       }
 'for'        { T_for       }
 'in'         { T_in        }
 'if'         { T_if        }
 'then'       { T_then      }
 'else'       { T_else      }
 'let'        { T_let       }
 'lin'        { T_lin       }
 'lincat'     { T_lincat    }
 'lindef'     { T_lindef    }
 'linref'     { T_linref    }
 'of'         { T_of        }
 'open'       { T_open      }
 'oper'       { T_oper      }
 'param'      { T_param     }
 'pattern'    { T_pattern   }
 'pre'        { T_pre       }
 'printname'  { T_printname }
 'resource'   { T_resource  }
 'strs'       { T_strs      }
 'variants'   { T_variants  }
 'where'      { T_where     }
 'with'       { T_with      }
 'coercions'  { T_coercions }
 'terminator' { T_terminator }
 'separator'  { T_separator }
 'nonempty'   { T_nonempty  }

Integer       { (T_Integer $$) }
Double        { (T_Double  $$) }
String        { (T_String  $$) }
Ident         { (T_Ident   $$) }


%%

Document :: { Term }
Document
  : Exp  { $1 }
  | Exp1 { $1 }

Exp :: { Term }
Exp
  : OpenTag ListExp CloseTag      {% if fst $1 /= $3
                                        then fail "Opening and closing tag doesn't match"
                                        else return (Tag (fst $1) (snd $1) $2) }
  | OpenTag Exp1 CloseTag         {% if fst $1 /= $3
                                        then fail "Opening and closing tag doesn't match"
                                        else return (Tag (fst $1) (snd $1) $2) }
  | EmptyTag                      { Tag (fst $1) (snd $1) Empty }
  | Exp1 ';'                      { $1 }

Exp1 :: { Term }
Exp1
  : Exp1 Exp2                     { App $1 $2 }
  | Exp2                          { $1 }

Exp2 :: { Term }
Exp2
  : Exp2 '.' Ident        { Prop $1 $3 }
  | Exp2 '?' Ident        { Qual $1 $3 }
  | Ident                 { Var $1 }
  | String                { if null $1 then Empty else Lit (LStr $1) }
  | Integer               { Lit (LInt $1) }
  | Double                { Lit (LFlt $1) }
  | '[' Document ']'      { Delim $2 }
  | '(' Exp1 ')'          { $2 }

ListExp :: { Term }
ListExp
  :                       { Empty }
  | Exp ListExp           { case C $1 $2 of {
                              C Empty t -> t ;
                              C t Empty -> t ;
                              t         -> t
                            }
                          }

OpenTag :: { (Ident, [Attribute]) }
OpenTag
  : '<' Ident ListAttributes '>' { ($2,$3) }

CloseTag :: { Ident }
CloseTag
  : '</' Ident '>' { $2 }

EmptyTag :: { (Ident, [Attribute]) }
EmptyTag
  : '<' Ident ListAttributes '/>' { ($2,$3) }

ListAttributes :: { [Attribute] }
ListAttributes
  : Attribute ListAttributes { $1:$2 }
  |                          { []    }

Attribute :: { Attribute }
Attribute
  : Ident '=' Exp2 { ($1,$3) }

{
happyError :: P a
happyError = fail "syntax error"
}
