module Language.Imp.Pretty where

import Language.Imp.Syntax
import Data.Text.Prettyprint.Doc
import Data.Text (Text)
import Data.Text.Prettyprint.Doc.Render.Text

printer :: CompUnit -> Text
printer = renderStrict . layoutPretty defaultLayoutOptions . pretty 

instance Pretty Literal where
  pretty (Integer i)   = pretty i
  pretty (Character c) = squote <> pretty c <> squote
  pretty (String t)    = dquote <> pretty t <> dquote  

instance Pretty Ident where
  pretty (Ident i)     = pretty i

instance Pretty Expr where
  pretty (Lit l)       = pretty l
  pretty (Var i)       = pretty i
  pretty (FunctionCall i es) =
    pretty i <> parens (fillSep (punctuate comma (map pretty es)))

instance Pretty Assignment where
  pretty (Assn i e) = pretty i <+> pretty "=" <+> pretty e <> semi

instance Pretty Declaration where
  pretty (Decl i (Just e)) = 
    pretty "var" <+> pretty i <+> pretty "=" <+>  pretty e <> semi
  pretty (Decl i Nothing) = 
    pretty "var" <+> pretty i <> semi

instance Pretty Statement where
  pretty (StatementExpr e) = pretty e <> semi
  pretty (If e bs bf)      =
    pretty "if" <> parens (pretty e) <> pretty bs <>
    pretty "else" <> pretty bf
  pretty (Loop e b)        =
    pretty "loop" <> parens (pretty e) <> pretty b
  pretty (Declaration dec) =
    pretty dec
  pretty (Assignment asn) =
    pretty asn
  pretty (Return e) = 
    pretty "return" <+> pretty e <> semi

instance Pretty Block where
  pretty (Block ss) =
    braces (align (vcat (map pretty ss)))

instance Pretty Function where
  pretty (Function i ps b) =
    pretty "fn" <+> pretty i <> parens (fillSep (punctuate comma (map (\e -> pretty "var" <+> pretty e) ps))) <>
    pretty b 

instance Pretty CompUnit where
  pretty (CompUnit fs) =
    vcat (map pretty fs)
