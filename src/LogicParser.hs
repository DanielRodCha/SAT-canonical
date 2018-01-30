module LogicParser (parseFProp) where

import Logic
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token as Token
import Text.Parsec.Language

-- Language definition
def = emptyDef { identStart = letter
               , identLetter = alphaNum
               , opStart = oneOf "&|>=~"
               , opLetter = oneOf "&|>=~"
               , reservedOpNames = ["&", "|", ">", "=", "~"]
               , reservedNames = ["T","F"]
               }

-- Lexer based on that definition
lexer = makeTokenParser def

-- Parsers
m_parens = Token.parens lexer
m_reserved = Token.reserved lexer
m_reservedOp = Token.reservedOp lexer
m_identifier = Token.identifier lexer

expr :: Parser FProp
expr = buildExpressionParser table term

term = m_parens expr <|> atomicTerm

atomicTerm = literalTerm <|> identifierTerm

literalTerm = (m_reserved "T" >> return (T))
          <|> (m_reserved "F" >> return (F))

identifierTerm = Atom <$> m_identifier

-- Operator table, from biggest to lowest priority
table = [ [ prefix "-" Neg ]
        , [ binary "&" Conj AssocLeft ]
        , [ binary "|" Disj  AssocLeft ]
        , [ binary "->" Impl AssocLeft ]
        , [ binary "<->" Equi AssocLeft ]
        ]

binary  name fun assoc = Infix   (m_reservedOp name >> return fun ) assoc
prefix  name fun       = Prefix  (m_reservedOp name >> return fun )
postfix name fun       = Postfix (m_reservedOp name >> return fun )

-- Parser
parseFProp :: String -> Either ParseError FProp
parseFProp = parse expr ""