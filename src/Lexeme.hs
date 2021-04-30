module Lexeme where

data CLexeme
  -- preprocessor
  = LPPConcat   -- ##
  | LPPDefine   -- #define
  | LPPElif     -- #elif
  | LPPElse     -- #else
  | LPPEmpty    -- #
  | LPPEndif    -- #endif
  | LPPError    -- #error
  | LPPIf       -- #if
  | LPPIfdef    -- #ifdef
  | LPPIfndef   -- #ifndef
  | LPPInclude  -- #include
  | LPPLine     -- #line
  | LPPPragma   -- #pragma
  | LPPUndef    -- #undef
  -- keywords
  | LAuto
  | LBreak
  | LCase
  | LChar
  | LConst
  | LContinue
  | LDefault -- for switch case
  | LDo
  | LDouble
  | LElse
  | LEnum
  | LExtern
  | LFloat
  | LFor
  | LGoto
  | LIf
  | LInt
  | LLong
  | LRegister
  | LReturn
  | LShort
  | LSigned
  | LSizeof
  | LStatic
  | LStruct
  | LSwitch
  | LTypedef
  | LUnion
  | LUnsigned
  | LVoid
  | LVolatile
  | LWhile
  -- parentheses
  | LParenthesisOpen        -- (
  | LParenthesisClose       -- )
  | LBracketOpen            -- [
  | LBracketClose           -- ]
  | LBraceOpen              -- {
  | LBraceClose             -- }
  -- comparison
  | LNot                    -- !
  | LEquals                 -- ==
  | LNotEquals              -- !=
  | LLT                     -- <
  | LLTE                    -- <=
  | LGT                     -- >
  | LGTE                    -- >=
  | LAnd                    -- &&
  | LOr                     -- ||
  -- operators
  | LModulo                 -- %
  | LStar                   -- * (pointer or multiplication)
  | LPlus                   -- +
  | LMinus                  -- -
  | LDivision               -- /
  | LAmp                    -- & (bitwise and & address of variable)
  | LBitwiseOr              -- |
  | LBitShiftLeft           -- <<
  | LBitShiftRight          -- >>
  | LBitwiseXor             -- ^
  | LBitwiseNot             -- ~
  -- assign opeartors
  | LAssign                 -- =
  | LModuloAssign           -- %=
  | LMultiplicationAssign   -- *=
  | LPlusAssign             -- +=
  | LMinusAssign            -- -=
  | LDivisionAssign         -- /=
  | LBitwiseAndAssign       -- &=
  | LBitwiseOrAssign        -- |=
  | LBitShiftLeftAssign     -- <<=
  | LBitShiftRightAssign    -- >>=
  | LBitwiseXorAssign       -- ^=
  | LIncrement              -- ++
  | LDecrement              -- --
  -- other symbols
  | LComma                  -- ,
  | LArrow                  -- -> (for structs)
  | LDot                    -- . (for structs)
  | LVarargs                -- ... (for functions with variable number of args)
  | LColon                  -- : (switch case & goto labels & ternary)
  | LSemiColon              -- ;
  | LTernary                -- ?
  -- literals
  | LCharLiteral Char
  | LFloatLiteral Double
  | LIntLiteral Int
  | LStringLiteral String
  -- label
  | LLabel String
  -- whitespace
  | LWhiteSpace
  -- comment
  | LComment
  -- end marker
  | LEndMarker
  deriving (Show, Eq)

