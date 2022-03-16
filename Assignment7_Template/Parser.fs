﻿module ImpParser

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x)
    let pletter = satisfy (fun x -> System.Char.IsLetter x)
    let pletters1 = many pletter
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x)
    let palphanumerics = many palphanumeric
    let pdigit = satisfy (fun x -> System.Char.IsDigit x)

    let spaces         = many (satisfy (fun x -> System.Char.IsWhiteSpace x))
    let spaces1        = many1 (satisfy (fun x -> System.Char.IsWhiteSpace x))

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2
    let (.>*>) p1 p2  = (p1 .>> spaces) .>> p2
    let (>*>.) p1 p2  = (p1 .>> spaces) >>. p2

    let parenthesise (p:Parser<'a>) = pchar '(' >*>. p .>*> pchar ')'

    let pid = 
        let aux (l: list<char>) = Seq.ofList l |> System.String.Concat
        pchar '_' <|> pletter .>>. many (palphanumeric <|> pchar '_') |>> fun (a,b) -> aux (a::b)

    let unop op a = op >*>. a

    let binop op p1 p2 = p1 .>*>. unop op p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    

    let VParse = pid |>> V <?> "V"
    let NParse   = pint32 |>> N <?> "Int"
    let NegParse = (pchar '-' >>. NParse) |>> (fun x -> Mul ((N (-1)), x)) <?> "Neg"
    let ParParse = parenthesise TermParse
    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PV"

    let CharacterParse, cref = createParserForwardedToRef<cExp>()
    let ParCParse = parenthesise CharacterParse
    let CharToIntParse = pCharToInt >*>. ParCParse|>> CharToInt <?> "CharToInt"

    let AexpParse = TermParse 

    let CParse = pchar ''' >>. pletter .>> pchar ''' |>> C <?> "C"
    let CVParse = ParParse |>> CV <?> "CV"
    let ToUpperParse = pToUpper >*>. ParCParse |>> ToUpper <?> "ToUpper"
    let ToLowerParse = pToLower >*>. ParCParse |>> ToLower <?> "ToLower"
    let IntToCharParse = pIntToChar >*>. ParParse|>> IntToChar <?> "IntToChar"
    
    do tref := choice [AddParse; SubParse; ProdParse;]
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    do aref := choice [CharToIntParse; NegParse; PVParse; VParse; NParse; ParParse]
    do cref := choice [CParse; ParCParse; ToUpperParse; ToLowerParse; IntToCharParse]


    let CexpParse = CharacterParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"

