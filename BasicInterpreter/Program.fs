module Interpreter2
open System

type terminal = 
    | Add 
    | Sub 
    | Mul 
    | Div 
    | Pow 
    | Rem 
    | Lpar 
    | Rpar 
    | Num of float
    | Vid of string
    | Equ

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let ischar c = System.Char.IsLetter c // UPDATE
let parseError = System.Exception("Parser error")
let symError = System.Exception("No value associated to variable name")

let rec scInt(iStr, iVal, frac, div) = 
    match iStr with
    | c :: tail when isdigit c -> 
        if frac then
            scInt(tail, iVal + (float (intVal c)) / div, frac, div * 10.0)
        else
            scInt(tail, 10.0 * iVal + (float (intVal c)), frac, div)

    | '.' :: tail -> 
        if frac then
            raise lexError // Error: More than one decimal point in a number
        else
            scInt(tail, iVal, true, 10.0)
    | _ -> (iStr, iVal)

let rec scChar(iStr, vName:string) =
    match iStr with
    | c :: tail when ischar c -> scChar(tail,(vName + c.ToString()))
    | _ -> (iStr, vName)
let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> 
            match scan tail with
            | Num num :: rest -> Num (-num) :: rest  // implicit Unary minus; 1st case, if the '-' is followed by a number, the number is negated and added to the 
            | _ -> Sub :: scan tail  // Binary minus
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail  // Correctly tokenizing '/' as Div
        | '^'::tail -> Pow :: scan tail
        | '%'::tail -> Rem :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | '='::tail -> Equ:: scan tail  // UPDATE
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c || (c = '-' && List.isEmpty tail |> not && isdigit (List.head tail)) -> 
            let (iStr, iVal) = scInt(tail, intVal c, false, 1.0) 
            Num iVal :: scan iStr
        | c :: tail when ischar c -> let (iStr, vName) = scChar(tail, c.ToString() ) // UPDATE
                                     Vid vName :: scan iStr
        | _ -> raise lexError
    scan (str2lst input)



let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in (E)BNF 1st sprint:
// <VA>       ::= <varID> "=" <E>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
// <NR>       ::= ["Num" | "varVal" ] <value> | "(" <E> ")"
// <varID>    ::= [a-z,A-Z]+     (* varVal is fetched from symbol table/list with key varID *)
//----------------------------------------------------------------------
//last sprint:
//<varID>    ::= [a-z,A-Z]+
//<VA> ::= <varID> "=" <E>
//<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
//<integer> ::= <digit> { <digit> }
//<float> ::= <digit> { <digit> } "." { <digit> }
//<value> ::= <float> | <integer>| varVal
//<E> ::= <T> <Eopt>
//<Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt>| <empty>
//<T> ::= <NR> <Topt>
//<Topt> ::= "*" <NR> <Topt> | "/" <NR> <Topt> |"^" <NR> <Topt> | "%" <NR> <Topt> |  <empty>
//<NR> ::= ("+"|"-")<NR> | <value>  | "(" <E> ")"
//




//
//
//

let rec parser tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt tList =                        
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (NR >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (NR >> Topt) tail
        | Div :: tail -> (NR >> Topt) tail
        | Pow :: tail -> (NR >> Topt) tail
        | Rem :: tail -> (NR >> Topt) tail
        | _ -> tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Vid vName :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError

    let VA tList =  // UPDATE
        match tList with
        | Vid vName :: tail -> match tail with
                               | Equ :: tail -> E tail // inner tail (from this line)
                               | _ -> E tList  // Need tList to keep Vid vName
        | _ -> E tList
    VA tList  // CHANGED FROM E tList

let rec searchVName vName (symList:List<string*float>) =
    match symList with
    | head :: tail -> if (fst head) = vName then (true, (snd head))
                      else searchVName vName tail
    | _ -> (false, 0)

let rec parseNeval tList (symList:List<string*float>) = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, (vID, value)) = 
        match tList with
        | Add :: tail -> let (tLst, (vID, tval)) = T tail
                         Eopt (tLst, (vID, value + tval))
        | Sub :: tail -> let (tLst, (vID, tval)) = T tail
                         Eopt (tLst, (vID, value - tval))
        | _ -> (tList, ("", value))
    and T tList = (NR >> Topt) tList
    and Topt (tList, (vID, value)) =
        match tList with
        | Mul :: tail -> let (tLst, (vID, tval)) = NR tail
                         Topt (tLst, (vID, value * tval))
        | Div :: tail -> let (tLst, (vID, tval)) = NR tail
                         if tval = 0.0 then
                             raise (System.DivideByZeroException("Division by zero is undefined"))
                         else
                             Topt (tLst, (vID, value / tval)) 
        | Pow :: tail -> let (tLst, (vID, tval)) = NR tail
                         Topt (tLst, (vID, value ** tval))
        | Rem :: tail -> let (tLst, (vID, tval)) = NR tail
                         Topt (tLst, (vID, value % tval))
        | _ -> (tList, ("", value))
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, ("", value))
        | Vid vName :: tail -> let res = searchVName vName symList
                               if (fst res) then (tail, ("",(snd res)))
                               else raise symError
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError
    let VA tList = 
        match tList with 
        | Vid vName :: tail -> match tail with 
                               | Equ :: tail -> let (tLst, (vID, tval)) = E tail
                                                (tLst, (vName, tval))
                               | _ -> E tList
        | _ -> E tList
    VA tList
    // UPDATE from E tList 




let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


let rec check4vid sList vName value =  // added to update symbol table list if already existing vName is overwritten
    match sList with
    | head :: tail -> if (fst head) = vName then [(vName,value)]@(check4vid tail vName value) // replace original value
                      else [head]@(check4vid tail vName value) // copy original value
    | _ -> []

let rec printSymTList (sList:List<string*float>)  =
    match sList with
    | head :: [] -> Console.Write("{0}", head)
                    printSymTList []
    | head :: tail -> Console.Write("{0};", head)
                      printSymTList tail
    | [] -> Console.WriteLine("]")

let rec inpLoop (symTList:List<string*float>) = 
    Console.Write("Symbol Table = [")
    let outSym = printSymTList symTList
    let input = getInputString()
    if input <> "" then
        let oList = lexer input
        let sList = printTList oList
        let pList = parser oList  // pList is the remaining token list and should be empty
        if not pList.IsEmpty then raise parseError // NOTE this update to avoid expressions like 3(2+3) that would return a value of 3 and have a nonempty token list ([Lpar Num 2 Add Num 3 Rpar], 3)
        let Out = parseNeval oList symTList
        let tempID = fst (snd Out)
        let tempVal = (snd (snd Out))
        Console.WriteLine("Variable name = {0}", tempID)    // UPDATE
        Console.WriteLine("Result = {0}", tempVal)          // UPDATED
        // Check whether variable name was already in symTList and if so replace with new value
        if tempID.Length > 0 then // update symbol table
            if symTList.IsEmpty then 
                inpLoop (symTList@[tempID, tempVal])  // append new value if symbol table is empty
            else 
                let res = check4vid symTList tempID tempVal // if tempID is already in symbol table replace its value
                let check = res.Equals(symTList)      // Check whether res is equal to the original (means no replacing was done)
                if check then inpLoop (symTList@[tempID, tempVal])  // if true pass old list with appended new tuple                 
                else inpLoop res   // if false pass updated res list with updated tuple
        else inpLoop symTList 
    else symTList

[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let res = inpLoop [] 
    Console.WriteLine("Symbol table is {0}", res )
    0
