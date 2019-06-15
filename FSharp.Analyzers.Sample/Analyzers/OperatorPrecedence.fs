module OperatorPrecedence

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open System.IO
open Expecto.Logging

let mutable functionNames = Map.empty




let rec visitExpression handler body= 
    match body with
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) -> 
        // printfn "In APP with funcExpr %A | argExpr %A" funcExpr argExpr
        match funcExpr with 
        | SynExpr.Ident(name) -> 
            // printfn "name of function call: %A" name
            let containsKey = (Map.containsKey (name.ToString()) functionNames)
            // printfn "Checking %A exists with function names: %A " name containsKey
            if containsKey then
                // let mutable count = 0
                // countArg body &count  
                let res = (functionNames.TryFind (name.ToString()))
                let mutable numArgs = 0
                match res with
                    | Some y -> (numArgs <- y)
                    | None -> ()
                // printfn "num args %d" numArgs
                handler range (name.ToString()) numArgs
        | _ -> ()
        visitExpression handler funcExpr
        visitExpression handler argExpr
    | SynExpr.LetOrUse(isRecurisve,isUse,bindings,body,range) ->()
        // printfn "Let isUse %A" isUse
        // printfn "Bindings %A" bindings
        // printfn "Body %A" body
        // printfn "range %A" range

    | x -> ()
    // printfn "unmatched! %A " x
       
    // | pat -> printfn "  .. other pattern: %A" pat
let visitSynVal (x:SynValData) =
    match x with 
    | SynValData (memberFlags,synvalInfo,indentOption) ->
        // printfn "memberFlags %A" memberFlags
        // printfn "SynvalInfo %A" synvalInfo
        match synvalInfo with
        | SynValInfo (curriedArgsInfo, returninfo) ->
            // printfn "curriedArgsInfo"  
            // printfn "There are %d args" curriedArgsInfo.Length
            // for arg in curriedArgsInfo do
            //     printfn "args %A" arg
            curriedArgsInfo.Length
            // printfn "returnInfo %A" returninfo
        // printfn "indentOption %A" indentOption
    
let rec visitPattern pat data = 
    match pat with
    | SynPat.Wild(x) -> () 
    // printfn "  .. underscore pattern"
    | SynPat.Named(pat, name, _, _, _) ->   
        visitPattern pat data
        // printfn "  .. named as '%s'" name.idText
        // This is for let result =.. -> result
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) -> 
        let names = 
            String.concat "." [for i in ident -> i.idText]
        // printfn "  .. identifier: %s" names
        // identifier is name of function call 
        // let add x y =... -> add
        let numArgs = visitSynVal data
        functionNames <- functionNames.Add(names,numArgs)
        ()
    | _ -> ()

let visitDeclarations handler decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, 
                             pat, retInfo, body, m, sp)) = binding
                // printfn "################# \n"
                // visitSynVal data
                // // printfn "SynVal data : %A" data
                // visitPattern pat \
                visitPattern pat data
                visitExpression handler body
                // printfn "################# \n"
        | _ -> ()

let visitModulesAndNamespaces handler modulesOrNss = 
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = 
        modulesOrNss
    // printfn "Namespace or module: %A" lid
    visitDeclarations handler decls

let checker = FSharpChecker.Create(keepAssemblyContents=true)
let parseAndCheckSingleFile (input) = 
    let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
    File.WriteAllText(file, input)
    // Get context representing a stand-alone (script) file
    let projOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, input)
        |> Async.RunSynchronously

    let fprojOptions = projOptions

    checker.ParseAndCheckProject (fprojOptions)
    |> Async.RunSynchronously

let checkPrefix str previousStack= 
    let rec loop xs stack = 
        match (xs, stack) with
        // find prefixes, add to stack
        | '+' :: ys,  stack -> loop ys ('+' :: stack)
        | '-' :: ys,  stack -> loop ys ('-' :: stack)
        // if we get a +, followed by a space, continue
        | ' ' :: ys, '+' :: stack -> loop ys stack
        | ' ':: ys, '-' :: stack -> loop ys stack
        // if there is no space between them possible error
        | c :: _, '+' :: stack -> (false,(c,'+'))
        | c :: _, '-' :: stack -> (false,(c,'-'))
        // any other character loop
        | _ :: ys, stack -> loop ys stack
        // both empty then fine
        | [], [] -> (true,(' ',' '))
        // empty, and possible continue to next line?
        | [], _ -> (false,(' ',' '))
    loop (Seq.toList str) previousStack


[<Analyzer>]
let IncorrectParameters : Analyzer  =
    fun ctx ->
        let contents = ctx.Content
        // printfn "ctx %A" ctx.ParseTree
        let state = ResizeArray<(range *char * char)>()
        let string = ctx.Content |> String.concat "\n"
        let checkProjectResults = parseAndCheckSingleFile(string)
        // printfn "Errors: %A" checkProjectResults.Errors
        let mutable FunctionName = ""
        let mutable ExpectedArguments = 0
        
        if checkProjectResults.Errors.Length > 0 then
            for error in checkProjectResults.Errors do 
                printfn "Errors %A " error
                printfn "Error line: %A - %A " error.StartLineAlternate error.EndLineAlternate
                if error.Message = "This value is not a function and cannot be applied." then do 
                    // check for prefixes here
                    if error.StartLineAlternate = error.EndLineAlternate then do 
                        let codeToCheck = contents.[error.StartLineAlternate - 1]
                        let (result,(char,prefix)) = checkPrefix codeToCheck []
                        if result = false then do 
                            let Startposition = mkPos (error.StartLineAlternate) error.StartColumn
                            let EndPosition = mkPos (error.EndLineAlternate) codeToCheck.Length
                            let range = mkRange ctx.FileName Startposition EndPosition
                            state.Add (range,char,prefix)
                            printfn "Result = %A, if false there is no space between + / - error! " result
                    
        printfn "State is %A" state

        state
        |> Seq.map (fun (range,char,prefix) ->
            { Type = "Possibly no spacing between prefixes! \n"
              Message = "Try adding a space between " +  prefix.ToString()  + " and " + char.ToString()
              Code = "P001"
              Severity = Warning
              Range = range
              Fixes = []}

        )
        |> Seq.toList