module OperatorPrecedence

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open System.IO


let rec visitExpression handler body= 
    match body with
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) -> 
        // printfn "In APP with funcExpr %A | argExpr %A" funcExpr argExpr
        match funcExpr with 
        | SynExpr.Ident(name) -> 
            printfn "name of function call: %A" name
        | _ -> ()
        visitExpression handler funcExpr
        visitExpression handler argExpr
    | SynExpr.LetOrUse(isRecurisve,isUse,bindings,body,range) ->
        printfn "Let isUse %A" isUse
        printfn "Bindings %A" bindings
        printfn "Body %A" body
        printfn "range %A" range

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
            printfn "curriedArgsInfo"  
            printfn "There are %d args" curriedArgsInfo.Length
            for arg in curriedArgsInfo do
                printfn "args %A" arg
            curriedArgsInfo.Length
            // printfn "returnInfo %A" returninfo
        // printfn "indentOption %A" indentOption
    
let rec visitPattern pat data = 
    match pat with
    | SynPat.Wild(x) -> printfn "  .. underscore pattern"
    | SynPat.Named(pat, name, _, _, _) ->   
        visitPattern pat data
        printfn "  .. named as '%s'" name.idText
        // This is for let result =.. -> result
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) -> 
        let names = 
            String.concat "." [for i in ident -> i.idText]
        printfn "  .. identifier: %s" names
        // identifier is name of function call 
        // let add x y =... -> add
        let numArgs = visitSynVal data
        ()
    | _ -> ()

let visitDeclarations handler decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, 
                             pat, retInfo, body, m, sp)) = binding
                printfn "################# \n"
                // visitSynVal data
                // // printfn "SynVal data : %A" data
                // visitPattern pat \
                visitPattern pat data
                visitExpression handler body
                printfn "################# \n"
        | _ -> ()

let visitModulesAndNamespaces handler modulesOrNss = 
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = 
        modulesOrNss
    printfn "Namespace or module: %A" lid
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

[<Analyzer>]
let OperatorPrecedence : Analyzer  =
    printfn "Inside OperatorPrecedence!"
    fun ctx ->
        printfn "ctx %A" ctx
        let state = ResizeArray<range>()
        let string = ctx.Content |> String.concat "\n"
        let checkProjectResults = parseAndCheckSingleFile(string)
        // printfn "Errors: %A" checkProjectResults.Errors
        if checkProjectResults.Errors.Length > 0 then
            // handler adds the range to display
            let handler (range: range) functionName expectedArguments = 
                // printfn "###################################"
                // printfn "SynExpr type %A" m
                // printfn "###################################"

                state.Add range
            let parseTree = ctx.ParseTree
            match parseTree with
            | ParsedInput.ImplFile(implFile) ->
                // Extract declarations and walk over them
                let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
                modules |>  List.iter (visitModulesAndNamespaces handler)
            | _ -> failwith "F# Interface file (*.fsi) not supported."

        state
        |> Seq.map (fun r ->
            { Type = "Possibly wrong number of parameters"
              Message = "For function "
              Code = "P001"
              Severity = Warning
              Range = r
              Fixes = []}

        )
        |> Seq.toList