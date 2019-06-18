module OperatorPrecedence

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open System.IO
let mutable functionNames = Set.empty

let rec visitExpression handler body= 
    match body with
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, range) -> 
        // printfn "In APP with funcExpr %A | argExpr %A" funcExpr argExpr
        // match funcExpr with 
        // | SynExpr.Ident(name) -> 
        //     functionNames <- functionNames.Add(name.ToString())
        //     // printfn "Found name %A" (name.ToString())
        // | SynExpr.LongIdent(op,ident,syn,range)->
        //     let names = String.concat "." [for i in ident.Lid -> i.idText]
        //     // printfn "LongIdent %A " names
        //     functionNames <- functionNames.Add(names)
        // | _ -> ()
        visitExpression handler funcExpr
        visitExpression handler argExpr
    | SynExpr.LetOrUse(isRecurisve,isUse,bindings,body,range) ->()
        // printfn "Let isUse %A" isUse
        // printfn "Bindings %A" bindings
        // printfn "Body %A" body
        // printfn "range %A" range
    | SynExpr.Ident(name) -> 
        functionNames <- functionNames.Add(name.ToString())
    // printfn "Found name %A" (name.ToString())
    | SynExpr.LongIdent(op,ident,syn,range)->
        let names = String.concat "." [for i in ident.Lid -> i.idText]
        // printfn "LongIdent %A " names
        functionNames <- functionNames.Add(names)

    | _ -> ()


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
        functionNames <- functionNames.Add(names)
        // identifier is name of function call 
        // let add x y =... -> add
        
        ()
    | _ -> ()
// save function calls
let visitDeclarations handler decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, 
                             pat, retInfo, body, m, sp)) = binding
                
                visitPattern pat data
                visitExpression handler body
        | _ -> ()



let visitModulesAndNamespaces handler modulesOrNss = 
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = 
        modulesOrNss
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

let checkPrefixSpacing (ctx:Context) (state:ResizeArray<(range *string )>) (error:FSharpErrorInfo) =
    // printfn "Errors %A " error
    // printfn "Error line: %A - %A " error.StartLineAlternate error.EndLineAlternate
    let contents = ctx.Content
    if error.Message = "This value is not a function and cannot be applied." then do 
        // if on same line
        if error.StartLineAlternate = error.EndLineAlternate then do 
            let codeToCheck = contents.[error.StartLineAlternate - 1]
            let (result,(char,prefix)) = checkPrefix codeToCheck []
            if result = false then do 
                let Startposition = mkPos (error.StartLineAlternate) error.StartColumn
                let EndPosition = mkPos (error.EndLineAlternate) codeToCheck.Length
                let range = mkRange ctx.FileName Startposition EndPosition
                // printfn "Result = %A, if false there is no space between + / - error! " result    
                state.Add (range,"Try adding a space between " +  prefix.ToString()  + " and " + char.ToString())
                // printfn "added to state"

// Iterate through function names and see if code contains them
let checkFunctionCalled (line:string)  = 
    let result= ResizeArray<string>()
    Set.iter  (fun name -> 
        if line.Contains (name) then do 
            result.Add name
    ) functionNames
    result

// check Function Names
let main (ctx:Context) (state:ResizeArray<(range *string )>) (error:FSharpErrorInfo)  = 
    // printfn "error %A " (error.ToString())
    let contents = ctx.Content
    // if error is on one line
    if error.StartLineAlternate = error.EndLineAlternate then do 
        let codeToCheck = contents.[error.StartLineAlternate - 1]
        // check if function name is called
        let functionCallNames = checkFunctionCalled codeToCheck
        if functionCallNames.Count > 0 then do 
            printfn "Found function calls in line %d, they are %A" error.StartLineAlternate functionCallNames
            // if contains plus most likely operator error
            if codeToCheck.Contains("+") then 
                let possibleFunctionCalls = String.concat "." [for i in functionCallNames -> i]
                let Startposition = mkPos (error.StartLineAlternate) error.StartColumn
                let EndPosition = mkPos (error.EndLineAlternate) error.EndColumn
                let range = mkRange ctx.FileName Startposition EndPosition
                state.Add (range,"The arguments for function \"" + possibleFunctionCalls   + "\" may need brackets to define arguments near character: \"" + "+\"")
                // printfn "Found +, added to state"

        else
            // printfn "Found no function calls, doing prefix check"
            checkPrefixSpacing ctx state error
    
    


[<Analyzer>]
let OperatorPrecedence : Analyzer  =
    functionNames <- Set.empty
    fun ctx ->
        // printfn "ctxParseTree %A" ctx.ParseTree
        // printfn "ctxTypedTree  %A" ctx.TypedTree 
        let state = ResizeArray<(range *string)>()
        let string = ctx.Content |> String.concat "\n"
        let checkProjectResults = parseAndCheckSingleFile(string)
        // printfn "Errors: %A" checkProjectResults.Errors
        
        if checkProjectResults.Errors.Length > 0 then
            match ctx.ParseTree with
            | ParsedInput.ImplFile(implFile) ->
                // Extract declarations and walk over them
                let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
                modules |>  List.iter (visitModulesAndNamespaces ())
            | _ -> failwith "F# Interface file (*.fsi) not supported."
            // printfn "Function names found are %A " functionNames
            // printfn "Number of errors %d" checkProjectResults.Errors.Length
            // check prefix errors
            // checkProjectResults.Errors |> Array.iter (fun error -> checkPrefixSpacing ctx state error ) 
            checkProjectResults.Errors |> Array.iter (main ctx state)
                    
        // printfn "function names are %A" functionNames
        
        state
        |> Seq.map (fun (range,message) ->
            { Type = "Operator Precedence Analyser "
              Message = message
              Code = "P001"
              Severity = Warning
              Range = range
              Fixes = []}

        )
        |> Seq.toList