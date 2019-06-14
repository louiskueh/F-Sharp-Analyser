module ParenthesisAnalyser

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast


let rec visitExpression handler= 
    function 
    | SynExpr.Paren(expr, lParen, rParen, rangeInclParen) -> 
        match expr with 
        // matches starting (  and then error
        | SynExpr.FromParseError(expr, range) ->
           handler range expr
        | _ -> ()
    | SynExpr.FromParseError(expr,range) ->
        printfn "Detected parse error with expr %A and range %A " expr range
        visitExpression handler expr
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, m) -> 
        visitExpression handler funcExpr
        visitExpression handler argExpr
    | _ -> ()
        


let visitDeclarations handler decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, 
                             pat, retInfo, body, m, sp)) = binding
                visitExpression handler body
        | _ -> ()

let visitModulesAndNamespaces handler modulesOrNss = 
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = 
        modulesOrNss
    visitDeclarations handler decls




[<Analyzer>]
let ParenthesisAnalyser : Analyzer =
    printfn "Inside Parenthesis analyzer!"
    
    fun ctx ->
        printfn "ctx %A" ctx.Content
        let state = ResizeArray<range>()
        // handler adds the range to display
        let handler (range: range) (m: SynExpr) =
            printfn "###################################"
            printfn "SynExpr type %A" m
            printfn "###################################"
            state.Add range
        let parseTree = ctx.ParseTree
        match parseTree with
        | ParsedInput.ImplFile(implFile) ->
            // Extract declarations and walk over them
            let (ParsedImplFileInput(fn, script, name, _, _, modules, _)) = implFile
            modules |>  List.iter (visitModulesAndNamespaces handler)
            // visitModulesAndNamespaces modules
        | _ -> failwith "F# Interface file (*.fsi) not supported."
        // printfn "STATE IS %A" state
        state
        |> Seq.map (fun r ->
            printfn "range added is: %A " r
            { Type = "Parenthesis Analyser"
              Message = "Possible bracket error"
              Code = "P001"
              Severity = Warning
              Range = r
              Fixes = []}

        )
        |> Seq.toList