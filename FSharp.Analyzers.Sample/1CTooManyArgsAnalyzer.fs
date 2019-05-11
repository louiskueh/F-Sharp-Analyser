module ToomanyArgs

open FSharp.Analyzers.SDK
// open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast


let rec visitExpression handler= 
    function 
    | SynExpr.App(exprAtomicFlag, isInfix, funcExpr, argExpr, m) -> 
        visitExpression handler funcExpr
        visitExpression handler argExpr
    | SynExpr.LetOrUse(isRecurisve,isUse,bindings,body,range) ->
        printfn "Let isUse %A" isUse
        printfn "Bindings %A" bindings
        printfn "Body %A" body
        printfn "range %A" range
    | _ -> printfn "unmatched!"
        


let visitDeclarations handler decls = 
    for declaration in decls do
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) -> 
            for binding in bindings do
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, 
                             pat, retInfo, body, m, sp)) = binding
                printfn "in visitExpression Let"
                visitExpression handler body
        | _ -> ()

let visitModulesAndNamespaces handler modulesOrNss = 
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = 
        modulesOrNss
    printfn "Namespace or module: %A" lid
    visitDeclarations handler decls




[<Analyzer>]
let paranthesis : Analyzer =
    printfn "Inside tooMany args!"
    
    fun ctx ->
        printfn "ctx %A" ctx.ParseTree
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
        // parseTree.
        state
        |> Seq.map (fun r ->
            { Type = "Parenthesis Analyser"
              Message = "Possible bracket error"
              Code = "P001"
              Severity = Warning
              Range = r
              Fixes = []}

        )
        |> Seq.toList