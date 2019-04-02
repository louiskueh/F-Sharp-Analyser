module SampleAnalyzer

open System
open FSharp.Analyzers.SDK
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast


let rec visitExpression handler= 
    function 
    | SynExpr.Paren(expr, lParen, rParen, rangeInclParen) -> 
        match expr with 
        | SynExpr.FromParseError(expr, range) ->
           handler range expr
           Some range
        | _ -> None
    | _-> None
        


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
let paranthesis : Analyzer =
    fun ctx ->
        let state = ResizeArray<range>()
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
            { Type = "Option.Value analyzer"
              Message = "Possible bracket error"
              Code = "OV001"
              Severity = Warning
              Range = r
              Fixes = []}

        )
        |> Seq.toList