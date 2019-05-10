module TooManyArgs1C
open System
open FSharp.Analyzers.SDK
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range
open System.IO
open System.Text.RegularExpressions
open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups else None  




let rec visitExpr memberCallHandler (e:FSharpExpr) =
    match e with
    | BasicPatterns.AddressOf(lvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
        visitExpr memberCallHandler lvalueExpr; visitExpr memberCallHandler rvalueExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
        visitExpr memberCallHandler funcExpr; visitExprs memberCallHandler argExprs
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->
        // foundType e.Range typeArgs2 state
        memberCallHandler e.Range memberOrFunc
        visitObjArg memberCallHandler objExprOpt; visitExprs memberCallHandler argExprs
    | BasicPatterns.Coerce(targetType, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
        visitExpr memberCallHandler startExpr; visitExpr memberCallHandler limitExpr; visitExpr memberCallHandler consumeExpr
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler thenExpr; visitExpr memberCallHandler elseExpr
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
        visitExpr memberCallHandler bindingExpr; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
        List.iter (snd >> visitExpr memberCallHandler) recursiveBindings; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.NewArray(arrayType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) ->
        visitExpr memberCallHandler delegateBodyExpr
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewRecord(recordType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewTuple(tupleType, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.Quote(quotedExpr) ->
        visitExpr memberCallHandler quotedExpr
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
        visitObjArg memberCallHandler objExprOpt
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) ->
        visitObjArg memberCallHandler objExprOpt; visitExpr memberCallHandler argExpr
    | BasicPatterns.Sequential(firstExpr, secondExpr) ->
        visitExpr memberCallHandler firstExpr; visitExpr memberCallHandler secondExpr
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler finalizeExpr
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) ->
        visitExpr memberCallHandler bodyExpr; visitExpr memberCallHandler catchExpr
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
        visitExpr memberCallHandler tupleExpr
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        visitExpr memberCallHandler decisionExpr; List.iter (snd >> visitExpr memberCallHandler) decisionTargets
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
        visitExprs memberCallHandler decisionTargetExprs
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) ->
        visitExpr memberCallHandler bodyExpr
    | BasicPatterns.TypeTest(ty, inpExpr) ->
        visitExpr memberCallHandler inpExpr
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->
        visitExpr memberCallHandler unionExpr; visitExpr memberCallHandler valueExpr
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) ->
        visitExpr memberCallHandler unionExpr
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) ->
        visitExpr memberCallHandler baseCallExpr
        List.iter (visitObjMember memberCallHandler) overrides
        List.iter (snd >> List.iter (visitObjMember memberCallHandler)) interfaceImplementations
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
        visitExprs memberCallHandler argExprs
    | BasicPatterns.ValueSet(valToSet, valueExpr) ->
        visitExpr memberCallHandler valueExpr
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        visitExpr memberCallHandler guardExpr; visitExpr memberCallHandler bodyExpr
    | BasicPatterns.BaseValue baseType -> ()
    | BasicPatterns.DefaultValue defaultType -> ()
    | BasicPatterns.ThisValue thisType -> ()
    | BasicPatterns.Const(constValueObj, constType) -> ()
    | BasicPatterns.Value(valueToGet) -> ()
    | _ -> ()

and visitExprs f exprs =
    List.iter (visitExpr f) exprs

and visitObjArg f objOpt =
    Option.iter (visitExpr f) objOpt

and visitObjMember f memb =
    visitExpr f memb.Body

let rec visitDeclaration f d =
    match d with
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        for subDecl in subDecls do
            visitDeclaration f subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
        visitExpr f e
    | FSharpImplementationFileDeclaration.InitAction(e) ->
        visitExpr f e

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
let testRegex str = 
    match str with
     // | FirstRegexGroup "error (FS\d\d\d\d): The type ''a -> 'b' does not match the type '(.*)'" host -> 
    | FirstRegexGroup "The type '(.*) -> (.*)' does not match the type '(.*)'" host -> 
        //    printfn "Found %s Error with type %s" host.[1].Value host.[2].Value
           "FS0001"
        //    (host.[1].Value,host.[2].Value, host.[3].Value)
    | _ -> ""
        // ("","", "")
        //    printfn "The value '%s' is something else" str
        //    The type 'int -> obj' does not match the type 'int'

[<Analyzer>]
let TooManyArgs1C : Analyzer =
    printfn "1CTooManyArgs Analyzer"
    fun ctx ->
        printfn "CTX string %A" ctx
        let state = ResizeArray<range>()
        let string = ctx.Content |> String.concat "\n"
        let checkProjectResults = parseAndCheckSingleFile(string)
        // printfn "Errors: %A" checkProjectResults.Errors
        for x in checkProjectResults.Errors do
            // printfn "Error info %A" x
            // printfn "Error Number %i" x.ErrorNumber
            // printfn "Start column %i" x.StartColumn
            // printfn "end column %i" x.EndColumn
            // printfn "StartLine alternate %i" x.StartLineAlternate
            // printfn "EndLine alternate %i" x.EndLineAlternate
            // printfn "Error message %s" x.Message
            let t = testRegex x.Message
            // printfn "2message: \"The type 'int' does not support the operator 'DivideByInt'\"" 
            if x.Subcategory = "typecheck" && t = "FS0001" then
                printfn "Inside FS0001"
                let handler (range: range) (m: FSharpMemberOrFunctionOrValue) = 
                    printfn "m %A" m
                    printfn "Parameters %A" m.GenericParameters
                    printfn "FullType %A" m.FullType
                    printfn "Curried Parameter Groups %A" m.CurriedParameterGroups
                    printfn "range %A" range
                    printfn "typedTreeDeclarations %A" ctx.TypedTree.Declarations 
                    // if m.ToString() = "val op_Range" then
                    //     state.Add range
                    //     printfn"Added op_range %A" range
                ctx.TypedTree.Declarations |> List.iter (visitDeclaration handler)
        state
        |> Seq.map (fun r ->
            { Type = "MismatchedTypes Analyzer"
              Message = "Using List Operations usually requires float instead of int"
              Code = "A"
              Severity = Warning
              Range = r
              Fixes = []}

        )
        |> Seq.toList