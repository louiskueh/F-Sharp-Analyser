module OperatorPrecedenceTests
open System.IO
open Expecto
open FSharp.Analyzers.SDK
open OperatorPrecedence
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range


let checker = FSharpChecker.Create(keepAssemblyContents=true)
/// Get untyped tree for a specified input
let getUntypedTree(file, input) = 
    // Get compiler options for the 'project' implied by a single script file
    let projectOptions, _errors = 
        checker.GetProjectOptionsFromScript(file, input) 
        |> Async.RunSynchronously
    let parsingOptions, _errors = 
        checker.GetParsingOptionsFromProjectOptions(projectOptions)
    // Run the first phase (untyped parsing) of the compiler
    let parseFileResults = 
        checker.ParseFile(file, input, parsingOptions) |> Async.RunSynchronously
    match parseFileResults.ParseTree with
    | Some tree -> tree
    | None -> failwith "Something went wrong during parsing!"

let file = "/home/user/Test.fsx"
//get typed tree
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


[<Tests>]  
let tests =
  testList "Operator Precedence test" [
    test "Error + Case Infix test" {
      let input = """let result = 1 +3
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }

      let result = OperatorPrecedence mockContext
      // printfn "Result is %A" result
      let expectedResult = "Try adding a space between + and 3"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }
    test "No error + Case Infix test" {
      let input = """let result = 1 + 3
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }

      let result = OperatorPrecedence mockContext

      // Expect.equal result.[0].Range 
      Expect.equal result  [] "No expected match for false positives"
    }
    test "Error Case Infix test" {
      let input = """let x = 1 -2
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }

      let result = OperatorPrecedence mockContext
      let expectedResult = "Try adding a space between - and 2"
      let Startposition = mkPos (1) 13
      let EndPosition = mkPos (1) 18
      let range = mkRange "" Startposition EndPosition
      // Expect.equal result.[0].Range 
      Expect.equal result.[0].Message expectedResult "Expected Message should match"
    }
    test "No error - Case Infix test" {
      let input = """let x = 1 - 2
    """
      // Generate Mock context for source code
      let inputStringArray = input.Split "\n"
      let checkProjectResults = parseAndCheckSingleFile(input)
      let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
      let tree = getUntypedTree(file, input) 
      let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
      let result = OperatorPrecedence mockContext
      Expect.equal result  [] "No expected match for false positives"
    }
    // test "Simple test cases" {
    //       let firstInput = "let add x y = x + y"
    //       let GenerateInput firstInput nums = 
    //         let s = "let result = add"    
    //         let mutable addon = ""
    //         for i in 1..nums do
    //           addon <- addon + " 1"
    //         firstInput + "\n" + s + addon + "\n"
    //       let runInputWithAnalyser input = 
    //         let checkProjectResults = parseAndCheckSingleFile(input)
    //         let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
    //         let inputStringArray = input.Split "\n"
    //         let tree = getUntypedTree(file, input) 
    //         let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
    //         OperatorPrecedence mockContext
          
    //       // generate random number to insert parameter
    //       for i in 1..10 do
    //         let r = System.Random()
    //         let nums = r.Next(1, 10)
    //         let input = GenerateInput firstInput nums
    //         // printfn "obtained a random number %A" nums
    //         // printfn "Input %A"  input
    //         let result = runInputWithAnalyser input
    //         if nums >2 then 
    //           let expectedResultMessage= "For function add, which expects 2 arguments "
    //           let rangeResult = result.[0].Range
    //           let expectedRange = "(2,13--2,18)"
    //           Expect.equal  (rangeResult.ToShortString()) expectedRange "For code that exceeds parameters"
    //           Expect.equal (result.[0].Message) expectedResultMessage "For code that exceeds parameters"
    //           // printfn "Correct range "
    //         else 
    //           Expect.equal result.IsEmpty  true  "Should not have any output for correct code"
    //           // printfn "no output for correct code"
    //     }


  ]
  |> testLabel "Operator Precedence Tests"


