module ParenthesisTests
open System.IO
open Expecto
open FSharp.Analyzers.SDK
open ParenthesisAnalyser
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

type Stack = StackContents of char list

// ==============================================
// Stack primitives
// ==============================================

/// Push a value on the stack
let push x (StackContents contents) =   
    StackContents (x::contents)

/// Pop a value from the stack and return it 
/// and the new stack as a tuple
let pop (StackContents contents) = 
    match contents with 
    | top::rest -> 
        let newStack = StackContents rest
        (top,newStack)
    | [] -> 
        failwith "Stack underflow"
let peek stack = 
    let x,_ = pop stack
    x
let EMPTY = StackContents []

// let isBalanced str = 
//     let rec loop xs stack = 
//         match (xs, stack) with
//         // find (, add ( to stack
//         | '(' :: ys,  stack -> loop ys ('(' :: stack)
//         // find ), and (  is already on top
//         | ')' :: ys, '(' :: stack -> loop ys stack
//         // find ), and ( is not on top, therefore error
//         | ')' :: _, _ -> false
//         // any other character loop
//         | _ :: ys, stack -> loop ys stack
//         // both empty then fine
//         | [], [] -> true
//         // empty line and stack is non empty
//         | [], _ -> false
    // loop (Seq.toList str) []

let isBalanced str previousStack= 
    let rec loop xs stack = 
        match (xs, stack) with
        // find (, add ( to stack
        | '(' :: ys,  stack -> loop ys ('(' :: stack)
        // find ), and (  is already on top
        | ')' :: ys, '(' :: stack -> loop ys stack
        // find ), and ( is not on top, therefore error
        | ')' :: _, _ -> (false,stack)
        // any other character loop
        | _ :: ys, stack -> loop ys stack
        // both empty then fine
        | [], [] -> (true,stack)
        // empty line and stack is non empty
        | [], _ -> (false,stack)
    loop (Seq.toList str) previousStack
[<Tests>]  
let tests =
  testList "Parenthesis analyser test" [
      test "Parenthesis Traversal via stack" {
      let contents = [|"let parsenumExpr ls s = 
"; "    Ok ((),s)
";
  "    |> ResExpr (fun _ e -> e)
"; "    |> ResCheckDone
";
  "    |> Result.Bind fun ((_,exp) ->
"; "        eval ls.SymTab expr
";
  "    |> Result.map snd 
"; "     
"; "let hello = 1 "|]

      // let mutable newStack = StackContents []
      // let mutable s = ""
      // contents |> Seq.iter (fun line -> s<- s+ line)
      // printfn "Total is %s" s
      // let res = isBalanced s
      // printfn "%A" res
      let mutable trackStack =  []
      for i in 0..contents.Length do
        printfn "Line Content %d %s" i contents.[i]
        let (balanced,tempStack)= isBalanced contents.[i] trackStack
        trackStack <- tempStack
        if balanced = false then do 
          printfn "Found bracket error at line %d" i
        else if balanced = true then do 
          printfn "Expression is balanced"
    //   line |> Seq.iter (fun char ->
    //     match char with 
    //     | '(' -> 
    //         printfn "FOUND (!"
    //         newStack <- push char newStack
    //     | ')' -> 
    //         match newStack.Length with 
    //         | 0 -> printfn "Empty stack!"
    //         | _ ->
        
    //     printfn "%A" newStack.Length
    //     | _ -> 

        
            
    


      
    //   let firstInput = "let add x y = x + y"
    //   let GenerateInput firstInput nums = 
    //     let s = "let result = add"    
    //     let mutable addon = ""
    //     for i in 1..nums do
    //       addon <- addon + " 1"
    //     firstInput + "\n" + s + addon + "\n"
    //   let runInputWithAnalyser input = 
    //     let checkProjectResults = parseAndCheckSingleFile(input)
    //     let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
    //     let inputStringArray = input.Split "\n"
    //     let tree = getUntypedTree(file, input) 
    //     let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
    //     ParenthesisAnalyser mockContext
      

    }
    // test "Statistical test cases" {
    //   let firstInput = "let add x y = x + y"
    //   let GenerateInput firstInput nums = 
    //     let s = "let result = add"    
    //     let mutable addon = ""
    //     for i in 1..nums do
    //       addon <- addon + " 1"
    //     firstInput + "\n" + s + addon + "\n"
    //   let runInputWithAnalyser input = 
    //     let checkProjectResults = parseAndCheckSingleFile(input)
    //     let typeTree = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
    //     let inputStringArray = input.Split "\n"
    //     let tree = getUntypedTree(file, input) 
    //     let mockContext:Context = {FileName=""; Content=inputStringArray; ParseTree=tree; TypedTree= typeTree;Symbols=[] }
    //     ParenthesisAnalyser mockContext
      
    //   // generate random number to insert parameter
    //   for i in 1..10 do
    //     let r = System.Random()
    //     let nums = r.Next(1, 10)
    //     let input = GenerateInput firstInput nums
    //     // printfn "obtained a random number %A" nums
    //     // printfn "Input %A"  input
    //     let result = runInputWithAnalyser input
    //     if nums >2 then 
    //       let expectedResultMessage= "For function add, which expects 2 arguments "
    //       let rangeResult = result.[0].Range
    //       let expectedRange = "(2,13--2,18)"
    //       Expect.equal  (rangeResult.ToShortString()) expectedRange "For code that exceeds parameters"
    //       Expect.equal (result.[0].Message) expectedResultMessage "For code that exceeds parameters"
    //       // printfn "Correct range "
    //     else 
    //       Expect.equal result.IsEmpty  true  "Should not have any output for correct code"
    //       // printfn "no output for correct code"
    // }

  ]
  |> testLabel "Incorrect Parameter Tests"


