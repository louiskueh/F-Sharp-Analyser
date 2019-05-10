open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups else None  


let testRegex str = 
    match str with
    | FirstRegexGroup "error (FS\d\d\d\d): The type ''a -> 'b' does not match the type '(.*)'" host -> 
           printfn "Found %s Error with type %s" host.[1].Value host.[2].Value
    | _ -> printfn "The value '%s' is something else" str


let input = "error FS0001: The type ''a -> 'b' does not match the type 'int'"
testRegex input
// ==> error FS0001: The type//''a -> 'b' does not match// the type 'int'