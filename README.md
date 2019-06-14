# F-Sharp-Analyser
* [Wiki](https://github.com/jovanhan2/F-Sharp-Analyser/wiki)
* [Log](https://docs.google.com/spreadsheets/d/1E-dl3aIaSuBxb92qC-840WB-43ynQYcF3fNY1qzTq3g/edit?usp=sharing)
F Sharp"Probable Error" Analysers for Ionide
# Custom analyzers with F#
(On first build run paket restore)

* build and compile into nupkg 
* `dotnet restore `
* `dotnet build`
* `dotnet pack`
* `dotnet pack --output nupkgs`

After packing it unzip the nuget file into the packages directory where you want to test the analyzer. Next change the analyzer path or open the folder only with the analyzer to test

* set ```FSharp.enableAnalyzers```
* set ```FSharp.analyzersPath``` (default is packages/Analyzers)
# Misc
* install paket `dotnet tool install --global Paket --version 5.189.1`
* paket restore
* restart vscode
* Specify which solution to use - `dotnet ef ... -p ThisOne.csproj -s ThisOne.csproj`
where `-p  --project <PROJECT> The project to use.
-s  --startup-project <PROJECT> The startup project to use.`

* dotnet new classlib -lang F#
* dotnet restore
* rem edit the .fsproj file here as per the C# instructions
* dotnet build
* dotnet pack

# Errors
* newest version of ionide 3.34 has errors relatig to updated FSAC
* use ionide 3.33 and disable auto updating 
* requries fsharp.analysers.sdk 0.0.1,  0.0.4 doesn't work
Check ```netcoreapp2.2``` in fsproj matches your netsdk installed.
* Analyser module not found -> check module name matches analyser function
Check Fsharp language server logs and read info
* paket direcotry name/volume label syntax incorrect - obj/debug delete nuspec files
# example context info
```fsharp
ctx: {FileName =
  "e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs";
 Content =
  [|"open System
"; "
"; "let x = None
"; "
"; "[<EntryPoint>]
";
    "let main argv =
"; "    x.Value
"; "    printfn "Hello World from F#!s"
";
    "    0 // return an integer exit code
"; ""|];
 ParseTree =
  ImplFile
    (ParsedImplFileInput
       ("e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs",
        false,QualifiedNameOfFile Program,[],[],
        [SynModuleOrNamespace
           ([Program],false,true,
            [Open
               (LongIdentWithDots ([System],[]),
                e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (1,5--1,11) IsSynthetic=false);
             Let
               (false,
                [Binding
                   (None,NormalBinding,false,false,[],
                    PreXmlDoc
                      ((3,5),Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
                    SynValData
                      (None,SynValInfo ([],SynArgInfo ([],false,None)),None),
                    Named
                      (Wild
                         e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (3,4--3,5) IsSynthetic=false,
                       x,false,None,
                       e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (3,4--3,5) IsSynthetic=false),
                    None,Ident None,
                    e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (3,4--3,5) IsSynthetic=false,
                    SequencePointAtBinding
                      e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (3,0--3,12) IsSynthetic=false)],
                e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (3,0--3,12) IsSynthetic=false);
             Let
               (false,
                [Binding
                   (None,NormalBinding,false,false,
                    [{TypeName = LongIdentWithDots ([EntryPoint],[]);
                      ArgExpr =
                       Const
                         (Unit,
                          e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (5,2--5,12) IsSynthetic=false);
                      Target = None;
                      AppliesToGetterAndSetter = false;
                      Range =
                       e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (5,2--5,12) IsSynthetic=false;}],
                    PreXmlDoc
                      ((6,13),Microsoft.FSharp.Compiler.Ast+XmlDocCollector),
                    SynValData
                      (None,
                       SynValInfo
                         ([[SynArgInfo ([],false,Some argv)]],
                          SynArgInfo ([],false,None)),None),
                    LongIdent
                      (LongIdentWithDots ([main],[]),None,None,
                       Pats
                         [Named
                            (Wild
                               e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (6,9--6,13) IsSynthetic=false,
                             argv,false,None,
                             e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (6,9--6,13) IsSynthetic=false)],
                       None,
                       e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (6,4--6,13) IsSynthetic=false),
                    None,
                    Sequential
                      (SequencePointsAtSeq,true,
                       LongIdent
                         (false,
                          LongIdentWithDots
                            ([x; Value],
                             [e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (7,5--7,6) IsSynthetic=false]),
                          None,
                          e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (7,4--7,11) IsSynthetic=false),
                       Sequential
                         (SequencePointsAtSeq,true,
                          App
                            (NonAtomic,false,Ident printfn,
                             Const
                               (String
                                  ("Hello World from F#!s",
                                   e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (8,12--8,35) IsSynthetic=false),
                                e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (8,12--8,35) IsSynthetic=false),
                             e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (8,4--8,35) IsSynthetic=false),
                          Const
                            (Int32 0,
                             e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (9,4--9,5) IsSynthetic=false),
                          e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (8,4--9,5) IsSynthetic=false),
                       e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (7,4--9,5) IsSynthetic=false),
                    e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (6,4--6,13) IsSynthetic=false,
                    NoSequencePointAtLetBinding)],
                e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (6,0--9,5) IsSynthetic=false)],
            PreXmlDocEmpty,[],None,
            e:\Programming\F-Sharp-Analyser\FSharp.Analyzers.Sample.Usage\Program.fs (1,0--10,0) IsSynthetic=false)],
        (true, true)));
 TypedTree =
  Microsoft.FSharp.Compiler.SourceCodeServices.FSharpImplementationFileContents;
 Symbols = [Program];}

```
# Description
The F# Compiler has a new feature (similar to that in the C# Roslyn compiler) called analysers. This allows intelligent editors
easily to interact with the source code parse tree and provide style and coding help.

This project aim is to implement much more sophisticated stylistic and error hints for F#, especially of use to new programmers,
incorporated into the Ionide plugin and so available as hints in an IDE.

The infrastructure for this is available: https://medium.com/lambda-factory/introducing-f-analyzers-772487889429

The project challenge (apart from a lot of interesting F# coding) is to identify and implement both desirable style guidelines and
"probable errors you cannot easily see" hints for F#. A good example of the latter is unbalanced opening brackets in
expressions. These typically cause errors much later in the code, with obscure error messages. Heuristic hints could be used to
detect when this is probably happening, even though the formal syntax does not show this.

A successful project would result in a high quality open source project add-in to Visual Code that could be used by anyone
programming in F#. It would be very useful.


F# is a fun language to code in, fairly easy to learn, but different in style from procedural languages. This project would be good
for anyone interested in improving (or learning from scratch) their functional programming skills.
