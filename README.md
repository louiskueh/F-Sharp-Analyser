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

Check Fsharp language server logs and read info
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
