# F-Sharp-Analyser
F Sharp"Probable Error" Analysers for Ionide
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
