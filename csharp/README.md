This implementation contains two variants: An idiomatic, short one in [`Search.Plinq.cs`](Search.Plinq.cs) and an ugly, verbose one in [`Search.Binary.cs`](Search.Binary.cs) built for speed.
The binary variant tries to minimize object creation by searching the byte buffer and creating string objects only when "knicks" was found.

Here are some results on my machine:

Language|Time in s
-------------|-----------
C|0.8
C# (Binary)|1.1
C# (PLINQ)|2.8
Erlang (Binary)|4.7
Erlang (Regex)|6.8
Erlang (Unsafe)|3.0
Go (Substring)|8.3
Go (Regex)|19.4
Nim|15.0
Node.js|4.0
Rust (Regex)|1.4
Rust (Substring)|2.4
