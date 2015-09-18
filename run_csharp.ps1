msbuild csharp\Search.sln /t:Rebuild /p:Configuration=Release /noconlog /nologo /v:m
Measure-Command { csharp\bin\Release\Search.exe tmp }
#Measure-Command { csharp\bin\Release\Search.exe tmp -plinq }
