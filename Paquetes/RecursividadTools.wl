(* ::Package:: *)

BeginPackage["RecursividadTools`"]

MostrarLlamadasRecursivas::usage = "MostrarLlamadasRecursivas[func_] registra y muestra las llamadas de una funci\[OAcute]n recursiva."
FactorialPila::usage = "FactorialPila[n] devuelve el factorial de n utilizando recursi\[OAcute]n de pila."
FactorialCola::usage = "FactorialCola[n] devuelve el factorial de n utilizando recursi\[OAcute]n de cola."
FibonacciPropio::usage = "Fibonacci[n] devuelve el n-\[EAcute]simo n\[UAcute]mero de Fibonacci usando recursi\[OAcute]n."

Begin["`Private`"]

(* Registro y visualizaci\[OAcute]n de llamadas *)
MostrarLlamadasRecursivas[func_] := 
  DynamicModule[{llamadas = {}}, 
   Module[{traza},
    traza[x___] := (
      AppendTo[llamadas, "Llamada: " <> ToString[{x}]];
      func[x]
    );
    Column[{
      "Resultado: " <> ToString[traza[]],
      "Llamadas:",
      Column[llamadas]
    }]
   ]
  ]

(* Factorial con recursi\[OAcute]n de pila *)
FactorialPila[n_] := 
 Module[{registro = {}}, 
  If[n == 0, 1, AppendTo[registro, n]; n * FactorialPila[n - 1]]
 ]

(* Factorial con recursi\[OAcute]n de cola *)
FactorialCola[n_] := 
 Module[{helper}, 
  helper[x_, acc_] := If[x == 0, acc, helper[x - 1, acc * x]];
  helper[n, 1]
 ]

(* Fibonacci *)
FibonacciPropio[n_] := 
 If[n == 0, 0, If[n == 1, 1, FibonacciPropio[n - 1] + FibonacciPropio[n - 2]]]

End[]
EndPackage[]



