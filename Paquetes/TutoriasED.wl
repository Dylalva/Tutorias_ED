(* ::Package:: *)

BeginPackage["TutoriasED`"]
Print[Style["Se ha cargado el paquete TutoriasED. USO PARA ESTUDIO. Versi\[OAcute]n 23/03/2025", Green]];
FactorialPilaED::usage =
"FactorialPilaED[n, show -> True|False] \
Devuelve el factorial de n usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

FibonacciPilaED::usage =
"FibonacciPilaED[n, show -> True|False] \
Devuelve el n-\[EAcute]simo n\[UAcute]mero de la serie de Fibonacci usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla mediante un grafo.";

PotenciaPilaED::usage =
"PotenciaPilaED[a, b, show -> True|False] \
Calcula a^b (a elevado a b) usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

SumatoriaPilaED::usage =
"SumatoriaPilaED[n, show -> True|False] \
Devuelve la sumatoria 1 + 2 + ... + n usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";


(*Funciones recursivas de cola*)
FactorialColaED::usage =
"FactorialColaED[n, show -> True|False] \
Devuelve el factorial de n usando recursi\[OAcute]n de cola (tail recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

PotenciaColaED::usage =
"PotenciaColaED[a, b, show -> True|False] \
Calcula a^b (a elevado a b) usando recursi\[OAcute]n de cola (tail recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

FibonacciColaED::usage =
"FibonacciColaED[n, show -> True|False] \
Devuelve el n-\[EAcute]simo n\[UAcute]mero de la serie de Fibonacci usando recursi\[OAcute]n de cola (tail recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

SumatoriaColaED::usage =
"SumatoriaColaED[n, show -> True|False] \
Devuelve la sumatoria 1 + 2 + ... + n usando recursi\[OAcute]n de cola (tail recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

RecursiveFactorial::usage = "RecursiveFactorial[n] calcula el factorial usando recursi\[OAcute]n de pila.";

TailRecursiveFactorial::usage = "TailRecursiveFactorial[n] calcula el factorial usando recursi\[OAcute]n de cola.";

TraceRecursiveFactorial::usage = "TraceRecursiveFactorial[n] devuelve la secuencia de llamadas en 
recursi\[OAcute]n de pila.";

TraceTailRecursiveFactorial::usage = "TraceTailRecursiveFactorial[n] devuelve la secuencia de llamadas en recursi\[OAcute]n 
de cola.";

PlotRecursionTree::usage = "PlotRecursionTree[n, type] grafica el \[AAcute]rbol de llamadas recursivas para 'stack' (pila) 
o 'tail' (cola).";

(*Funciones auxiliares*)
mostrarLlamadasED::usage =
"mostrarLlamadasED[func1_, func2_] \
Crea una interfaz con Manipulate que, al cambiar el valor de n, \
muestra a la par los resultados de las dos funciones enviadas como par\[AAcute]metros.";
ExplicacionFibonacciED::usage =
"";

ExplicacionFactorialED::usage =
"ExplicacionFactorialED[] \
Imprime la relaci\[OAcute]n de recurrencia del factorial en la pantalla.";

TablaFuncED::usage =
"TablaFactorialED[n, func] \
Muestra una tabla de {n, funcion(n)} desde 0 hasta n.";

GraficaFuncED::usage =
"GraficaFactorialED[n, func] \
Hace un ListPlot de los valores de funcion(n) para n=0..n.";

EvaluaFuncion::usage =
"EvaluaFuncion[fun1, fun2, max] o bien EvaluaFuncion[fun1, fun2, max, inicio->x] \ 
Esta funcion recibe 2 funciones para ver si dan el mimso resultado, la cantidad de comparaciones sera de inicio
el cual por default es uno hasta max.
"

(* Declaraciones de uso p\[UAcute]blico *)
PlotFactorialRecursionTree::usage = "PlotFactorialRecursionTree[n] muestra el \[AAcute]rbol de llamadas del factorial con recursi\[OAcute]n de pila.";
PlotTailFactorialRecursionTree::usage = "PlotTailFactorialRecursionTree[n] muestra el \[AAcute]rbol de llamadas del factorial con recursi\[OAcute]n de cola.";

(*-----------------------------------------REL DE RECURRENCIA----------------------------------------------*)

(*Analisis de algoritmos*)
GraficoTiempos::usage = 
  "GraficoTiempos[funciones, pruebas, Options] genera un gr\[AAcute]fico que muestra los tiempos de ejecuci\[OAcute]n \
de cada funci\[OAcute]n evaluada para valores de n en el rango [inicio, pruebas]. La opci\[OAcute]n 'inicio' es opcional (default 1).";

OrdenFunciones::usage = 
  "OrdenFunciones[funciones, pruebas, Options] eval\[UAcute]a las funciones para valores de n en [inicio, pruebas], \
calcula el tiempo promedio de ejecuci\[OAcute]n de cada una y muestra en pantalla el orden de rapidez.";

bigO::usage = 
"bigO[f, g, n] devuelve el l\[IAcute]mite de f(n)/g(n) conforme n->\[Infinity], para verificar O().";
bigOmega::usage = 
"bigOmega[f, g, n] devuelve el l\[IAcute]mite de g(n)/f(n) conforme n->\[Infinity], para verificar \[CapitalOmega]().";
bigTheta::usage = 
"bigTheta[f, g, n] verifica si f(n) es \[CapitalTheta](g(n)) al verificar ambos l\[IAcute]mites (O y \[CapitalOmega]).";

PlotAsymptoticFunctions::usage = 
 "PlotAsymptoticFunctions[{xmin, xmax}] grafica un conjunto de funciones asint\[OAcute]ticas comunes \
en una escala logar\[IAcute]tmica en ambos ejes. Las funciones graficadas son: \
1, Log[n], n, Log[Log[n]], n^n, n^2 y n^3.";
(*-------------------------------------------------------------------------------------------------------*)
Begin["`Private`"]
(*Opciones de cada funci\[OAcute]n*)
Options[FactorialPilaED]={show->False};
Options[FactorialColaED]={show->False};
Options[PotenciaPilaED] = {show -> False};
Options[PotenciaColaED] = {show -> False};
Options[FibonacciColaED] = {show -> False};
Options[FibonacciPilaED] = {show -> False};
Options[SumatoriaPilaED] = {show -> False};
Options[SumatoriaColaED] = {show -> False};
(*----------------------------------------------Recursividad----------------------------------------------*)
FactorialPilaED[n_,OptionsPattern[]]:=Module[{toShow=OptionValue[show],resultado},
	If[n<=1,If[toShow,Print["Caso Base"]];
	1,If[toShow,Print["Factorial pila: "<>ToString[n]]];
	resultado=n * FactorialPilaED[n-1,show->toShow];
	If[toShow,Print["Factorial pila listo "<>ToString[n]<>": "<>ToString[resultado]]];
	resultado]];

FactorialColaED[n_,OptionsPattern[]]:=Module[{funcAux,toShow=OptionValue[show]},
	funcAux[x_,acc_]:=If[x==0, If[toShow,Print["Caso Base!"]];
	acc, If[toShow,Print["Factorial cola: "<>ToString[x]]];
	With[{nuevoAcc=x*acc},With[{resultadoRec=funcAux[x-1,nuevoAcc]},
	If[toShow,Print["Factorial cola listo de "<>ToString[x]<>" Resultado: "<>ToString[resultadoRec]]];
	resultadoRec]]];
	funcAux[n,1]];

PotenciaPilaED[a_, b_, OptionsPattern[]] :=
 Module[{toShow = OptionValue[show], result},
  If[b == 0,
   If[toShow, Print["Caso Base!"]];
   1
   ,
   If[toShow, Print["Potencia Pila (" <> ToString[a] <> "^" <> ToString[b] <> ")"]];
   result = a * PotenciaPilaED[a, b - 1, show -> toShow];
   If[toShow,
    Print[
     "Potencia Pila listo para " <> ToString[a] <> "^" <> ToString[b] <>
      ": " <> ToString[result]
    ]
   ];
   result
  ]
 ]


SumatoriaPilaED[n_, OptionsPattern[]] := Module[{toShow = OptionValue[show], resultado},
  If[n <= 0,
    If[toShow, Print["Caso Base"]];
    0
    ,
    If[toShow, Print["Sumatoria Pila: " <> ToString[n]]];
    resultado = n + SumatoriaPilaED[n - 1, show -> toShow];
    If[toShow, Print["Sumatoria Pila listo " <> ToString[n] <> ": " <> ToString[resultado]]];
    resultado
  ]
];

SumatoriaColaED[n_, OptionsPattern[]] := Module[{funcAux, toShow = OptionValue[show]},
  funcAux[x_, acc_] :=
   If[x == 0,
     If[toShow, Print["Caso Base"]];
     acc
     ,
     If[toShow, Print["Sumatoria Cola: " <> ToString[x]]];
     With[{res = funcAux[x - 1, x + acc]},
       If[toShow, Print["Sumatoria Cola listo " <> ToString[x] <> ": " <> ToString[acc]]];
       res
     ]
   ];
  funcAux[n, 0]
];

PotenciaColaED[a_, b_, OptionsPattern[]] :=
 Module[{funcAux, toShow = OptionValue[show]},
  funcAux[x_, y_, acc_] :=
   If[y == 0,
    If[toShow, Print["Caso Base!"]];
    acc
    ,
    If[toShow, Print["Potencia Cola (" <> ToString[x] <> "^" <> ToString[y] <> ")"]];
    With[{res = funcAux[x, y - 1, x * acc]},
     If[toShow,
      Print[
       "Potencia Cola listo para " <> ToString[x] <> "^" <> ToString[y] <>
        ": " <> ToString[res]
      ]
     ];
     res
    ]
   ];
  funcAux[a, b, 1]
 ]

Options[FibonacciColaED] = {show -> False};

FibonacciColaED[n_, OptionsPattern[]] := Module[
  {edges = {}, labels = {}, colors = <||>, callCounter = 0, toShow = OptionValue[show], funcAux, fibValue},
  
  (* Funci\[OAcute]n auxiliar con acumulador y registro gr\[AAcute]fico *)
  funcAux[x_, a_, b_, parentID_: None] := Module[{currentID, res},
    currentID = callCounter;
    callCounter++;
    
    (* Registro de la llamada: se etiqueta con los par\[AAcute]metros actuales *)
    labels[currentID] = "FibCola(" <> ToString[x] <> ", " <> ToString[a] <> ", " <> ToString[b] <> ")";
    colors[currentID] = If[x == 0, White, LightBlue];
    
    (* Conectar con la llamada anterior, si existe *)
    If[parentID =!= None, AppendTo[edges, parentID -> currentID]];
    
    (* Proceso de la recursi\[OAcute]n *)
    If[x == 0,
      If[toShow, Print["[FibCola] Caso base: fib(0) = ", a]];
      a,
      If[toShow, Print["[FibCola] Calculando fib(", x, ") con a = ", a, " y b = ", b]];
      res = funcAux[x - 1, b, a + b, currentID];
      If[toShow, Print["[FibCola] fib(", x, ") listo: ", res]];
      res
    ]
  ];
  
  (* C\[AAcute]lculo del valor de Fibonacci y registro de la cadena de llamadas *)
  fibValue = funcAux[n, 0, 1];
  
  (* Construcci\[OAcute]n del grafo explicativo *)
  Module[{grafo},
    grafo = Graph[
      edges,
      VertexLabels -> labels,
      VertexStyle -> Normal[colors],
      GraphStyle -> "NameLabeled",
      GraphLayout -> "LayeredDigraphEmbedding"
    ];
    If[toShow, Print[grafo]];
  ];
  
  fibValue
];

(* Recursi\[OAcute]n de Pila (Normal) *)
RecursiveFactorial[n_] := If[n == 0, 1, n * RecursiveFactorial[n - 1]];

(* Recursi\[OAcute]n de Cola *)
TailRecursiveFactorial[n_] := Module[{factTail},
  factTail[k_, acc_] := If[k == 0, acc, factTail[k - 1, k * acc]];
  factTail[n, 1]
];

(* Trazado de Recursi\[OAcute]n de Pila *)
TraceRecursiveFactorial[n_] := Module[{trace = {}},
  TraceScan[(AppendTo[trace, #1]) &, RecursiveFactorial[n], _RecursiveFactorial];
  trace
];

(* Trazado de Recursi\[OAcute]n de Cola *)
TraceTailRecursiveFactorial[n_] := Module[{trace = {}},
  TraceScan[(AppendTo[trace, #1]) &, TailRecursiveFactorial[n], _TailRecursiveFactorial];
  trace
];

(* Generaci\[OAcute]n de \[CapitalAAcute]rbol de Llamadas *)
PlotRecursionTree[n_, type_: "stack"] := Module[{edges = {}, recursiveCall},

  (* Definir la funci\[OAcute]n que genera los nodos y aristas *)
  recursiveCall[k_, parent_] := If[k >= 0,
    AppendTo[edges, parent -> k]; (* Guardar la relaci\[OAcute]n padre-hijo *)
    If[type == "stack",
      recursiveCall[k - 1, k], (* Recursi\[OAcute]n normal *)
      recursiveCall[k - 1, k * parent] (* Recursi\[OAcute]n de cola con acumulador *)
    ];
  ];

  (* Iniciar el \[AAcute]rbol con la llamada inicial *)
  recursiveCall[n, "Start"];

  (* Graficar el \[AAcute]rbol *)
  Graph[edges, VertexLabels -> "Name", GraphStyle -> "Tree", ImageSize -> Large]
];



FibonacciPilaED[n_, OptionsPattern[]] := 
  Module[{edges = {}, labels = {}, colors = <||>, memo = <||>, FibonacciPilaAux, grafo, toShow, fibValue},

   (* Obtener la opci\[OAcute]n show *)
   toShow = OptionValue[show];

   (* Funci\[OAcute]n auxiliar *)
   FibonacciPilaAux[k_] := Module[{left, right},
     If[KeyExistsQ[memo, k], colors[k] = Red; Return[memo[k]]];

     If[k < 2, 
       labels[k] = "Fibonacci_rec(" <> ToString[k] <> ")";
       colors[k] = White;
       memo[k] = k;
       Return[k]
     ];

     left = FibonacciPilaAux[k - 1];
     right = FibonacciPilaAux[k - 2];

     AppendTo[edges, k -> k - 1];
     AppendTo[edges, k -> k - 2];

     labels[k] = "Fibonacci_rec(" <> ToString[k] <> ")";
     colors[k] = If[MemberQ[Keys[colors], k], Blue, LightOrange];

     memo[k] = left + right;
     memo[k]
   ];

   (* Construcci\[OAcute]n del grafo y c\[AAcute]lculo del valor de Fibonacci *)
   fibValue = FibonacciPilaAux[n];

   (* Creaci\[OAcute]n del grafo con estilos *)
   grafo = Graph[
     DeleteCases[edges, _ -> None],
     VertexLabels -> (labels /. None -> ""),
     VertexStyle -> Normal[colors],
     GraphStyle -> "NameLabeled",
     GraphLayout -> "LayeredDigraphEmbedding"
   ];

   If[toShow, Print[grafo]];
   fibValue
];

mostrarLlamadasED[func1_, func2_] :=
  Manipulate[
    Grid[{
      {"Valor de n", n},
      {"Resultado de la primera funci\[OAcute]n", func1[n]},
      {"Resultado de la segunda funci\[OAcute]n", func2[n]}
    }],
    {n, 0, 10, 1},
    SaveDefinitions -> True
  ]
  
  ExplicacionFibonacciED[] := Print[
  "La sucesi\[OAcute]n de Fibonacci se define as\[IAcute]:",
  "\n  - fib(0) = 0",
  "\n  - fib(1) = 1",
  "\n  - Para n \[GreaterEqual] 2, fib(n) = fib(n - 1) + fib(n - 2).",
  "\n\nEsta relaci\[OAcute]n de recurrencia significa que cada t\[EAcute]rmino es la suma de los dos anteriores."
]

ExplicacionFactorialED[] := Print[
  "La funci\[OAcute]n factorial se define as\[IAcute]:\n",
  "  - factorial(0) = 1\n",
  "  - Para n \[GreaterEqual] 1, factorial(n) = n * factorial(n - 1).\n\n",
  "Esta relaci\[OAcute]n de recurrencia significa que cada n multiplica el resultado de factorial(n-1)."
];

TablaFuncED[n_, func_] := Module[{lista},
  lista = Table[{k, func[k]}, {k, 0, n}];
  Print["Tabla de 0 hasta ", n, ":"];
  Grid[
    Prepend[lista, {"n", "func(n)"}],
    Frame -> All, Alignment -> Center
  ]
]

GraficaFuncED[n_, func_] := Module[{lista},
  lista = Table[func[k], {k, 0, n}];
  ListPlot[
    lista,
    PlotStyle -> {Red, PointSize[0.015]},
    PlotRange -> All,
    AxesLabel -> {"n", "func(n)"},
    PlotLabel -> "Gr\[AAcute]fica de de 0 a " <> ToString[n]
  ]
]
Options[EvaluaFuncion] = {inicio -> 1};
EvaluaFuncion[fun1_, fun2_, max_,OptionsPattern[]]:=
	Module[{init = OptionValue[inicio]},
		Table[fun1[n]==fun2[n],{n, init, max}]
	]

(* Definir funciones en Mathematica para llamar a Python *)
PlotFactorialRecursionTree[n_] := 
ExternalEvaluate["Python","import sys; sys.path.append('C:/Users/dylal/Documentos/Tutorias 2025/Tutorias_ED/Paquetes');    
import FactorialGraph; FactorialGraph.draw_factorial_tree("<>ToString[n]<>")"];
PlotTailFactorialRecursionTree[n_] := ExternalEvaluate[
"Python","import sys; sys.path.append('C:/Users/dylal/Documentos/Tutorias 2025/Tutorias_ED/Paquetes');    
import FactorialGraph; 
FactorialGraph.draw_tail_factorial_tree("<>ToString[n]<>")"];

(*------------------------------------------Relaci\[OAcute]n Recurrencia------------------------------------------*)


(*------------------------------------------Analisis de algoritmos------------------------------------------*)
Options[GraficoTiempos] = {inicio -> 1};

GraficoTiempos[funciones_List, pruebas_Integer, OptionsPattern[]] := Module[
  {n, datos, curvas, colores, etiquetas},
  
  (* Rango de n, de 'inicio' hasta 'pruebas' *)
  n = Range[OptionValue[inicio], pruebas];
  
  (* Medir el tiempo de cada funci\[OAcute]n para cada valor de n *)
  datos = Table[
    Table[
      First[AbsoluteTiming[func[x]]],
      {x, n}
    ],
    {func, funciones}
  ];
  
  (* Generar una lista de pares {n, tiempo} para cada funci\[OAcute]n *)
  curvas = Table[Transpose[{n, datos[[i]]}], {i, Length[funciones]}];
  
  (* Define una lista de colores para cada l\[IAcute]nea (ajusta si tienes m\[AAcute]s funciones) *)
  colores = {Blue, Orange, Green, Red, Purple, Brown};
  
  (* Genera etiquetas din\[AAcute]micamente: "S1", "S2", "S3", ... *)
  etiquetas = Table[Style["S" <> ToString[i], colores[[i]]], {i, Length[funciones]}];
  
  (* Construye el gr\[AAcute]fico con una l\[IAcute]nea por funci\[OAcute]n *)
  ListLinePlot[
    curvas,
    PlotRange -> All,
    PlotStyle -> Table[{Thick, colores[[i]]}, {i, Length[funciones]}],
    PlotLabels -> Placed[etiquetas, "End"],
    AxesLabel -> {"Cantidad de pruebas", "Tiempo (seg)"},
    PlotLabel -> "Comparaci\[OAcute]n de Tiempos"
  ]
];

Options[OrdenFunciones] = {inicio -> 1};
OrdenFunciones[funciones_List, pruebas_Integer, OptionsPattern[]] := 
  Module[
     {n, tiempos, promedios, orden},
     n = Range[OptionValue[inicio], pruebas];
     (* Se recopilan los tiempos para cada funci\[OAcute]n y se calcula el \
promedio *)
     tiempos = Table[
         Table[
            First[AbsoluteTiming[ func[x] ]],
            {x, n}
          ],
         {func, funciones}
       ];
     promedios = Mean /@ tiempos;
     orden = Ordering[promedios];
     Print["Orden de rapidez:"];
     Do[
        Print[
           "La funci\[OAcute]n ", orden[[i]], 
           Switch[i, 
              1, " es la m\[AAcute]s r\[AAcute]pida.", 
              2, " es la segunda m\[AAcute]s r\[AAcute]pida.", 
              3, " es la tercera m\[AAcute]s r\[AAcute]pida.", 
              _, 
      " se encuentra en la posici\[OAcute]n " <> ToString[i] <> "."
            ]
         ],
        {i, Length[orden]}
      ]
   ];
bigO[f_, g_, n_Symbol] := Limit[f/g, n -> Infinity];

bigOmega[f_, g_, n_Symbol] := Limit[g/f, n -> Infinity];

bigTheta[f_, g_, n_Symbol] := Module[
  {
   limitFoG = bigO[f, g, n],
   limitGoF = bigOmega[f, g, n]
  },
  Which[
   limitFoG == 0 && limitGoF == 0, "Inconcluso",
   (limitFoG > 0 && limitFoG < Infinity) , True,
   True, False
  ]
];

PlotAsymptoticFunctions[{xmin_?NumericQ, xmax_?NumericQ}] := Module[{fns, labels},
  (* Definir las funciones asint\[OAcute]ticas *)
  fns = {1, Log[n], n, Log[Log[n]], n^n, n^2, n^3};
  labels = {"1", "Log[n]", "n", "Log[Log[n]]", "n^n", "n^2", "n^3"};
  
  (* Generar la gr\[AAcute]fica en escala log-log *)
  Plot[Evaluate[fns], {n, xmin, xmax},
    PlotLegends -> Placed[labels, Above],
    ScalingFunctions -> {"Log", "Log"},
    PlotRange -> All,
    Frame -> True,
    Axes -> False,
    FrameLabel -> {"n", "f(n)"},
    PlotLabel -> "Funciones asint\[OAcute]ticas comunes"
  ]
];

End[]
EndPackage[]

