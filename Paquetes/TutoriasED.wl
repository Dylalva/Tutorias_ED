(* ::Package:: *)

BeginPackage["TutoriasED`"]
Print[Style["Se ha cargado el paquete TutoriasED. USO PARA ESTUDIO", Blue]];
FactorialPilaED::usage =
"FactorialPilaED[n, show -> True|False] \
Devuelve el factorial de n usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

FibonacciPilaED::usage =
"FibonacciPilaED[n, show -> True|False] \
Devuelve el n-\[EAcute]simo n\[UAcute]mero de la serie de Fibonacci usando recursi\[OAcute]n de pila (head recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

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
SumatoriaColaED::usage =
"SumatoriaColaED[n, show -> True|False] \
Devuelve la sumatoria 1 + 2 + ... + n usando recursi\[OAcute]n de cola (tail recursion). \
La opci\[OAcute]n show controla si se muestran (True) o no (False) los pasos intermedios por pantalla.";

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

FibonacciColaED[n_, OptionsPattern[]] := Module[{funcAux, toShow = OptionValue[show]},
  funcAux[x_, a_, b_] :=
    If[x == 0,
      If[toShow, Print["[FibCola] Caso base: fib(0) = ", a]];
      a, If[toShow, Print["[FibCola] Calculando fib(", x, ") con a = ", a, " y b = ", b]];
      With[{res = funcAux[x - 1, b, a + b]},
        If[toShow, Print["[FibCola] fib(", x, ") listo: ", res]];
        res
      ]
    ];
  funcAux[n, 0, 1]
]

FibonacciPilaED[n_, OptionsPattern[]] := Module[{toShow = OptionValue[show], result},
  If[n < 2,
    If[toShow, Print["[FibPila] Caso base: fib(", n, ") = ", n]];
    n,If[toShow, Print["[FibPila] Calculando fib(", n, ")"]];
    result = FibonacciPilaED[n - 1, show -> toShow] + FibonacciPilaED[n - 2, show -> toShow];
    If[toShow, Print["[FibPila] fib(", n, ") listo: ", result]];
    result
  ]
]

mostrarLlamadasED[func1_, func2_] :=
  Manipulate[
    Grid[{
      {"Valor de n", n},
      {"Resultado de la primera funci\[OAcute]n", func1[n, show -> True]},
      {"Resultado de la segunda funci\[OAcute]n", func2[n, show -> True]}
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
  lista = Table[{k, func[k, show -> False]}, {k, 0, n}];
  Print["Tabla de 0 hasta ", n, ":"];
  Grid[
    Prepend[lista, {"n", "func(n)"}],
    Frame -> All, Alignment -> Center
  ]
]

GraficaFuncED[n_, func_] := Module[{lista},
  lista = Table[func[k, show -> False], {k, 0, n}];
  ListPlot[
    lista,
    PlotStyle -> {Red, PointSize[0.015]},
    PlotRange -> All,
    AxesLabel -> {"n", "fib(n)"},
    PlotLabel -> "Gr\[AAcute]fica de de 0 a " <> ToString[n]
  ]
]

(*------------------------------------------Relaci\[OAcute]n Recurrencia------------------------------------------*)
End[]
EndPackage[]



