(* ::Package:: *)

BeginPackage["StackExchange`StylesheetFunctions`", {"StackExchange`"}]

parseString::usage = "replace strings with Cell equivalents"
parseInput::usage = "parse input, possibly including Box structures"
convertInput::usage = "convert parsed input"
setStyle::usage = "setStyle[cell, style] sets the style of cell"
seString::usage = "seString returns the StackExchange string version"
toHybrid::usage = "toHybrid converts a cell to an editable WYSIWYG version"

$Stylesheet::usage = "Notebook expression corresponding to the stylesheet"

$StackExchangeInitialization::usage = "Global variable indicating that the package has been initialized"

Begin["`Private`"]

$StackExchangeInitialization = True

setStyle[Cell[a_, Longest[___String], b___], style_String] := Cell[a, style, b]

parseString[str_String] := Block[{interpretCode, $NewSymbol = Sow[#2<>#1]&},
	First @ Reap[
		StringReplace[str,
			{
			"``"~~Shortest[w__]~~"``":>Cell[w, If[NameQ[w], "Symbol", "CodeInput"]],
			"`"~~Longest[w__]~~"`" /; SyntaxQ[w] :> interpretCode[w],
			b:(StartOfString|_)~~"$$"~~Shortest[w__]~~"$$"~~e:(EndOfString|_):>If[b===e==="\n",
				Sequence @@ {b, Cell[w,"TeXInput"], e},
				Sequence @@ {b, "\n", Cell[w,"TeXInput"], "\n", e}
			],
			"$"~~Shortest[w__]~~"$":>Cell[w,"InlineTeXInput"],
			StartOfString~~w:Shortest[Except[WhitespaceCharacter]~~__]~~WordBoundary:>w,
			"." ~~ ws:Whitespace ~~ WordBoundary ~~ Shortest[w__] ~~ WordBoundary :> "."<>ws<>w,
			"["~~label__~~"]("~~link__~~")"/;StringFreeQ[label,"]"]&&StringFreeQ[link,")"] :> Cell[{label,link}, "Hyperlink"],
			h:(LetterCharacter~~WordCharacter...)~~"["~~Shortest[w__]/;StringCount[h,WordBoundary|"$"]==2&&SyntaxQ[h<>"["<>w] :>Cell[h<>"["<>w,"CodeInput"],
			WordBoundary~~w:(WordCharacter..)~~WordBoundary /;StringLength[w]>1&&If[StringFreeQ[w,"`"],NameQ["System`"<>w],NameQ[w]] :>Cell[w,"Symbol"]
			}
		],
		_,
		Remove /@ #2&
	]
]

interpretCode[w_] := Which[
	StringFreeQ[w, "`"], Cell[w, If[NameQ["System`"<>w], "Symbol", "CodeInput"]],
	NameQ[w]&&StringMatchQ[w, "Developer`"~~__], Cell[w, "Symbol"],
	True, Cell[w, "CodeInput"]
]

seString = ReplaceAll[#,
	{
	Cell[BoxData[TemplateBox[{a_, b_}, "SymbolTemplate"]], ___] :> b,
	Cell[BoxData[TemplateBox[{a_, b_}, "CodeTemplate"]], ___] :> If[StringFreeQ[b, "`"], "`"<>b<>"`","``"<>b<>"``"],
	Cell[BoxData[TemplateBox[{a_, b_}, "TeXTemplate"]], ___] :> "$$"<>b<>"$$",
	Cell[BoxData[TemplateBox[{a_, b_}, "InlineTeXTemplate"]], ___] :> "$"<>b<>"$",
	Cell[BoxData[TemplateBox[{a_, b_}, "HyperlinkTemplate"]], ___] :> "["<>a<>"]("<>b<>")" (* a not a string? *)
	}
]&

toHybrid[boxes_] := mergeStrings @ ReplaceAll[boxes,
	{
	Cell[BoxData[TemplateBox[{a_, b_}, "SymbolTemplate"]], ___] :> If[StringQ[a], "`"<>a<>"`", Cell[BoxData[a]]],
	Cell[BoxData[TemplateBox[{a_, b_}, "HyperlinkTemplate"]], ___] :> "["<>a<>"]("<>b<>")" (* a not a string? *)
	}
]

mergeStrings = ReplaceAll[#,
	TextData[a_List] :> TextData[List @@ StringExpression @@ a]
]&

Clear[parseInput]
parseInput[c_] := ReplaceAll[c,
	{
	Cell[s_String,r___]:>Cell[TextData[List@@parseString[s]],r],
	TextData[s_String]:>TextData[List@@parseString[s]],
	TextData[s_List]:>TextData @ Replace[s,
		{
		str_String:>Sequence@@parseString[str],
		Cell[b_,r___] /; !MatchQ[{r}, {"Hyperlink"|"Symbol"|"CodeInput"|"TeXInput"|"InlineTeXInput", ___}] :> Cell[b, "InlineTeXInput"]
		},
		{1}
	]
	}
]

Clear[convertInput]
convertInput[c_] := ReplaceAll[c,
	{
	Cell[s_,"Symbol", ___] :> makeSymbolBox[s],
	Cell[s_,"CodeInput", ___] :> makeCodeBox[s],
	Cell[s_,"TeXInput", ___] :> makeTeXBox[s],
	Cell[s_,"InlineTeXInput", ___] :> makeInlineTeXBox[s],
	Cell[{label_, link_}, "Hyperlink"] :> makeHyperlinkBox[label,link]
	}
]

makeSymbolBox[BoxData[TemplateBox[{a_, b_}, "SymbolTemplate"]]] := If[StringQ@a,
	makeSymbolBox[a],
	makeCodeBox[BoxData[a]]
]
makeSymbolBox[s_String] := Replace[
	refURL[s],
	{
		ref_String :> toSymbolCell[s, ref],
		_ :> toCodeCell[s, s]
	}
]

refURL[s_String] := Which[
	StringMatchQ[s, "Developer`"~~__],
	"[``"<>s<>"``](http://reference.wolfram.com/language/Developer/ref/"<>StringDrop[s, 10]<>")",

	StringFreeQ[s, "`"] && NameQ[s] && Context[s] === "System`",
	"[`"<>s<>"`](http://reference.wolfram.com/language/ref/"<>s<>")",
	
	_, 
	$Failed
]

makeCodeBox[BoxData@TemplateBox[{a_, b_}, "CodeTemplate"]] := If[StringQ[a], makeCodeBox[a], makeCodeBox[BoxData[a]]]

makeCodeBox[s_String] := Which[
	!SyntaxQ[s], "``"<>s<>"``",
	NameQ[s], makeSymbolBox[s],
	True, toCodeCell[s, s]
]
makeCodeBox[s_BoxData] := toCodeCell[
	First @ s,
	KillLinearSyntax @ First @ FrontEndExecute @ ExportPacket[Cell[s] /. "."->Sequence[" ", ".", " "], "InputText"]
]

makeTeXBox[s_] := Cell[
	BoxData @ TemplateBox[
		toTeX[s],
		"TeXTemplate"
	],
	"TeXInput",
	TextClipboardType->"InputText"
]

makeInlineTeXBox[s_] := Cell[
	BoxData @ TemplateBox[
		toTeX[s],
		"InlineTeXTemplate"
	],
	"InlineTeXInput",
	TextClipboardType->"InputText"
]

makeHyperlinkBox[label_String, link_] := With[{trim = StringTrim[label, "`"..]},
	With[{ref = refURL[trim]},
		If[StringQ@ref && StringMatchQ[ref, __~~"("~~StringReplace[link, ".html"~~EndOfString->""]~~")"],
			toSymbolCell[trim, ref],
			toHyperlinkCell[trim, link]
		]
	]
]	

toSymbolCell[s_, str_] := Cell[
	BoxData @ TemplateBox[{s, str}, "SymbolTemplate"],
	"Symbol",
	TextClipboardType->"Package"
]

toCodeCell[s_, str_] := Cell[
	BoxData @ TemplateBox[{s, str}, "CodeTemplate"],
	"CodeInput",
	TextClipboardType->"Package"
]

toHyperlinkCell[label_, link_] := Cell[
	BoxData @ TemplateBox[{label, link}, "HyperlinkTemplate"],
	"Hyperlink",
	TextClipboardType->"PlainText"
]

KillLinearSyntax = StringReplace[#, 
	"\!"~~w__~~"\)" /; SyntaxQ["\!"<>w<>"\)"] :> 
	ToExpression["\!"<>w<>"\)", InputForm, Function[z, ToString[Unevaluated@z, InputForm], HoldAll]]
]&

Clear[toTeX]
toTeX[BoxData @ TemplateBox[{a_, b_}, "TeXTemplate" | "InlineTeXTemplate"]] := toTeX[a]
toTeX[s_String] := With[
	{
	expr = Quiet @ Check[ToExpression[s, TraditionalForm, HoldComplete], $Failed]
	},
	If[expr === $Failed,
		fromTeX[s],
		{createBoxes@expr, createTeX@expr}
	]
]
toTeX[s_] := With[
	{
	expr = Quiet @ Check[ToExpression[s, TraditionalForm, HoldComplete], $Failed]
	},
	Which[
		expr =!= $Failed,
		{createBoxes@expr, createTeX@expr},

		StringQ[expr],
		fromTeX[s],

		True,
		{FrameBox[s, FrameStyle->RGBColor[1,0,0]], $Failed}
	]
]

fromTeX[s_] := With[
	{
	expr = Quiet @ Check[ToExpression[s, TeXForm, HoldComplete], $Failed]
	},
	If[toTeXString[expr] === s,
		{createBoxes@expr, s},
		{s, s}
	]
]
toTeXString[HoldComplete[s_]] := ToString[Unevaluated[s], TeXForm]

createBoxes[HoldComplete[expr_]] := FormBox[MakeBoxes[expr, TraditionalForm], TraditionalForm]
createTeX[HoldComplete[expr_]] := ToString[Unevaluated[expr], TeXForm]

$Stylesheet = Notebook[
	{
	Cell[StyleData[StyleDefinitions->"Default.nb"]],
	Cell[StyleData["Notebook"],
		ShowCellLabel->False
	],
	Cell[StyleData["Input"],
		ShowCellLabel->True
	],
	Cell[StyleData["Output"],
		ShowCellLabel->True
	],
	Cell[StyleData["Code"],
		StyleKeyMapping -> {"Tab" -> "CodeBlock"}
	],
	Cell[StyleData["CodeBlock", StyleDefinitions->StyleData["Code"]],
		InitializationCell -> False,
		StyleKeyMapping -> {"Backspace" -> "Code"},
		Background -> RGBColor[.94,.94,.95]
	],
	Cell[StyleData["CodeBlockOutput", StyleDefinitions->StyleData["Output"]],
		TextClipboardType->"Package",
		AutoIndent->True,
		AutoSpacing->False,
		Background -> RGBColor[.94,.94,.95]
	],
	Cell[StyleData["Text"],
		StyleKeyMapping -> {"Tab" -> "StackExchangeEdit"}
	],
	Cell[StyleData["StackExchange", StyleDefinitions->StyleData["Text"]],
		ContextMenu->{
			MenuItem[
				"Show String",
				FrontEndExecute[{
					FrontEndToken["SelectNextCell"],
					SelectionSetStyle[InputNotebook[], "StackExchangeShowExpression"]
				}]
			],
			MenuItem[
				"Mixed Format",
				FrontEndExecute[{
					FrontEndToken["SelectNextCell"],
					SelectionSetStyle[InputNotebook[], "StackExchangeHybrid"]
				}]
			]
		},
		StyleKeyMapping -> {
			"Tab" -> "StackExchangeHybrid",
			System`KeyEvent["Tab",System`Modifiers->{System`Shift}]->"StackExchangeShowExpression"
		},
		CellEditDuplicate->True,
		DefaultDuplicateCellStyle->"StackExchangeEdit"
	],
	Cell[StyleData["StackExchangeEdit", StyleDefinitions->StyleData["Text"]],
		ContextMenu->{
			MenuItem[
				"Format",
				FrontEndExecute[{
					FrontEndToken["SelectNextCell"],
					SelectionSetStyle[InputNotebook[], "StackExchangeFormat"]
				}]
			],
			MenuItem[
				"Show String",
				FrontEndExecute[{
					FrontEndToken["SelectNextCell"],
					SelectionSetStyle[InputNotebook[], "StackExchangeShowExpression"]
				}]
			],
			MenuItem[
				"Mixed Format",
				FrontEndExecute[{
					FrontEndToken["SelectNextCell"],
					SelectionSetStyle[InputNotebook[], "StackExchangeHybrid"]
				}]
			]
		},
		StyleKeyMapping -> {
			"Tab" -> "StackExchangeFormat",
			System`KeyEvent["Tab",System`Modifiers->{System`Shift}]->"StackExchangeShowExpression"
		},
		CellFrameMargins->0,
		Background->RGBColor[0.92, 1, 0.92],
		Evaluatable->True,
		CellEvaluationFunction->Function[Null, Null],
		CellProlog :> If[!BooleanQ@$StackExchangeInitialization,
			Get["StackExchange`"]; 
			$StackExchangeInitialization = TrueQ@$StackExchangeInitialization
		],
		CellEpilog :> If[$StackExchangeInitialization,
			NotebookWrite[
				EvaluationCell[],
				setStyle[
					convertInput @ parseInput @ NotebookRead[EvaluationCell[]],
					"StackExchange"
				],
				After
			]
		]
	],
	Cell[StyleData["StackExchangeHybrid", StyleDefinitions->StyleData["Text"]],
		CellDynamicExpression :> With[{cell = NotebookRead@EvaluationCell[]},
			If[!BooleanQ@$StackExchangeInitialization,
				Get["StackExchange`"]; 
				$StackExchangeInitialization = TrueQ@$StackExchangeInitialization
			];
			If[TrueQ@$StackExchangeInitialization,
				NotebookWrite[EvaluationCell[], Cell[""], All];
				NotebookWrite[
					EvaluationNotebook[],
					setStyle[
						toHybrid @ cell,
						"StackExchangeEdit"
					],
					All
				];
				SelectionMove[EvaluationNotebook[], Before, CellContents],
				NotebookWrite[EvaluationCell[], Cell[""], All];
				NotebookWrite[
					EvaluationCell[],
					Replace[cell,
						Cell[a_, _, b__] :>
						Cell[a, "StackExchange", CellDynamicExpression:>None, b]
					]
				]
			]
		]
	],
	Cell[StyleData["StackExchangeShowExpression", StyleDefinitions->StyleData["Text"]],
		CellDynamicExpression :> With[{cell = NotebookRead@EvaluationCell[]},
			If[!BooleanQ@$StackExchangeInitialization,
				Get["StackExchange`"]; 
				$StackExchangeInitialization = TrueQ@$StackExchangeInitialization
			];
			If[TrueQ@$StackExchangeInitialization,
				NotebookWrite[EvaluationCell[], Cell[""], All];
				NotebookWrite[
					EvaluationNotebook[],
					setStyle[
						seString @ cell,
						"StackExchangeEdit"
					],
					All
				];
				SelectionMove[EvaluationNotebook[], Before, CellContents],
				NotebookWrite[EvaluationCell[], Cell[""], All];
				NotebookWrite[
					EvaluationCell[],
					Replace[cell,
						Cell[a_, _, b__] :>
						Cell[a, "StackExchange", CellDynamicExpression:>None, b]
					]
				]
			]
		]
	],
	Cell[StyleData["StackExchangeFormat", StyleDefinitions->StyleData["Text"]],
		CellDynamicExpression :> With[{cell = NotebookRead@EvaluationCell[]},
			SetOptions[EvaluationCell[], CellDynamicExpression->None];
			With[
				{
				old = Replace[cell,
					Cell[a__, "StackExchangeFormat", b___] :> Cell[a, "StackExchange", b]
				],
				new = setStyle[
					convertInput @ parseInput @ cell,
					"StackExchange"
				]
				},
				Replace[
					new,
					{
					c_Cell :> NotebookWrite[EvaluationCell[], c, All],
					o_ :> NotebookWrite[EvaluationCell[], old, All]
					}
				];
				SelectionMove[EvaluationNotebook[], CellContents, Before]
			]
		]
	],
	Cell[StyleData["HyperlinkTemplate"],
		TemplateBoxOptions -> {
			Editable -> True,
			DisplayFunction -> (DynamicBox[ToBoxes@Hyperlink[#1,#2, BaseStyle->{RGBColor[.64,.22,.35]}]]&),
			InterpretationFunction -> (Cell[TextData[{"[",#1,"](",#2,")"}]]&)
		}
	],
	Cell[StyleData["SymbolTemplate"],
		TemplateBoxOptions -> {
			Editable->True,
			DisplayFunction->(StyleBox[
				PaneBox[#,
					BaselinePosition->Baseline,
					BaseStyle->{Background->RGBColor[.94,.94,.95]},
					ImageMargins->2,
					FrameMargins->{{4,4},{2,2}}
				],
				"Text",
				RGBColor[.64,.22,.35],
				ShowStringCharacters -> False
			]&),
			InterpretationFunction->(Cell[#2]&)
		}
	],
	Cell[StyleData["CodeTemplate"],
		TemplateBoxOptions -> {
			Editable->True,
			DisplayFunction->(StyleBox[
				PaneBox[TooltipBox[#1, #2, TooltipStyle->{ShowStringCharacters->True}],
					BaselinePosition->Baseline,
					BaseStyle->{ShowStringCharacters->True, Background->RGBColor[.94,.94,.95]},
					ImageMargins->2,
					FrameMargins->{{4,4},{2,2}}
				],
				"Text",
				ShowStringCharacters -> False
			]&),
			InterpretationFunction->(Cell[TextData[{"``", Cell@BoxData@#2, "``"}]]&)
		}
	],
	Cell[StyleData["TeXTemplate"],
		TemplateBoxOptions -> {
			Editable->True,
			DisplayFunction->(FormBox[
				StyleBox[
					PaneBox[TooltipBox[#1, #2],
						BaselinePosition->Baseline,
						BaseStyle->{Background->RGBColor[1,1,.85,.5]},
						Alignment->Center,
						ImageSize->Full,
						FrameMargins->20
					],
					FontFamily->"Times",
					FontSize->16,
					ScriptLevel->0,
					ShowStringCharacters -> False
				],
				TraditionalForm
			]&),
			InterpretationFunction->(Cell[TextData[{"$$",#2,"$$"}]]&)
		}
	],
	Cell[StyleData["InlineTeXTemplate"],
		TemplateBoxOptions -> {
			Editable->True,
			DisplayFunction->(FormBox[
				StyleBox[
					PaneBox[TooltipBox[#1, #2],
						BaselinePosition->Baseline,
						BaseStyle->{Background->RGBColor[1,1,.85,.5]},
						ImageMargins->2,
						FrameMargins->{{4,4},{2,2}}
					],
					FontFamily->"Times",
					FontSize->16,
					ScriptLevel->0,
					ShowStringCharacters -> False
				],
				TraditionalForm
			]&),
			InterpretationFunction->(Cell[TextData[{"$",#2,"$"}]]&)
		}
	],
	Cell[StyleData["ImageTemplate"],
		TemplateBoxOptions -> {
			DisplayFunction -> (#1&),
			InterpretationFunction -> (Cell[TextData[{"![", #3, "](", #2, ")"}]]&)
		}
	],
	Cell[StyleData["Quote"],
		Background->RGBColor[1, .98, .89],
		CellFrame->{{2,0},{0,0}},
		CellFrameColor->RGBColor[1,.92,.56]
	],
	Cell[StyleData["Comment"],
		CellMargins->{{90, 10}, {10, 5}},
		FontSize->12
	],
	Cell[StyleData["Answer"],
		CellGroupingRules -> {"SectionGrouping", 70},
		CellDingbat -> StyleBox["A", FontSize->9],
		ShowGroupOpener->True,
		CellFrame -> {{0, 0}, {0, 1}},
		CellFrameColor->GrayLevel[.9]
	],
	Cell[StyleData["AnsweredBy", StyleDefinitions->StyleData["Signature"]],
		CellFrameLabels->{{None, StyleBox["answered", GrayLevel[.8]]}, {None, None}}
	],
	Cell[StyleData["AskedBy", StyleDefinitions->StyleData["Signature"]],
		CellFrameLabels->{{None, StyleBox["asked", GrayLevel[.8]]}, {None, None}}
	],
	Cell[StyleData["EditedBy", StyleDefinitions->StyleData["Signature"]],
		CellFrameLabels->{{None, StyleBox["edited", GrayLevel[.8]]}, {None, None}}
	],
	Cell[StyleData["Community", StyleDefinitions->StyleData["Signature"]],
		CellFrameLabels->{{None, StyleBox["community", GrayLevel[.8]]}, {None, None}}
	],
	Cell[StyleData["Signature", StyleDefinitions->StyleData["Text"]],
		TextAlignment->Right,
		ShowStringCharacters->False
	],
	Cell[StyleData["HorizontalLine", StyleDefinitions->StyleData["Output"]],
		Editable->False, Selectable->False,
		CellFrame->{{0,0},{0,1}},
		ShowCellBracket->False, CellElementSpacings->{"CellMinHeight"->1},
		CellFrameMargins->0, CellFrameColor->GrayLevel[.9], CellSize->{Inherited,4}
	]
	},
	Saveable->False,WindowSize->{808,689},WindowMargins->{{Automatic,143},{40,Automatic}},
	FrontEndVersion->"10.3 for Mac OS X x86 (32-bit, 64-bit Kernel) (December 10, 2015)",
	StyleDefinitions->"PrivateStylesheetFormatting.nb"
]

InstallStylesheet[] := NotebookPut[$Stylesheet]

End[]

EndPackage[]
