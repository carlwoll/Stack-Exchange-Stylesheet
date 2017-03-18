(* ::Package:: *)

BeginPackage["StackExchangeView`", {"StackExchange`"}]

StackExchangeView::usage = "StackExchangeView[url] returns a Mathematica notebook version of url"

Begin["`Private`"]

Options[StackExchangeView] = {Deployed -> False}

StackExchangeView[link_] := Notebook[postprocessCell /@ Flatten @ Last @ Reap @
	With[{xml = Import[link,"XMLObject"]},
		Cases[
			xml,
			x:Alternatives[
				XMLElement["div",{"id"->"question-header"}, _],
				XMLElement["div",{"class"->"comment-body",__},_],
				XMLElement["td", {__, "class"->"answercell"|"postcell"},_]
			] :> processXML[x],
			Infinity
		];
	],
	StyleDefinitions->"StackExchange.nb"
]

(* extract title *)
processXML[XMLElement["div", {"id"->"question-header"}, q_]] := Cases[
	q,
	XMLElement["a", {___,"class"->"question-hyperlink",__}, x_] :> Sow@Cell[TextData[x],"Subsubsection"],
	Infinity,
	1
]

(* extract question/answer *)
processXML[XMLElement["td", {__, "class"->type_}, q_]] := CompoundExpression[
	iXML[q, type];
	Cases[
		q,
		x:XMLElement["div",{"class"->("user-info"|"user-info user-hover")}, _] :> processXML[x],
		Infinity
	]
]

(* extract users *)
processXML[XMLElement["div", {"class"->("user-info"|"user-info user-hover")}, q_]] := Replace[
	userRules @ q,
	r:Except[{}] :> Sow @ Cell[
		BoxData @ ToBoxes @ userSignature @ r,
		"type" /. r /. {"asked"->"AskedBy", "edited"->"EditedBy", "answered"->"AnsweredBy", "community"->"Community", _->"Signature"}
	]
]

(* extract comments *)
processXML[XMLElement["div",{"class"->"comment-body",__},q_]] := CompoundExpression[
	Sow[grayLine[]];
	bodyXML[
		XMLElement[
			"p",
			{},
			DeleteCases[Replace[q, s_String:>StringReplace[StringTrim[s], "\[Dash]"->" \[LongDash] "], {1}], ""]
		],
		"Comment"
	]
]

iXML[q_, t_] := Block[{first},
	If[t==="postcell",
		Sow@grayLine[];
		first=Automatic,

		(* first answer cell gets a horizontal top frame line *)
		first:=(first=Automatic; "Answer")
	];

	ReplaceAll[q, x:XMLElement[type:"p"|"pre"|"blockquote"|"ul"|"ol"|"hr"|"h1"|"h2"|"h3", __] :> bodyXML[x, first]]
]

bodyXML[q_, style_:Automatic] := Block[
	{
	XMLElement = XMLToString,
	optional = Replace[style, Automatic->Sequence[]],
	required = Replace[style,
		{
		Automatic->"StackExchange",
		"Answer"->Sequence["StackExchange","Answer"]
		}
	]
	},

	Replace[stripWrappers@q,
		{
		Cell[s_, t__String, u___] :> Sow @ Cell[s,t,optional,u],
		c_ :> Sow @ Cell[TextData[c], required]
		}
	]
]

(* XMLToString converts XMLElements to Text/StackExchange and Input/Output cells as appropriate *)
Clear[XMLToString]

(* signature of an Input cell *)
XMLToString["pre", {}, s_] := Replace[
	stripWhitespace[s],
	{t_String} :> Cell[BoxData @ FrontEndExecute @ FrontEnd`ReparseBoxStructurePacket @ StringTrim[t, (Whitespace|"`")..], "CodeBlock"]
]

(* signature of an inline cell *)
XMLToString["code",{}, {s_String}] := Which[
	!StringFreeQ[s, "``"], "``` "<>s<>" ```",
	!StringFreeQ[s, "`"], "`` "<>s<>" ``",
	True, "`"<>s<>"`"
]

(* signature of an ordinary paragraph *)
XMLToString["p", {}, s_] := s

(* signature of an image *)
XMLToString["p", {}, {s_BoxData}] := Cell[s, "Output"]

(* signature of a blockquote *)
XMLToString["blockquote", {}, s_] := Cell[
	TextData[trim @ augment[s]],
	"Quote",
	"StackExchange"
]

XMLToString["hr", {}, {}] := grayLine[]

(* headers *)
XMLToString["h1", {}, s_] := augment["<h1>", s, "</h1>"]

XMLToString["h2", {}, s_] := augment["<h2>", s, "</h2>"]

XMLToString["h3", {}, s_] := augment["<h3>", s, "</h3>"]

(* signature of emphasized text *)
XMLToString["em", {}, s_] := augment["*", s, "*"]

(* signature of bold text *)
XMLToString["strong", {}, s_] := augment["**", s, "**"]

(* links *)
XMLToString["a", rules_, s_] := With[{link = "href"/.rules},
	Which[
		!StringQ[link] || !StringMatchQ[link, "http"~~__],
		augment[s],

		StringMatchQ[link, __~~".png"], 
		If[MatchQ[s, {Cell[BoxData[TemplateBox[_, "ImageTemplate"]], __]}],
			First @ s,
			XMLToString["img", "src"->link, {}]
		],

		True,
		augment["[", s, "](", link, ")"]
	]
]

XMLToString["li", {}, s_] := augment["* ", s, "\n\n"]

(* signature of an image *)
XMLToString["img",rules_,_] := Cell[
	BoxData@TemplateBox[
		{
		ToBoxes@Import["src"/.rules],
		"src"/.rules,
		"alt"/.rules
		},
		"ImageTemplate"
	],
	"Output"
]

(* signature of a comment *)
XMLToString["span", {"class"->"comment-copy",___}, q_] := XMLToString["p", {}, q]

(* bullets *)
XMLToString["ul", {}, s_] := trim @ augment[s]

(* numbered list *)
XMLToString["ol", {}, s_] := trim @ augment @ MapIndexed[
	toNumber[#1, #2[[1]]]&,
	s
]

toNumber[s_String, index_] := StringReplace[s, StartOfString~~"*" :> ToString[index]<>"."]
toNumber[{s_String, t___}, index_] := {toNumber[s, index], t}

(* all others *)
XMLToString[a__] := Sequence[]

(* a horizontal separator line *)
grayLine[] := Cell[
	"",
	"HorizontalLine", 
	CellMargins -> {First@CurrentValue[{StyleDefinitions, "Output", CellMargins}], {0, 0}}
]

(* remove superfluous cell wrappers *)
stripWrappers[{c_Cell}] := stripWrappers[c]
stripWrappers[Cell[c_, opts___]] := Cell[
	ReplaceRepeated[
		c,
		{
		TextData[{s__String}]:>StringJoin[s],
		Cell[s_String,__]:>s
		}
	],
	opts
]
stripWrappers[c_] := c
stripWrappers[] = ""

(* whitespace *)
stripWhitespace[s_String] := StringTrim[s]
stripWhitespace[s_] := s
stripWhitespace[s_List] := DeleteCases[stripWhitespace /@ s, ""]

(* augment "TextData" objects *)
augment[s__] := iaugment @ Flatten[{s}]

iaugment[s_List] := List @@ StringExpression @@ Replace[s,
	{a___, b_String /; !StringFreeQ[b, "[]("], c_, d___} :> 
	{a, StringReplace[b, x___~~"[](" :> StringExpression[x, "[", c, "]("]], d}
]

trim[s_] := Replace[s,
	{
		{b_, m___, e_} :> 
			{
			If[StringQ@b, StringReplace[b, StartOfString~~Whitespace~~t___ -> t], b],
			m,
			If[StringQ@e, StringReplace[e, t___~~Whitespace~~EndOfString -> t], e]
			},
		b_String | {b_String} :> StringTrim[b, Whitespace]
	}
]

(* remove superfluous cell wrappers, and convert blockquote cells into "Output" cells when they follow an "Input" cell *)
postprocessCell[Cell[c_, "Quote", r___]] := Internal`WithLocalSettings[
	Null,
	(* eliminat3e WithLocalSettings, and "CodeBlock" + "Output" \[Rule] "CodeBlockOutput" *)
	Cell[
		Replace[c,
			{
			TextData[Cell[d_,e___] | {Cell[d_,e___]}] :> Sequence[d, "Quote", e],
			other_ :> Sequence[c,"Quote",r]
			}
		],
		If[TrueQ@codeBlock, "Output", Sequence@@{}]
	],
	codeBlock=False
]
postprocessCell[Cell[c_, d__]] := Internal`WithLocalSettings[
	Null,
	Replace[c,
		{
		TextData[cell_Cell|{cell_Cell}]:>cell,
		_:>Cell[c,d]
		}
	],
	codeBlock=MemberQ[{d},"CodeBlock"]
]
postprocessCell[c_]:=c

(* extract user info *)

userRules[info_] := Flatten @ Cases[
	info,
	XMLElement["div", {"class"->type_}, q_] :> getUserInfo[type,q],
	Infinity
]

userSignature[rules_] := With[
	{
	avatar = "avatar" /. rules,
	name = "name" /. rules /. "name"->"Anonymous",
	flair = 
		{
		"reputation" /. rules /."reputation"->Sequence[],
		"badge1" /. rules /. {"badge1"->Sequence[], n_->Sequence[badge1,n]},
		"badge2" /. rules /. {"badge2"->Sequence[], n_->Sequence[badge2,n]},
		"badge3" /. rules /. {"badge3"->Sequence[], n_->Sequence[badge3,n]}
		}
	},

	If[avatar==="avatar",
		Identity,
		Row[{Pane[avatar, BaselinePosition->Center], #}, " ", Alignment->{Left,Center}]&
	] @ If[flair==={},
		name,
		Grid[
			{
			{name, SpanFromLeft},
			flair
			},
			Spacings->0, Alignment->{Left,Baseline}, BaselinePosition->Center
		]
	]
]

getUserInfo[__]=Sequence[];

(* get type: asked, edited, answered *)
getUserInfo["user-action-time", q_] := Cases[
	q,
	{s_String, XMLElement["span", {"class"->"relativetime",__},_],___} :> "type"->StringTrim[s],
	{0,Infinity},
	1
]

(* get avatar *)
getUserInfo["gravatar-wrapper-32", q_] := Cases[
	q,
	XMLElement["img",rules_,_] :> "avatar" -> ImageResize[Import["src"/.rules], ToExpression[{"width","height"}/.rules]]
]

(* get name and flair *)
getUserInfo["user-details",q_] := Flatten @ Last @ Reap @ ReplaceAll[q,
	{
	XMLElement["span", {"class"->"community-wiki",__},_] :> Sow["type"->"community"],
	XMLElement["a", _, {name_}] :> Sow["name"->StringTrim@name],
	XMLElement["span", {"class"->"reputation-score",__}, {rep_}] :> Sow["reputation"->rep],
	{XMLElement["span", {"class"->b_}, _], XMLElement["span", {"class"->"badgecount"}, {c_}]} :> Sow[b->c]
	}
]

badge1 = Graphics[
	{
	FaceForm[RGBColor[1.,0.8,0.]],
	FilledCurve[{
		{{1,4,3},{1,3,3},{1,3,3},{1,3,3},{1,3,3}}},
		{{{101.9,507.3},{102.9,508.9},{101.6,513.1},{99.89999,513.5},{101.6,513.1},{105.2,515.7},{105.2,517.3},{105.2,515.7},{108.8,513.1},{110.5,513.5},{108.8,513.1},{107.5,508.9},{108.5,507.3},{107.3,508.9},{102.9,508.9},{101.9,507.3}}}
	]
	},
	ImageSize->12,
	BaselinePosition->Scaled[0.45]->Center
];
badge2 = Graphics[
	{
	FaceForm[RGBColor[0.772549,0.772549,0.772549]],
	FilledCurve[{
		{{1,4,3},{1,3,3},{1,3,3},{1,3,3}}},
		{{{85.10001,506.},{85.10001,508.3},{81.40001000000001,512.},{79.10001,512.},{81.40001000000001,512.},{85.10001,515.7},{85.10001,518.},{85.10001,515.7},{88.8,512.},{91.10001,512.},{88.8,512.},{85.10001,508.3},{85.10001,506.}}}
	]
	},
	ImageSize->12,
	BaselinePosition->Center->Center
];
badge3 = Graphics[
	{
	FaceForm[RGBColor[0.8,0.6,0.4]],
	FilledCurve[{
		{{1,4,3},{1,3,3},{1,3,3}}},
		{{{59.3,507.3},{61.40001,509.1},{65.10001,515.5},{65.10001,517.3},{65.10001,515.5},{68.8,509.1},{70.90001000000001,507.3},{68.7,509.1},{61.3,509.1},{59.3,507.3}}}
	]
	},
	ImageSize->12,
	BaselinePosition->Center->Center
];

End[]

EndPackage[]
