(* ::Package:: *)

(* Mathematica Package *)

(*
to use, simply:
	1. place this file in
		 a. linux: ~/.Mathematica/Applications
		 b. windows: C:\users\<you>\appdata\roaming\Mathematica\applications (i
		  	THINK... something like that at least),
		c. or the equivalent for your OS
	2. insert
			<<NotebookBackup`;
		 into your init.m file, located in ~/.Mathematica/Kernel for linux
	3. update the various settings below to your liking (particularly
		 AcceptablePaths!)
*)

BeginPackage["NotebookBackup`"];

BackupInterval::usage = "Minimum time interval, in seconds, required to elapse before \
backing up file.";
BackupInterval = 10 * 60;

AcceptablePaths::usage = "Mathematica files saved \"downstream\" of any of these paths \
will be backed up.";
AcceptablePaths = Switch[$OperatingSystem,
	"Unix", {"/home"},
	"Windows", {"C:\\Users"},
	"MacOSX", {"/Users"},
	_, {""}
];

AcceptableExtensions::usage = "Only files with these extensions will be backed up.";
AcceptableExtensions = {"nb"};

SetStatus::usage = "If true, put backup message in status area after each backup.";
SetStatus = True;

RemoveStatus::usage = "If true, remove backup message from status area after any \
subsequent notebook evaluation.";
RemoveStatus = False;

BackupName::usage = "This function takes a string containing the notebook's filename, \
as well as an integer specifying the backup number, and returns a new string with the \
backup filename.";


Begin["`Private`"];

BackupName[file_String, num_Integer] :=
	file <> StringJoin @@ Table["~", {num}];

SubPathQ[basepath_String, testpath_String] :=
	(StringLength[testpath] >= StringLength[basepath] && StringTake[testpath, StringLength[basepath]] === basepath);

BackupCurrentNotebook[input_] :=
	Module[{nbfile},
		nbfile = Quiet[NotebookFileName[]];

		If[(!nbfile === $Failed) && (*nb must have been saved*)
			(Or @@ (SubPathQ[#, nbfile]&) /@ AcceptablePaths) && (*must be saved within acceptable path (avoid help files!)*)
			(MemberQ[AcceptableExtensions, FileExtension[nbfile]]) && (*must have acceptable extension*)
	
				(AbsoluteTime[FileDate[nbfile]] + BackupInterval < AbsoluteTime[]) (* and not have been updated within BackupInterval*)
		,
			NotebookSave[];

			If[SetStatus, SetOptions[EvaluationNotebook[], WindowStatusArea -> FileNameTake[nbfile] <>
				" autosaved at  "  <>
				DateString[{"DateShort", " at ", "Hour24", ":", "Minute", ":", "Second"}]]];
		,
			If[RemoveStatus, SetOptions[EvaluationNotebook[], WindowStatusArea -> Automatic]]
		];

		input
	];

$Pre (*$Post*) = BackupCurrentNotebook;

End[];

EndPackage[];
