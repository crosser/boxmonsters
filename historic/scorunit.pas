unit ScorUnit;

interface

type
	ScorePtrType=	^ScoreType;
	ScoreType=	record
				Name:	string[11];
				Points:	integer
			end;
const
	ScoreLines=10;
var
	Score:	array[1..10] of ScoreType;
procedure ReadScore;
procedure WriteScore(ScoreLine:ScoreType);

implementation

uses CRT;
var
	ScoreActive:	Boolean;
	ScoreFile:	file of ScoreType;
procedure ReadScore;
const
	NullScore:	ScoreType = (Name:'';Points:0);
	InfoLines=	8;
	InfoLine:	array[1..InfoLines] of string[64]=
	(
	'Welcome to my new 3d game "Flying Arrows"!',
	'Feel free to copy and distribute this game.',
	'Please, report about any bugs to:',
	'           Eugene G. Crosser',
	'           USSR, Moscow, 125083,',
	'           Verhniaya Maslovka 5-16.',
	'           Telephone: (095)212-5274.',
	'This release 10/29/90.'
	);
var
	InfoFile:	text;
	i:		integer;
begin
	ScoreActive:=true;
	Assign(ScoreFile,'arrow.hsc');
	{$I-}
	Reset(ScoreFile);
	if IOResult <> 0 then begin
		Rewrite(ScoreFile);
		{$I+}
		if IOResult = 0 then begin
			for i := 1 to ScoreLines do
				Write(ScoreFile,NullScore);
			Reset(ScoreFile);
		end else ScoreActive:=false;
		Assign(InfoFile,'arrow-rd.me!');
		{$I-}
		Rewrite(InfoFile);
		{$I+}
		if IOResult = 0 then begin
			for i := 1 to InfoLines do begin
				writeln(InfoFile,InfoLine[i]);
				writeln(InfoLine[i]);
			end;
			Close(InfoFile);
			if ReadKey = ' ' then
		end
	end;
	if ScoreActive then begin
		for i:=1 to ScoreLines do
			Read(ScoreFile,Score[i]);
		Close(ScoreFile)
	end else
		for i:=1 to ScoreLines do
			Score[i]:=NullScore
end; {of procedure ReadScore}
procedure WriteScore(ScoreLine:ScoreType);
var
	i:		integer;
begin if ScoreActive then begin
	if ScoreLine.Points > Score[ScoreLines].Points then begin
		i:=ScoreLines-1;
		{$R-}
		while (ScoreLine.Points > Score[i].Points) and
		{$R+}
		      (i > 0) do begin
			Score[i+1]:=Score[i];
			i:=i-1;
		end;
		Score[i+1]:=ScoreLine
	end;
	Rewrite(ScoreFile);
	for i:=1 to ScoreLines do
		Write(ScoreFile,Score[i]);
	Close(ScoreFile)
end end; {of procedure WriteScore}
end.