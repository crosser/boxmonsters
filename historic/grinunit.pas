unit GrInUnit;

interface

var
	GrDriver,
	GrMode,
	MaxColor,
	MaxX,
	MaxY:		integer;
procedure GrOpen;
procedure GrClose;

implementation

uses Graph;
function UCline(line:string):string;
var
	i:	integer;
begin
	for i:=1 to length(line) do
		UCline[i]:= UpCase(line[i]);
	UCline[0]:=chr(length(line));
end; {of function UCline}
procedure GrOpen;
var
	GrError:	integer;
begin
	DetectGraph(GrDriver,GrMode);
	if UCline(ParamStr(1))='/C' then GrDriver:=CGA;
	InitGraph(GrDriver,GrMode,'');
	GrError:=GraphResult;
	if GrError <> 0 then begin
		WriteLn(GraphErrorMsg(GrError));
		halt(1)
	end;
	MaxColor:=GetMaxColor;
	MaxX:=GetMaxX;
	MaxY:=GetMaxY
end; {of procedure GrOpen}
procedure GrClose;
begin
	CloseGraph
end; {of procedure GrClose}
end.