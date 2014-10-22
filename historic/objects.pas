unit Objects;

interface

uses Globals,Graph3d,Graph,Monsters,CRT;

type
	FOPtr=	^FO;
	UFOPtr=	^UFO;
	IFOPtr=	^IFO;
	FO=	object
			Next:	FOPtr;
			P,V:	Point3;
			Time:	integer;
			Color:	ColorIndex;
			constructor	Init;
			destructor	Done;virtual;
			function	IsIFO:Boolean;virtual;
			function	IsDying:Boolean;virtual;
			procedure	SetSpeed(vx,vy,vz:integer);
			procedure	SwapColor(col:ColorIndex);
			procedure	Step;virtual;
			function	HitBy(O:FOPtr):Boolean;virtual;
		end;
	UFO=	object(FO)
			Size:	integer;
			Old:	Point2;
			OSize:	integer;
			AcTime,
			PushTime:integer;
			Active,
			Dying:	Boolean;
			constructor	Init(x,y,z,vx,vy,vz:integer;col:ColorIndex);
			destructor	Done;virtual;
			function	IsIFO:Boolean;virtual;
			function	IsDying:Boolean;virtual;
			procedure	Step;virtual;
			function	HitBy(O:FOPtr):Boolean;virtual;
		end;
	IFO=	object(FO)
			Length:	integer;
			O1,O2:	Point2;
			constructor	Init(x,y,z,vx,vy,vz:integer;col:ColorIndex);
			destructor	Done;virtual;
			function	IsIFO:Boolean;virtual;
			function	IsDying:Boolean;virtual;
			procedure	Step;virtual;
			function	HitBy(O:FOPtr):Boolean;virtual;
		end;
	FOList=	object
			Last:	FOPtr;
			procedure	Ins(O:FOPtr);
			procedure	Del(var O:FOPtr);
			function	Next(O:FOPtr):FOPtr;
			function	First:FOPtr;
			function	EOL(O:FOPtr):Boolean;
			procedure	Flush;
		end;

var
	List:		FOList;

procedure	NewUFO(x,y,z,vx,vy,vz:integer;col:ColorIndex);
procedure	NewIFO(x,y,z,vx,vy,vz:integer;col:ColorIndex);
function	Strike(FO1,FO2:FOPtr):Boolean;

implementation

constructor	FO.Init;
begin
end;

constructor	UFO.Init(x,y,z,vx,vy,vz:integer;col:ColorIndex);
begin
	Next:=nil;
	P.X:=x;
	P.Y:=y;
	P.Z:=z;
	V.X:=vx;
	V.Y:=vy;
	V.Z:=vz;
	Color:=col;
	Time:=0;
	if GrDriver >= EGA then Size:=longint(10) * l div h
	else Size:=longint(5) * l div h;
	AcTime:=180+Random(360);
	PushTime:=18+Random(180);
	Active:=false;
	Dying:=false
end;

constructor	IFO.Init(x,y,z,vx,vy,vz:integer;col:ColorIndex);
begin
	Next:=nil;
	P.X:=x;
	P.Y:=y;
	P.Z:=z;
	V.X:=vx;
	V.Y:=vy;
	V.Z:=vz;
	Color:=col;
	Time:=0;
	Length:=l div 20
end;

destructor	FO.Done;
begin
end;

destructor	UFO.Done;
begin
	ClearMonster(Old.X,Old.Y);
end;

destructor	IFO.Done;
begin
	SetColor(Palette[BackColor]);
	Line(O1.X,O1.Y,O2.X,O2.Y)
end;

procedure	FO.SetSpeed(vx,vy,vz:integer);
begin
	V.X:=V.X+vx;
	V.Y:=V.Y+vy;
	V.Z:=V.Z+vz;
end;

procedure	FO.SwapColor(col:ColorIndex);
begin
	Color:=col;
end;

function	FO.IsIFO:Boolean;
begin
end;

function	UFO.IsIFO:Boolean;
begin
	IsIFO:=false
end;

function	IFO.IsIFO:Boolean;
begin
	IsIFO:=true
end;

function	FO.IsDying:Boolean;
begin
end;

function	UFO.IsDying:Boolean;
begin
	IsDying:=Dying
end;

function	IFO.IsDying:Boolean;
begin
	IsDying:=false
end;

procedure	FO.Step;
begin
end;

procedure	UFO.Step;
var
	Other:	FOPtr;
	New:	Point2;
	NSize:	integer;
begin
	if AcTime <= 0 then Active:=true
	else dec(AcTime);
	dec(PushTime);
	if PushTime <= 0 then begin
		PushTime:=36+Random(360);
		V.X:=V.X-5+Random(10);
		V.Y:=V.Y-5+Random(10);
		if Active then
			NewIFO(	P.X,P.Y,P.Z,
				0,0,-(l-h) div (80-30),Ar2Color);
	end;
	P.X:=P.X+V.X;
	P.Y:=P.Y+V.Y;
	P.Z:=P.Z+V.Z;
	if P.X < -globals.v+dx then begin
		P.X:=-globals.v+dx;
		V.X:=-V.X
	end;
	if P.X >  globals.v-dx then begin
		P.X:= globals.v-dx;
		V.X:=-V.X
	end;
	if P.Y < -w+dy then begin
		P.Y:=-w+dy;
		V.Y:=-V.Y
	end;
	if P.Y >  w-dy then begin
		P.Y:= w-dy;
		V.Y:=-V.Y
	end;
	Other:=List.First;
	while not List.EOL(Other) do begin
		if HitBy(Other) then begin
			List.Del(Other);
			Dying:=true;
		end;
		Other:=List.Next(Other)
	end;
	if Dying then begin
		if Active then inc(PlayPoints,25)
		else inc(PlayPoints,10);
		if PlaySounds then Sound(50)
	end;
	View3(P,New);
	NSize:=longint(Size) * h div P.Z;
	if NotSame(Old,New) or (OSize <> NSize) then begin
		ClearMonster(Old.X,Old.Y);
	end;
	if Dying then BlastMonster(New.X,New.Y)
	else PutMonster(New.X,New.Y,Active);
	Old:=New;
	OSize:=NSize
end;

procedure	IFO.Step;
var
	D:	Point3;
	N1,N2:	Point2;
	Norm:	longint;
begin
	Norm:=round(sqrt(sqr(longint(V.X)) +
		sqr(longint(V.Y)) + sqr(longint(V.Z))));
	P.X:=P.X+V.X;
	P.Y:=P.Y+V.Y;
	P.Z:=P.Z+V.Z;
	D.X:=P.X - longint(V.X) * Length div Norm;
	D.Y:=P.Y - longint(V.Y) * Length div Norm;
	D.Z:=P.Z - longint(V.Z) * Length div Norm;
	View3(P,N1);
	View3(D,N2);
	if NotSame(O1,N1) or NotSame(O2,N2) then begin
		SetColor(Palette[BackColor]);
		Line(O1.X,O1.Y,O2.X,O2.Y);
	end;
	SetColor(Palette[Color]);
	Line(N1.X,N1.Y,N2.X,N2.Y);
	O1:=N1;
	O2:=N2;
end;

function	FO.HitBy(O:FOPtr):Boolean;
begin
end;

function	UFO.HitBy(O:FOPtr):Boolean;
begin
	if O^.IsIFO then
		HitBy:=	(O^.P.Z > P.Z) and
			(abs(O^.P.X-P.X) <= Size) and
			(abs(O^.P.Y-P.Y) <= Size)
	else HitBy:=false
end;

function	IFO.HitBy(O:FOPtr):Boolean;
begin
	HitBy:=false
end;


function	FOList.EOL(O:FOPtr):Boolean;
begin
	EOL:= O = nil
end;

function	FOList.First:FOPtr;
begin
	if Last <> nil then
		First:=Last^.Next
	else
		First:=nil
end;

function	FOList.Next(O:FOPtr):FOPtr;
begin
	if (O = nil) or (O = Last) then
		Next:=nil
	else
		Next:=O^.Next
end;

procedure	FOList.Ins(O:FOPtr);
begin
	if Last <> nil then begin
		O^.Next:=Last^.Next;
		Last^.Next:=O
	end else O^.Next:=O;
	Last:=O
end;

procedure	FOList.Del(var O:FOPtr);
var	P:FOPtr;
begin
	P:=First;
	while	(not EOL(P)) and
		(P^.Next <> O) do
		P:=P^.Next;
	if P^.Next = O then
		if P^.Next = P then begin
			dispose(O,Done);
			Last:=nil;
			O:=nil
		end else begin
			if Last = O then Last:=P;
			P^.Next:=O^.Next;
			dispose(O,Done);
			O:=P^.Next
		end;
end;

procedure	FOList.Flush;
var	O:FOPtr;
begin
	O:=First;
	while not EOL(O) do Del(O)
end;

procedure	NewUFO(x,y,z,vx,vy,vz:integer;col:ColorIndex);
begin
	List.Ins(new(UFOPtr,Init(x,y,z,vx,vy,vz,col)))
end;

procedure	NewIFO(x,y,z,vx,vy,vz:integer;col:ColorIndex);
begin
	List.Ins(new(IFOPtr,Init(x,y,z,vx,vy,vz,col)))
end;

function	Strike(FO1,FO2:FOPtr):Boolean;
begin
	if FO1 = FO2 then Strike:=false
	else begin
		Strike:=false
	end
end;

begin
	List.Last:=nil
end.