unit Globals;

interface

type
	Point2=	record
			X,Y:	integer
		end;

	Point3=	record
			X,Y,Z:	integer
		end;
	ColorIndex=	(	BackColor,BoxColor,CrossColor,
				Ar1Color,Ar2Color,
				Obj1Color,Obj2Color,
				PaperColor,LetterColor);

var
	PlaySounds:		Boolean;
	PlayPoints:		integer;
	MyX,MyY,
	MyVx,MyVy:		integer;
	dx,dy:			integer;
	h,l,v,w:		integer;
	MaxX,MaxY:		integer;
	Palette:		array [ColorIndex] of word;

procedure	View3(P3:Point3;var P2:Point2);
function	NotSame(P1,P2:Point2):Boolean;
function	UCline(line:string):string;
procedure 	MoveMyself;

implementation

function UCline(line:string):string;
var
	i:	integer;
begin
	for i:=1 to length(line) do
		UCline[i]:= UpCase(line[i]);
	UCline[0]:=chr(length(line));
end; {of function UCline}

procedure	View3(P3:Point3;var P2:Point2);
begin
	P2.X:=	dx + longint(P3.X-MyX) * h div P3.Z;
	P2.Y:=	dy - longint(P3.Y-MyY) * h div P3.Z
end; {of procedure View3}

function	NotSame(P1,P2:Point2):Boolean;
begin
	NotSame:=(P1.X <> P2.X) or (P1.Y <> P2.Y)
end; {of function NotSame}

procedure	MoveMyself;
begin
	MyX:=MyX+MyVx;
	MyY:=MyY+MyVy;
	if MyX < -v+dx then begin
		MyX:=-v+dx;
		MyVx:=0
	end;
	if MyX > v-dx then begin
		MyX:=v-dx;
		MyVx:=0
	end;
	if MyY < -w+dy then begin
		MyY:=-w+dy;
		MyVy:=0
	end;
	if MyY > w-dy then begin
		MyY:=w-dy;
		MyVy:=0
	end;
end;

begin
	PlaySounds:=true;
end.