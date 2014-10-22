unit BoxUnit;

interface

uses Graph,Globals;

type
	BoxT=	object
			O1,O2,O3,O4,
			O5,O6,O7,O8:	Point2;
			procedure	Step;
		end;

var
	Box:	BoxT;

implementation

procedure BoxT.Step;
var
	N1,N2,N3,N4,
	N5,N6,N7,N8:	Point2;
begin
	N1.X:= dx + longint(-v-MyX) * h div l;
	N1.Y:= dy - longint( w-MyY) * h div l;
	N2.X:= dx + longint( v-MyX) * h div l;
	N2.Y:= N1.Y;
	N3.X:= N2.X;
	N3.Y:= dy - longint(-w-MyY) * h div l;
	N4.X:= N1.X;
	N4.Y:= N3.Y;
	N5.X:= dx -v-MyX;
	N5.Y:= dy -w-MyY;
	N6.X:= dx +v-MyX;
	N6.Y:= N5.Y;
	N7.X:= N6.X;
	N7.Y:= dy +w-MyY;
	N8.X:= N5.X;
	N8.Y:= N7.Y;
	SetColor(Palette[BoxColor]);
	if NotSame(N1,O1) or NotSame(N2,O2) then begin
		SetColor(Palette[BackColor]);
		Line(O1.X,O1.Y,O2.X,O2.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N1.X,N1.Y,N2.X,N2.Y);
	if NotSame(N2,O2) or NotSame(N3,O3) then begin
		SetColor(Palette[BackColor]);
		Line(O2.X,O2.Y,O3.X,O3.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N2.X,N2.Y,N3.X,N3.Y);
	if NotSame(N3,O3) or NotSame(N4,O4) then begin
		SetColor(Palette[BackColor]);
		Line(O3.X,O3.Y,O4.X,O4.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N3.X,N3.Y,N4.X,N4.Y);
	if NotSame(N4,O4) or NotSame(N1,O1) then begin
		SetColor(Palette[BackColor]);
		Line(O4.X,O4.Y,O1.X,O1.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N4.X,N4.Y,N1.X,N1.Y);
	if NotSame(N1,O1) or NotSame(N5,O5) then begin
		SetColor(Palette[BackColor]);
		Line(O1.X,O1.Y,O5.X,O5.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N1.X,N1.Y,N5.X,N5.Y);
	if NotSame(N2,O2) or NotSame(N6,O6) then begin
		SetColor(Palette[BackColor]);
		Line(O2.X,O2.Y,O6.X,O6.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N2.X,N2.Y,N6.X,N6.Y);
	if NotSame(N3,O3) or NotSame(N7,O7) then begin
		SetColor(Palette[BackColor]);
		Line(O3.X,O3.Y,O7.X,O7.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N3.X,N3.Y,N7.X,N7.Y);
	if NotSame(N4,O4) or NotSame(N8,O8) then begin
		SetColor(Palette[BackColor]);
		Line(O4.X,O4.Y,O8.X,O8.Y);
		SetColor(Palette[BoxColor]);
	end;
	Line(N4.X,N4.Y,N8.X,N8.Y);
	O1.X:=N1.X;O1.Y:=N1.Y;
	O2.X:=N2.X;O2.Y:=N2.Y;
	O3.X:=N3.X;O3.Y:=N3.Y;
	O4.X:=N4.X;O4.Y:=N4.Y;
	O5.X:=N5.X;O5.Y:=N5.Y;
	O6.X:=N6.X;O6.Y:=N6.Y;
	O7.X:=N7.X;O7.Y:=N7.Y;
	O8.X:=N8.X;O8.Y:=N8.Y;
end; {of procedure Box.Step}
end.