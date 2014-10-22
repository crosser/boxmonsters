unit Graph3d;

interface

var
	GrDriver,
	GrMode:		integer;

procedure GrOpen;
procedure GrClose;
procedure Clear;
procedure Die;
procedure Crosshair;

implementation

uses Graph,Globals,Timer;

var
	ax,ay:		word;
	hx,hy:		integer;

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
	MaxX:=GetMaxX;
	MaxY:=GetMaxY;
	dx:=MaxX div 2;
	dy:=MaxY div 2;
	h:=dX*3;
	l:=h*10;
	v:=dX*4;
	w:=dY*4;
	MyX:=0;
	MyY:=0;
	GetAspectRatio(ax,ay);
	hx:=dx div 30 * ax div ay;
	hy:=dy div 30;
	if GrDriver < EGA then begin
		Palette[BackColor]:=0;
		Palette[BoxColor]:=3;
		Palette[CrossColor]:=1;
		Palette[Ar1Color]:=1;
		Palette[Ar2Color]:=2;
		Palette[Obj1Color]:=1;
		Palette[Obj2Color]:=2;
		Palette[PaperColor]:=3;
		Palette[LetterColor]:=0;
		SetBkColor(Blue);
		ClearDevice
	end else begin
		Palette[BackColor]:=Blue;
		Palette[BoxColor]:=White;
		Palette[CrossColor]:=LightGray;
		Palette[Ar1Color]:=LightCyan;
		Palette[Ar2Color]:=LightRed;
		Palette[Obj1Color]:=Cyan;
		Palette[Obj2Color]:=Red;
		Palette[PaperColor]:=LightGray;
		Palette[LetterColor]:=Black;
		SetFillStyle(SolidFill,Palette[BackColor]);
		Bar(0,0,MaxX,MaxY)
	end;
end; {of procedure GrOpen}

procedure GrClose;
begin
	CloseGraph
end; {of procedure GrClose}

procedure Clear;
begin
	ClearDevice;
	SetFillStyle(SolidFill,Palette[BackColor]);
	Bar(0,0,MaxX,MaxY)
end; {Clear}

procedure Crosshair;
begin
	SetColor(Palette[CrossColor]);
	Line(dx-hx-hx,dy,dx-hx,dy);
	Line(dx,dy-hy-hy,dx,dy-hy);
	Line(dx+hx,dy,dx+hx+hx,dy);
	Line(dx,dy+hy,dx,dy+hy+hy)
end; {Crosshair}

procedure Die;
var
	i,j:		integer;
begin
	if GrDriver < EGA then for i := MaxColors downto 0 do begin
		SetBkColor(i);
		WaitTick
	end else for i := MaxColors downto 0 do begin
		SetPalette(Palette[BackColor],i);
		WaitTick
	end;
	if GrDriver < EGA then SetBkColor(Blue)
	else SetPalette(Palette[BackColor],Blue);
end; {Die}

end.