program info;
uses Graph;
var
	GrDriver,GrMode:	integer;
	a:			string;
	MaxX,MaxY:		integer;
	Step:			integer;
	img:			pointer;
	i,j:			integer;
begin
	GrDriver:=Detect;
	GrMode:=Detect;
	InitGraph(GrDriver,GrMode,'c:\tp');
	MaxX:=GetMaxX;
	MaxY:=GetMaxY;
	GetMem(img,ImageSize(0,0,MaxX,0));
	SetFillStyle(SolidFill,Blue);
	Bar(0,0,MaxX,MaxY);
	SetColor(White);
	SetTextStyle(SmallFont,HorizDir,8);
	Step:=TextHeight(a) * 5 div 4;
	repeat
		readln(a);
		MoveTo(0,MaxY - Step * 2);
		OutText(a);
		for j:=1 to Step do
		for i:=1 to MaxY do begin
			GetImage(0,i,MaxX,i,img^);
			PutImage(0,i-1,img^,NormalPut)
		end
	until a='end';
	CloseGraph;
end.