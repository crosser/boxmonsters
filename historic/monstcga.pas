program GenImage;
uses Graph;
type
	larray=		array [1..maxint] of byte;
const
	p:array[1..9,1..9] of byte =
	(
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00),
($00,$00,$00,$00,$00,$00,$00,$00,$00)
	);
var
	Img:		^larray;
	GrDriver,
	GrMode:		integer;
	ofile:		file of byte;
	i,j:		integer;
	pos:		longint;
	size:		word;
	a:		char;
begin
	Assign(ofile,'Image.bin');
	rewrite(ofile);
	GrDriver:=CGA;
	GrMode:=CGAC1;
	InitGraph(GrDriver,GrMode,'c:\tp');
	SetBkColor(Blue);
	for i:=1 to 9 do for j:=1 to 9 do
		PutPixel(j,i,p[i,j]);
	size:=ImageSize(1,1,9,9);
	GetMem(Img,size);
	GetImage(1,1,9,9,Img^);
	for pos:=1 to size do
		write(ofile,Img^[pos]);
	close(ofile);
	PutImage(50,50,Img^,NormalPut);
	readln(a);
	CloseGraph;
	writeln('Size=',size);
end.