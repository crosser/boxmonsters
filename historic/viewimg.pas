program GenImage;
uses Graph;
type
	larray=		array [1..maxint] of byte;
var
	Img:		^larray;
	GrDriver,
	GrMode:		integer;
	ofile:		file of byte;
	i,j:		integer;
	pos:		longint;
	size:		word;
begin
	Assign(ofile,'Image.bin');
	reset(ofile);
	GrDriver:=Detect;
	InitGraph(GrDriver,GrMode,'c:\tp');
	size:=ImageSize(1,1,19,19);
	GetMem(Img,size);
	for pos:=1 to size do
		read(ofile,Img^[pos]);
	close(ofile);
	PutImage(50,50,Img^,NormalPut);
	CloseGraph;
	writeln('Size=',size);
end.