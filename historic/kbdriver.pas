unit KBDriver;

interface

type
	SpeedType=	(Low,Norm,High);
	DirType=	(Stay,N,NE,E,SE,S,SW,W,NW);
var
	SpeedIndex:	SpeedType;
	DirIndex:	DirType;
	Escape,
	Pause,
	Sounds,
	Enter:		Boolean;

procedure	KBDInit;
procedure	KBDReset;

implementation

uses Dos;

var
	OldInt9:	pointer;
	Ctrl,
	Shft:		Boolean;
	PresSet:	set of DirType;
	st1,st2,
	key:		byte;
	LastDir:	DirType;
	Nkeys:		integer;
	accept:		Boolean;

procedure KBDISR(Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP:Word);interrupt;
begin
	key:=port[$60];
	Escape:=false;
	Pause:=false;
	Enter:=false;
	if (key = 224) or (key = 225) then accept:=false
	else if accept or ((key <> 42) and (key <> 170)) then
	if key < 128 then case key of
		1:	Escape:=true;
		28,57:	Enter:=true;
		60:	Sounds:=true;
		69:	Pause:=true;
		42,54:	Shft:=true;
		29:	Ctrl:=true;
		71:     begin PresSet:=PresSet+[NW];LastDir:=NW end;
		72:     begin PresSet:=PresSet+[N]; LastDir:=N  end;
		73:     begin PresSet:=PresSet+[NE];LastDir:=NE end;
		75:     begin PresSet:=PresSet+[W]; LastDir:=W  end;
		77:     begin PresSet:=PresSet+[E]; LastDir:=E  end;
		79:     begin PresSet:=PresSet+[SW];LastDir:=SW end;
		80:     begin PresSet:=PresSet+[S]; LastDir:=S  end;
		81:     begin PresSet:=PresSet+[SE];LastDir:=SE end;
	end else case key-128 of
		1:	{Escape:=true};
		28,57:	{Enter:=true};
		60:	{Sounds:=true};
		69:	Pause:=true;
		42,54:	Shft:=false;
		29:	Ctrl:=false;
		71:     PresSet:=PresSet-[NW];
		72:     PresSet:=PresSet-[N];
		73:     PresSet:=PresSet-[NE];
		75:     PresSet:=PresSet-[W];
		77:     PresSet:=PresSet-[E];
		79:     PresSet:=PresSet-[SW];
		80:     PresSet:=PresSet-[S];
		81:     PresSet:=PresSet-[SE];
	end else accept:=true;
	if Shft then SpeedIndex:=High
	else if Ctrl then SpeedIndex:=Low
	else SpeedIndex:=Norm;
	if PresSet = [] then DirIndex:=Stay
	else begin
		Nkeys:=0;
		for DirIndex:=Stay to NW do
			if DirIndex in PresSet then inc(Nkeys);
		case Nkeys of
			0: DirIndex:=Stay;
			1: begin
				DirIndex:=Stay;
				repeat DirIndex:=succ(DirIndex)
				until DirIndex in PresSet
			end;
			2:	     if PresSet = [N,E] then DirIndex:=NE
				else if PresSet = [E,S] then DirIndex:=SE
				else if PresSet = [S,W] then DirIndex:=SW
				else if PresSet = [W,N] then DirIndex:=NW;
			else DirIndex:=LastDir
		end {of case}
	end;
	st1:=port[$61];
	st2:=st1 or $80;
	port[$61]:=st2;
	port[$61]:=st1;
	port[$20]:=$20
end;

procedure	KBDInit;
begin
	Escape:=false;
	Pause:=false;
	Sounds:=false;
	Enter:=false;
	Ctrl:=false;
	Shft:=false;
	PresSet:=[];
	accept:=true;
	GetIntVec($09,OldInt9);
	SetIntVec($09,@KBDISR)
end;

procedure	KBDReset;
begin
	SetIntVec($09,OldInt9);
end;

end.