unit Monsters;

interface

procedure	PutMonster(x,y:integer;Red:Boolean);
procedure	BlastMonster(x,y:integer);
procedure	ClearMonster(x,y:integer);

implementation

uses Graph,Graph3d,Globals;

{$L RedMons	}
{$L GrenMons	}
{$L NoMons	}
{$L CgaGreen	}
{$L CgaRed	}
{$L CgaNo	}

procedure	RedMons;external;
procedure	GrenMons;external;
procedure	NoMons;external;
procedure	CgaGreen;external;
procedure	CgaRed;external;
procedure	CgaNo;external;

procedure	PutMonster(x,y:integer;Red:Boolean);
var
	Img:		pointer;
begin
	case GrDriver of
		EGA,VGA:begin
			if Red then Img:=@RedMons
			else Img:=@GrenMons;
			PutImage(x-9,y-9,Img^,NormalPut)
		end;
		CGA:begin
			if Red then Img:=@CgaRed
			else Img:=@CgaGreen;
			PutImage(x-4,y-4,Img^,NormalPut)
		end;
		else begin
			if Red then
				SetFillStyle(SolidFill,Palette[Obj1Color])
			else	SetFillStyle(SolidFill,Palette[Obj2Color]);
			Bar(x-9,y-9,x+9,y+9)
		end
	end;
end;

procedure	BlastMonster(x,y:integer);
begin
	SetFillStyle(SolidFill,White);
	if GrDriver < EGA then
		Bar(x-4,y-4,x+4,y+4)
	else	Bar(x-9,y-9,x+9,y+9);
end;

procedure	ClearMonster(x,y:integer);
var
	Img:		pointer;
begin
	case GrDriver of
		EGA,VGA:begin
			Img:=@NoMons;
			PutImage(x-9,y-9,Img^,NormalPut)
		end;
		CGA:begin
			Img:=@CgaNo;
			PutImage(x-4,y-4,Img^,NormalPut)
		end;
		else begin
			SetFillStyle(SolidFill,Palette[BackColor]);
			if GrDriver < EGA then
				Bar(x-4,y-4,x+4,y+4)
			else	Bar(x-9,y-9,x+9,y+9);
		end
	end
end;

end.