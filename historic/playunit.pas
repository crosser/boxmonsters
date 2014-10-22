unit PlayUnit;

interface

function Play:integer;

implementation

uses CRT,Graph3d,Timer,Objects,BoxUnit,Globals,KBDriver;
function Play:integer;
var
	key:		char;
	avx,avy,avz:	integer;
	EndPlay,
	Killed:		Boolean;
	Current:	FOPtr;
	vxa,vya:	array [SpeedType] of integer;
	PlayTime:	integer;
begin
	avx:=0;
	avy:=longint(dy) div longint(48);
	avz:=longint(l-h) div longint(48);
	vxa[Low]:=v div 180;
	vxa[Norm]:=v div 45;
	vxa[High]:=v div 18;
	vya[Low]:=globals.w div 180;
	vya[Norm]:=globals.w div 45;
	vya[High]:=globals.w div 18;
	MyVx:=0;
	MyVy:=0;
	MyX:=0;
	MyY:=0;
	EndPlay:=false;
	PlayPoints:=0;
	PlayTime:=Random(48);
	Killed:=false;
	SetTimer;
	Clear;
	List.Flush;
	Box.Step;
	KBDInit;
	repeat
		case DirIndex of
			Stay:	begin
					MyVx:=0;
					MyVy:=0
				end;
			N:	begin
					MyVx:=0;
					MyVy:=vya[SpeedIndex]
				end;
			NE:	begin
					MyVx:=vxa[SpeedIndex];
					MyVy:=vya[SpeedIndex]
				end;
			E:	begin
					MyVy:=0;
					MyVx:=vxa[SpeedIndex];
				end;
			SE:	begin
					MyVx:=vxa[SpeedIndex];
					MyVy:=-vya[SpeedIndex]
				end;
			S:	begin
					MyVx:=0;
					MyVy:=-vya[SpeedIndex]
				end;
			SW:	begin
					MyVx:=-vxa[SpeedIndex];
					MyVy:=-vya[SpeedIndex]
				end;
			W:	begin
					MyVy:=0;
					MyVx:=-vxa[SpeedIndex];
				end;
			NW:	begin
					MyVx:=-vxa[SpeedIndex];
					MyVy:=vya[SpeedIndex]
				end;
		end;
		while Pause do;
		if Enter then begin
			NewIFO(	MyX,MyY-dy,h,
				avx,avy,avz,
				Ar1Color);
			dec(PlayPoints);
			Enter:=false
		end;
		if Pause then begin
			Pause:=false
		end;
		if Sounds then begin
			PlaySounds:=not PlaySounds;
			Sounds:=false
		end;
		if Escape then EndPlay:=true;

		Current:=List.First;
		while not List.EOL(Current) do begin
			if Current^.IsDying then begin
				List.Del(Current);
				if PlaySounds then NoSound
			end;
			if not List.EOL(Current) then begin
			Current^.Step;
			if (Current^.P.Z < h) then begin
				if (abs(Current^.P.X-MyX) < dx) and
				   (abs(Current^.P.Y-MyY) < dy)
				   then begin
					EndPlay:=true;
					Killed:=true
				end;
				List.Del(Current)
			end else begin
				if (Current^.P.Z > l) then begin
					Current^.SetSpeed(0,0,-2*avz);
					Current^.SwapColor(Ar2Color)
				end
			end;
			Current:=List.Next(Current)
			end
		end;
		Box.Step;
		MoveMyself;
		Crosshair;
		dec(PlayTime);
		if PlayTime <= 0 then begin
			NewUFO(	dx-v+Random(2*(v-dx)),
				dy-globals.w+Random(2*(globals.w-dy)),l,
				0,0,0,
				Obj1Color);
			PlayTime:=48+Random(180);
		end;
		WaitTick;
	until EndPlay;
	if Killed then begin
		if PlaySounds then begin
			Sound(50);
			WaitTick;
			NoSound
		end;
		Die;
	end;
	KBDReset;

	if PlayPoints > 0 then Play:=PlayPoints else Play:=0;
	ResetTimer
end; {of function Play}
end.