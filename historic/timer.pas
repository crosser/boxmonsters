unit Timer;

interface

procedure SetTimer;
procedure ResetTimer;
procedure WaitTick;

implementation

uses Dos;
var
	TimerSet,
	TickEncounted:	Boolean;
	OldInt1C:	pointer;
{$F+}
procedure TickISR(Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP: Word);interrupt;
begin
	TickEncounted:=true
end; {TickISR}
{$F-}

procedure SetTimer;
begin
	TimerSet:=true;
	TickEncounted:=false;
	GetIntVec($1C,OldInt1C);
	SetIntVec($1C,@TickISR)
end; {SetTimer}

procedure ResetTimer;
begin
	TimerSet:=false;
	TickEncounted:=false;
	SetIntVec($1C,OldInt1C)
end; {ResetTimer}

procedure WaitTick;
begin
	if TimerSet then
		repeat until TickEncounted;
	TickEncounted:=false
end; {WaitTick}

begin
	TimerSet:=false
end.