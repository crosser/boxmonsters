unit InfoUnit;

interface

procedure Info(Height:integer);

implementation

uses CRT,Graph,Globals;

const
	numlines=	16;
	itext:array [1..numlines] of string[60]=(
	'The goal of the game is to eliminate as many',
	'monsters as You can. Green monster costs 10',
	'points, red - 25 points.',
	'Each shoot decreases score by 1 point.',
	'   ! ! ! Be aware of Flying Arrows ! ! !',
	'',
	'Use of the keyboard:',
	' - Direction keys moves You',
	' - SHFT increases Your speed, CTRL decreases',
	' - ENTER and Space bar shoot',
	' - PAUSE suspends, any key resumes',
	' - F2 toggles sounds on/off',
	' - ESC exits',
	'',
	'   Good luck! You will need it!',
	'Eugene G.Crosser, Moscow, Tel. (095)212-5274');
procedure Info(Height:integer);
var
	i,xpos,ypos,step:	integer;
begin
	SetTextStyle(SmallFont,HorizDir,Height);
	SetTextJustify(LeftText,TopText);
	SetFillStyle(SolidFill,Palette[PaperColor]);
	SetColor(Palette[LetterColor]);
	step:=TextHeight('I') * 5 div 4;
	xpos:=MaxX div 10;
	ypos:=MaxY div 10;
	Rectangle(xpos-9,ypos-9,MaxX-xpos+1,MaxY-ypos+1);
	Rectangle(xpos-7,ypos-7,MaxX-xpos+3,MaxY-ypos+3);
	Rectangle(xpos-5,ypos-5,MaxX-xpos+5,MaxY-ypos+5);
	Bar(xpos-4,ypos-4,MaxX-xpos+4,MaxY-ypos+4);
	for i:=1 to numlines do
		OutTextXY(xpos,Ypos+(i-1)*step,itext[i]);
	if ReadKey=#0 then if ReadKey=' ' then
end;
end.