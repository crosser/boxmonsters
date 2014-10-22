unit LogoUnit;

interface

procedure Logo;

implementation

uses CRT,Graph3d,Graph,PlayUnit,ScorUnit,Timer,Objects,Globals,InfoUnit;

var
	ScoreLine:		ScoreType;

procedure Logo;
var
	TitleSize,
	TableSize,
	Margin,
	TableTop,
	TableStep:		integer;
	Points:			integer;
	key:			char;
	i:			integer;
procedure DrawTitle;
var
	i:		integer;
	strn:		string[12];
begin
	Clear;
	SetColor(GetMaxColor);
	SetLineStyle(SolidLn,0,ThickWidth);
	Rectangle(5,5,MaxX-5,MaxY-5);
	SetTextJustify(CenterText,TopText);
	SetTextStyle(GothicFont,HorizDir,TitleSize);
	OutTextXY(MaxX div 2,MaxY div 15,
		'Flying Arrows');
	SetTextJustify(CenterText,BottomText);
	SetTextStyle(SmallFont,HorizDir,TitleSize);
	OutTextXY(MaxX div 2,MaxY - MaxY div 15,
		'By Eugene G. Crosser, Moscow, 1990.');
	SetTextStyle(SmallFont,HorizDir,TableSize);
	OutTextXY(MaxX div 2,MaxY - MaxY div 6,
		'ENTER - Play, ESC - Exit, I - Instructions.');
	for i:=1 to 5 do begin
		SetTextJustify(LeftText,BottomText);
		OutTextXY(Margin,TableTop+TableStep*i,Score[i].Name);
		OutTextXY(dx+Margin,TableTop+TableStep*i,Score[i+5].Name);
		SetTextJustify(RightText,BottomText);
		str(Score[i].Points,strn);
		OutTextXY(dx-Margin,TableTop+TableStep*i,strn);
		str(Score[i+5].Points,strn);
		OutTextXY(dx+dx-Margin,TableTop+TableStep*i,strn);
	end;
	SetLineStyle(SolidLn,0,NormWidth);
end; {DrawTitle}

function ReadName:Boolean;
var
	Xpos,
	Ypos:		integer;
	i:		integer;
	charline:	string[11];
	sizeline:	array [1..11] of integer;
	posline:	array [1..12] of integer;
	key:		char;
begin
	SetFillStyle(SolidFill,Palette[BackColor]);
	SetTextJustify(RightText,BottomText);
	SetTextStyle(SmallFont,HorizDir,TitleSize);
	Xpos:=(MaxX-TextWidth('Please enter Your name: WWWWWWWWWWW')) div 2 +
		TextWidth('Please enter Your name: ');
	Ypos:=MaxY-MaxY div 6;
	Bar(	8,Ypos-TextHeight('I'),
		MaxX-8,Ypos+TextHeight('I') div 3);
	OutTextXY(Xpos,Ypos,
		'Please enter Your name: ');
	SetTextJustify(LeftText,BottomText);
	charline:=ScoreLine.Name;
	PosLine[1]:=Xpos;
	i:=0;
	for i:=1 to length(charline) do begin
		sizeline[i]:=TextWidth(charline[i]);
		posline[i+1]:=posline[i]+TextWidth(charline[i]);
		OutTextXY(posline[i],Ypos,charline[i]);
	end;
	repeat
		key:=ReadKey;
		if key=#0 then if ReadKey=#75 then key:=^H;
		case key of
		^M:ReadName:=true;
		^[:ReadName:=false;
		^H:if i>0 then begin
			Bar(	posline[i],
				Ypos-TextHeight(charline[i]),
				posline[i]+sizeline[i],
				Ypos+TextHeight(charline[i]) div 3);
			dec(i);
			charline:=copy(charline,1,i);
		end;
		#32..#126:if i<11 then begin
			inc(i);
			charline:=charline+key;
			sizeline[i]:=TextWidth(key);
			posline[i+1]:=posline[i]+TextWidth(key);
			OutTextXY(posline[i],Ypos,key);
		end;
		else if PlaySounds then begin
			Sound(2000);
			Delay(50);
			NoSound
		end
		end; {of case}
	until key in [^M,^[];
	ScoreLine.Name:=charline;
end; {ReadName}

procedure Fly;
var
	i:		integer;
begin
	Randomize;
	NewIFO(	-dx+Random(dx*2),-dy+Random(dy*2),l,
		0,0,-(l-h) div (80-30),Ar2Color);
	SetTimer;
	for i:=80 downto 30 do begin
		if PlaySounds then Sound(i*75);
		List.First^.Step;
		WaitTick;
	end;
	if PlaySounds then Sound(50);
	WaitTick;
	if PlaySounds then NoSound;
	List.Flush;
	Die;
	ResetTimer;
end; {Fly}

begin {Logo body}
	if GrDriver < EGA then begin
		TitleSize:=5;
		TableSize:=4;
	end else begin
		TitleSize:=8;
		TableSize:=6;
	end;
	TableTop:=MaxY div 3;
	TableStep:=MaxY div 15;
	Margin:=MaxX div 15;
	DrawTitle;
	Fly;
	repeat
		repeat
			key:=UpCase(ReadKey);
			if key=#0 then if ReadKey=' ' then;
			if key='S' then PlaySounds:=not PlaySounds
		until key in [^M,' ','P',#27,'Q','I'];
		if key in [^M,' ','P'] then begin
			Points:=Play;
			DrawTitle;
			while KeyPressed do
				if ReadKey=#0 then
					if ReadKey=' 'then;
			if Points > Score[10].Points then begin
				if ReadName then begin
					ScoreLine.Points:=Points;
					WriteScore(ScoreLine)
				end;
				DrawTitle
			end
		end;
		if key = 'I' then begin
			Info(TableSize);
			DrawTitle
		end
	until key in [#27,'Q'];
end; {of procedure Logo}

begin
	ScoreLine.Name:='';
	ScoreLine.Points:=0;
end.