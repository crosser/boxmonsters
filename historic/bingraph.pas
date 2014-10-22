unit BinGraph;

interface

implementation

uses Graph;
{$L cgabgi	}
{$L egabgi	}
{$L hrcbgi	}
{$L gothchr	}
{$L littchr	}

procedure cgabgi;external;
procedure egabgi;external;
procedure hrcbgi;external;
procedure gothchr;external;
procedure littchr;external;

begin
	if	(RegisterBGIDriver(@cgabgi) < 0) or
		(RegisterBGIDriver(@egabgi) < 0) or
		(RegisterBGIDriver(@hrcbgi) < 0) or
		(RegisterBGIFont(@gothchr) < 0) or
		(RegisterBGIFont(@littchr) < 0) then begin
		writeln('Internal error: unable to register BGI');
		halt(1)
	end
end.