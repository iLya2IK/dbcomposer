unit syntaxanalizer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OGLFastList, regexpr;

type

FloatArray  = array[0..$efff] of Single;
TFloatArray = FloatArray;
PFloatArray = ^FloatArray;

TSinTokenKind = (sktRusString, sktEnString, sktNumber);

{ TSinToken }

TSinToken = class
private
  FToken : String;
  FKind : TSinTokenKind;
  class function CheckIsLat(const S : String) : Word;
  class function CheckIsNumeric(const S : String) : Word;
  class function DLdist(const afirst, asecond : String) : Single;
  class function LatToCyr(const S : String) : String;
public
  constructor Create(const aToken : String);
  function Compare(aE : TSinToken) : Single;
  property Token : String read FToken;
  property Kind : TSinTokenKind read FKind;
end;

{ TSinExpr }

TSinExpr = class(TFastCollection)
private
  FOrigExpr : String;
  function GetToken(index : integer): TSinToken;
  class function DLdist(const afirst, asecond: TSinExpr) : Single;
public
  constructor Create(const aExpr : String); overload;
  property Token[index : integer] : TSinToken read GetToken; default;
  function Compare(aE : TSinExpr) : Single;
  function Contain(aE : TSinExpr) : Single;
  property OrigExpr : String read FOrigExpr write FOrigExpr;
end;

implementation

uses Math, LazUTf8;

{ TSinExpr }

function TSinExpr.GetToken(index: integer): TSinToken;
begin
  Result := TSinToken(Item[index]);
end;

class function TSinExpr.DLdist(const afirst, asecond: TSinExpr): Single;
var firstLength, secondLength, amax, i, afrom, ato, j : integer;
    value, cost : Single;
    secondCh, firstCh : TSinToken;
    currentRow, previousRow, transpositionRow, tempRow, data : PFloatArray;
    currentRowLen : Integer;
begin
    firstLength := afirst.Count;
    secondLength := asecond.Count;

    if (firstLength = 0) then Exit(1.0)
     else if (secondLength = 0) then Exit(1.0);

    if (firstLength > secondLength) then begin
       Exit(DLDist(asecond, afirst));
    end;

    amax := secondLength;
    if (secondLength - firstLength > amax) then Exit(1.0);

    currentRowLen := (firstLength + 1) ;

    data := AllocMem(currentRowLen * 3 * sizeOf(single));
    currentRow := @(data^[0]);
    previousRow := @(data^[currentRowLen]);
    transpositionRow := @(data^[currentRowLen * 2]);

    for i := 0 to firstLength do
      previousRow^[i] := i;

    for i := 1 to secondLength do begin
      secondCh := asecond[i-1];
      currentRow^[0] := i;

      afrom := Math.max(i - amax - 1, 1);
      ato := Math.min(i + amax + 1, firstLength);

      for j := afrom to ato do begin
          firstCh := afirst[j-1];

          cost  := Math.min(firstCh.Compare(secondCh), 1.0);

          value := Math.min(Math.min(currentRow^[j - 1] + 1.0, previousRow^[j] + 1.0), previousRow^[j - 1] + cost);

          currentRow^[j] := value;
      end;

      tempRow := transpositionRow;
      transpositionRow := previousRow;
      previousRow := currentRow;
      currentRow := tempRow;
    end;
    Result := Math.min(previousRow^[firstLength] / (firstLength + secondLength) * 2.0, 1.0);
    FreeMem(data);
end;

constructor TSinExpr.Create(const aExpr: String);
var
    re : TRegExpr;
    MExpr : String;
    T : TSinToken;
begin
  inherited Create;
  FOrigExpr:=aExpr;
  MExpr := UTF8LowerCase(Utf8Trim(FOrigExpr));
  re := TRegExpr.Create(widestring('(([а-я]+)|([a-z]+)|(\d+))'));
  try
    if re.Exec(widestring(MExpr)) then
    begin
      repeat
        T := TSinToken.Create(UTF8Encode(re.Match[0]));
        Add(T);
      until not re.ExecNext;
    end;
  finally
    re.Free;
  end;
end;

function TSinExpr.Compare(aE: TSinExpr): Single;
begin
  Result := DLdist(Self, aE);
end;

function TSinExpr.Contain(aE: TSinExpr): Single;
var i, j, k : integer;
    v : Single;
begin
  if aE.Count = 0 then Exit(1);

  Result := 1;
  i := 0;
  while i <= (Count - aE.Count) do
  begin
    v := 0;
    k := 0;
    for j := i to (i + aE.Count - 1) do
    begin
      v := Math.Max(Token[i].Compare(aE.Token[k]), v);
      inc(k);
    end;
    if Result > v then Result := v;
    Inc(i);
  end;
end;

{ TSinToken }

constructor TSinToken.Create(const aToken: String);
begin
  FToken := aToken;
  if CheckIsNumeric(FToken) = 1 then
     FKind:= sktNumber else
  begin
    if CheckIsLat(FToken) = 1 then
       FKind:= sktEnString else
       FKind:= sktRusString;
  end;
end;

function TSinToken.Compare(aE: TSinToken): Single;
begin
  if not Assigned(aE) then Exit(1.0);
  if (Kind <> aE.Kind) then
  begin
    if (Kind = sktNumber) or (aE.Kind = sktNumber) then
        Result := 1 else
        Result := DLdist(LatToCyr(FToken), LatToCyr(aE.Token));
  end else
     Result := DLdist(FToken, aE.Token);
end;

class function TSinToken.LatToCyr(const S: String): String;
var i, l : Integer;
  ch : String;
  passed: Boolean;
begin
  i := 1;
  L := UTF8Length(s);
  Result := '';
  while(i <= L) do // Идем по строке слева направо. В принципе, подходит для обработки потока
  begin
    ch := UTF8Copy(S, i, 1);
    passed := true;
    if (ch = 'j') then // Префиксная нотация вначале
    begin
         passed := false;
         inc(i); // преходим ко второму символу сочетания
         if i > L then break;
         ch := UTF8Copy(S, i, 1);
         if ch = 'e' then Result:= Result + 'ё' else
         if ch = 's' then
           Result := Result+ 'щ'
         else
         if ch = 'h' then Result:= Result+ 'ь' else
         if ch = 'u' then Result:= Result+ 'ю' else
         if ch = 'a' then Result:= Result+ 'я' else begin
           dec(i);
           ch := 'j';
           passed := true;
         end;
    end else
    if(((i+1) <= L) and
       (UTF8Copy(S, i+1, 1)='h') and
       not (((i+2) <= L) and (UTF8Copy(S, i+2, 1)='h'))) then
       // Постфиксная нотация, требует информации о двух следующих символах.
       // Для потока придется сделать обертку с очередью из трех символов.
    begin
      passed := false;
	if (ch = 'z') then Result:= Result+ 'ж' else
	if (ch = 'k') then Result:= Result+ 'х' else
	if (ch = 'c') then Result:= Result+ 'ч' else
	if (ch = 's') then Result:= Result+ 'ш' else
	if (ch = 'e') then Result:= Result+ 'э' else
	if (ch = 'h') then Result:= Result+ 'ъ' else
	if (ch = 'i') then Result:= Result+ 'ы' else
           passed := true;
        if not passed then
	   inc(i); // пропускаем постфикс
     end;
    if passed then
    begin // одиночные символы
      if (ch = 'a') then Result:= Result+ 'а' else
      if (ch = 'b') then Result:= Result+ 'б' else
      if (ch = 'v') then Result:= Result+ 'в' else
      if (ch = 'w') then Result:= Result+ 'в' else
      if (ch = 'g') then Result:= Result+ 'г' else
      if (ch = 'd') then Result:= Result+ 'д' else
      if (ch = 'e') then Result:= Result+ 'е' else
      if (ch = 'z') then Result:= Result+ 'з' else
      if (ch = 'i') then Result:= Result+ 'и' else
      if (ch = 'y') then Result:= Result+ 'й' else
      if (ch = 'k') then Result:= Result+ 'к' else
      if (ch = 'l') then Result:= Result+ 'л' else
      if (ch = 'm') then Result:= Result+ 'м' else
      if (ch = 'n') then Result:= Result+ 'н' else
      if (ch = 'o') then Result:= Result+ 'о' else
      if (ch = 'p') then Result:= Result+ 'п' else
      if (ch = 'r') then Result:= Result+ 'р' else
      if (ch = 's') then Result:= Result+ 'с' else
      if (ch = 't') then Result:= Result+ 'т' else
      if (ch = 'u') then Result:= Result+ 'у' else
      if (ch = 'f') then Result:= Result+ 'ф' else
      if (ch = 'c') then Result:= Result+ 'ц' else
      if (ch = 'x') then Result:= Result+ 'кс' else
      if (ch = 'q') then Result:= Result+ 'ку' else
      if (ch = 'j') then Result:= Result+ 'дж' else
      if (ch = 'h') then Result:= Result+ 'х' else
         Result:= Result+ ch
    end;
    inc(i); // переходим к следующему символу
  end;
end;

class function TSinToken.DLdist(const afirst, asecond: String): Single;

const cLatToPosMat : Array ['a'..'z'] of TPoint =
 {a-e:} ((x:0;y:1), (x:4;y:2), (x:2;y:2), (x:2;y:1), (x:2;y:0),
 {f-j:}  (x:3;y:1), (x:4;y:1), (x:5;y:1), (x:7;y:0), (x:6;y:1),
 {k-o:}  (x:7;y:1), (x:8;y:1), (x:6;y:2), (x:5;y:2), (x:8;y:0),
 {p-u:}  (x:9;y:0), (x:0;y:0), (x:3;y:0), (x:1;y:1), (x:4;y:0), (x:6;y:0),
 {v-z:}  (x:3;y:2), (x:1;y:0), (x:1;y:2), (x:5;y:0), (x:0;y:2));

const cCyrToPosMat1 : Array [$b0..$bf] of TPoint = // $d0 prefix
 {а-е:} ((x:3;y:1), (x:7;y:2), (x:2;y:1), (x:6;y:0), (x:8;y:1), (x:4;y:0),
 {ж-л:}  (x:9;y:1), (x:9;y:0), (x:4;y:2), (x:0;y:0), (x:3;y:0), (x:7;y:1),
 {м-п:}  (x:3;y:2), (x:5;y:0), (x:6;y:1), (x:4;y:1));

const cCyrToPosMat2 : Array [$80..$8f] of TPoint = // $d1 prefix
 {р-т:} ((x:5;y:1), (x:2;y:2), (x:5;y:2),
 {у-ш:}  (x:2;y:0), (x:0;y:1), (x:10;y:0),(x:1;y:0), (x:1;y:2), (x:7;y:0),
 {щ-ю:}  (x:8;y:0), (x:11;y:0),(x:1;y:1),(x:6;y:2),(x:10;y:1),(x:8;y:2),
 {я:}    (x:0;y:2) );

function CharPos(C : PChar) : TPoint;
var U8Len : integer;
begin
  U8Len := UTF8CodepointSizeFast(C);
  if U8Len = 1 then
  begin
    if C^ in ['1'..'9'] then
       Result := Point(Ord(C^) - Ord('1'),10) else
    if C^='0' then
       Result := Point(9,10) else
    if C^ in ['a'..'z'] then
       Result := cLatToPosMat[C^]
    else
       Result := Point(-10, -10);
  end else
  if U8Len = 2 then
  begin
    if Ord(C^) = $d0 then
    begin
      C := C + 1;
      if Ord(C^) in [$b0..$bf] then
        Result := cCyrToPosMat1[Ord(C^)] else
        Result := Point(-10,-10);
    end else
    if Ord(C^) = $d1 then
    begin
      C := C + 1;
      if Ord(C^) in [$80..$8f] then
        Result := cCyrToPosMat2[Ord(C^)] else
        Result := Point(-10,-10);
    end else
      Result := Point(-10,-10);
  end else
     Result := Point(-10,-10);
end;

function charGaussDistance(C1, C2 : PChar) : Single;
var P1, P2 : TPoint;
begin
  P1 := CharPos(C1);
  P2 := CharPos(C2);
  Result := Math.max(abs(p1.x - p2.x), abs(p1.y - p2.y)) * 0.5;
end;

function U8Compare(C1, C2 : PChar) : Boolean;
var l1, l2 : Integer;
begin
  if Assigned(C1) and Assigned(C2) then
  begin
    l1 := UTF8CodepointSizeFast(C1);
    l2 := UTF8CodepointSizeFast(C2);
    if (l1 = l2) then
    begin
      Result := true;
      while l1 > 0 do
      begin
        if (C1^ <> C2^) then Exit(false);
        Inc(C1); Inc(C2); Dec(l1);
      end;
    end else Result := false;
  end else Result := false;
end;

var firstLength, secondLength, amax, i, afrom, ato, j,
    fLen : integer;
    value, cost : Single;
    lastSecondCh, secondCh, lastFirstCh, firstCh : PChar;
    currentRow, previousRow, transpositionRow, tempRow, data : PFloatArray;
    currentRowLen : Integer;
begin
    firstLength := UTF8Length(afirst);
    secondLength := UTF8Length(asecond);

    if (firstLength = 0) then Exit(1.0)
     else if (secondLength = 0) then Exit(1.0);

    if (firstLength > secondLength) then begin
       Exit(DLDist(asecond, afirst));
    end;

    fLen := Length(afirst);

    amax := secondLength;
    if (secondLength - firstLength > amax) then Exit(1.0);

    currentRowLen := (firstLength + 1) ;

    data := AllocMem(currentRowLen * 3 * sizeOf(single));
    currentRow := @(data^[0]);
    previousRow := @(data^[currentRowLen]);
    transpositionRow := @(data^[currentRowLen * 2]);

    for i := 0 to firstLength do
      previousRow^[i] := i;

    lastSecondCh := nil;
    secondCh := PChar(@(asecond[1]));
    for i := 1 to secondLength do begin
       currentRow^[0] := i;

       // Вычисляем только диагональную полосу шириной 2 * (max + 1)
       afrom := Math.max(i - amax - 1, 1);
       ato := Math.min(i + amax + 1, firstLength);

       lastFirstCh := nil;
       firstCh := UTF8CodepointStart(PChar(@(afirst[1])), flen, afrom-1);
       for j := afrom to ato do begin
         // Вычисляем минимальную цену перехода в текущее состояние из предыдущих среди удаления, вставки и
         // замены соответственно.
         if U8Compare(firstCh, secondCh) then cost := 0 else cost := min(charGaussDistance(firstCh, secondCh),1.0);

         value := Math.min(Math.min(currentRow^[j - 1] + 1.0, previousRow^[j] + 1.0), previousRow^[j - 1] + cost);

         // Если вдруг была транспозиция, надо также учесть и её стоимость.
         if (i > 1) and (j > 1) then
           if (U8Compare(firstCh, lastSecondCh) and
               U8Compare(lastFirstCh, secondCh)) then
       	      value := Math.min(value, transpositionRow^[j - 2] + 0.5);

         currentRow^[j] := value;
         lastFirstCh := firstCh;
         inc(firstCh, UTF8CodepointSize(firstCh));
       end;
       lastSecondCh := secondCh;
       inc(secondCh, UTF8CodepointSize(secondCh));

       tempRow := transpositionRow;
       transpositionRow := previousRow;
       previousRow := currentRow;
       currentRow := tempRow;
    end;
    Result := Math.min(previousRow^[firstLength] / (firstLength + secondLength) * 2.0, 1.0);
    FreeMem(data);
end;

class function TSinToken.CheckIsNumeric(const S: String): Word;
begin
  if Length(S) > 0 then
  begin
    if UTF8CodepointSizeFast(PChar(@(S[1]))) = 1 then
    begin
      if PChar(@(S[1]))^ in ['0'..'9'] then
        Result := 1 else
        Result := 2;
    end else Result := 1;
  end else Result := 0;
end;

class function TSinToken.CheckIsLat(const S: String): Word;
begin
  if Length(S) > 0 then
  begin
    if UTF8CodepointSizeFast(PChar(@(S[1]))) = 1 then
    begin
      if PChar(@(S[1]))^ in ['a'..'z'] then
        Result := 1 else
        Result := 2;
    end else Result := 1;
  end else Result := 0;
end;

end.

