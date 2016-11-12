// Direct Oracle Access - Compression unit
// Copyright 2003 Allround Automations
// support@allroundautomations.com
// http://www.allroundautomations.com

{$I Oracle.inc}

Unit OracleCompress;

{$A+,R-,S-,J+}

(*
 * LZHUF.C English version 1.0
 * Based on Japanese version 29-NOV-1988
 * LZSS coded by Haruhiko OKUMURA
 * Adaptive Huffman Coding coded by Haruyasu YOSHIZAKI
 * Edited and translated to English by Kenji RIKITAKE
 * Translated from C to Turbo Pascal by Douglas Webb   2/18/91
 * Converted to Delphi and added TStream interface by Allround Automations
*)

Interface

uses Classes;

procedure LZHStreamPack(InStream, OutStream: TStream);
procedure LZHStreamUnpack(InStream, OutStream: TStream);

Implementation

uses OracleCI;

Type
  PutBytesProc = Procedure(Var DTA; NBytes : Word; Var Bytes_Put : Word);
  GetBytesProc = Procedure(Var DTA; NBytes : Word; Var Bytes_Got : Word);

Const
  Exit_OK = 0;
  Exit_FAILED = 1;

  { LZSS Parameters }
  N = 4096;                            { Size of String buffer }
  F = 60;                              { Size of look-ahead buffer }
  THRESHOLD = 2;
  NUL = N;                             { end of tree's node  }

  { Huffman coding parameters }
  N_Char = (256 - THRESHOLD + F);

  { Character code (:= 0..N_Char-1) }
  T = (N_Char * 2 - 1);                { Size of table }
  R = (T - 1);                         { root position }

  { update when cumulative frequency }
  { reaches to this value }
  MAX_FREQ = $8000;

{
 * Tables For encoding/decoding upper 6 bits of
 * sliding dictionary Pointer
 }

  { encoder table }
  p_len : Array[0..63] of Byte =
  ($03, $04, $04, $04, $05, $05, $05, $05,
   $05, $05, $05, $05, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $08, $08, $08, $08, $08, $08, $08, $08);

  p_code : Array[0..63] of Byte =
  ($00, $20, $30, $40, $50, $58, $60, $68,
   $70, $78, $80, $88, $90, $94, $98, $9C,
   $A0, $A4, $A8, $AC, $B0, $B4, $B8, $BC,
   $C0, $C2, $C4, $C6, $C8, $CA, $CC, $CE,
   $D0, $D2, $D4, $D6, $D8, $DA, $DC, $DE,
   $E0, $E2, $E4, $E6, $E8, $EA, $EC, $EE,
   $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7,
   $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF);

  { decoder table }
  d_code : Array[0..255] of Byte =
  ($00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $00, $00, $00, $00, $00, $00, $00, $00,
   $01, $01, $01, $01, $01, $01, $01, $01,
   $01, $01, $01, $01, $01, $01, $01, $01,
   $02, $02, $02, $02, $02, $02, $02, $02,
   $02, $02, $02, $02, $02, $02, $02, $02,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $09, $09, $09, $09, $09, $09, $09, $09,
   $0A, $0A, $0A, $0A, $0A, $0A, $0A, $0A,
   $0B, $0B, $0B, $0B, $0B, $0B, $0B, $0B,
   $0C, $0C, $0C, $0C, $0D, $0D, $0D, $0D,
   $0E, $0E, $0E, $0E, $0F, $0F, $0F, $0F,
   $10, $10, $10, $10, $11, $11, $11, $11,
   $12, $12, $12, $12, $13, $13, $13, $13,
   $14, $14, $14, $14, $15, $15, $15, $15,
   $16, $16, $16, $16, $17, $17, $17, $17,
   $18, $18, $19, $19, $1A, $1A, $1B, $1B,
   $1C, $1C, $1D, $1D, $1E, $1E, $1F, $1F,
   $20, $20, $21, $21, $22, $22, $23, $23,
   $24, $24, $25, $25, $26, $26, $27, $27,
   $28, $28, $29, $29, $2A, $2A, $2B, $2B,
   $2C, $2C, $2D, $2D, $2E, $2E, $2F, $2F,
   $30, $31, $32, $33, $34, $35, $36, $37,
   $38, $39, $3A, $3B, $3C, $3D, $3E, $3F);

  d_len : Array[0..255] of Byte =
  ($03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $03, $03, $03, $03, $03, $03, $03, $03,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $04, $04, $04, $04, $04, $04, $04, $04,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $05, $05, $05, $05, $05, $05, $05, $05,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $06, $06, $06, $06, $06, $06, $06, $06,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $07, $07, $07, $07, $07, $07, $07, $07,
   $08, $08, $08, $08, $08, $08, $08, $08,
   $08, $08, $08, $08, $08, $08, $08, $08);

  getbuf : Word = 0;
  getlen : Byte = 0;
  putlen : Byte = 0;
  putbuf : Word = 0;
  TextSize : LongInt = 0;
  codesize : LongInt = 0;
  printcount : LongInt = 0;
  match_position : SmallInt = 0;
  match_length : SmallInt = 0;


Type
  FreqType = Array[0..T] of Word;
  FreqPtr = ^FreqType;
  PntrType = Array[0..pred(T + N_Char)] of SmallInt;
  pntrPtr = ^PntrType;
  SonType = Array[0..pred(T)] of SmallInt;
  SonPtr = ^SonType;
  TextBufType = Array[0..N + F - 2] of Byte;
  TBufPtr = ^TextBufType;
  WordRay = Array[0..N] of SmallInt;
  WordRayPtr = ^WordRay;
  BWordRay = Array[0..N + 256] of SmallInt;
  BWordRayPtr = ^BWordRay;

Var
  LZHSection: TOracleCriticalSection;
  Text_buf : TBufPtr;
  lson, dad : WordRayPtr;
  rson : BWordRayPtr;
  freq : FreqPtr;                      { cumulative freq table }

{
 * pointing parent nodes.
 * area [T..(T + N_Char - 1)] are Pointers For leaves
 }
  prnt : pntrPtr;

  { pointing children nodes (son[], son[] + 1)}
  son : SonPtr;

  GlobalInStream: TStream;
  GlobalOutStream: TStream;


  Procedure InitTree;                  { Initializing tree }
  Var
    i : SmallInt;
  begin
    For i := N + 1 to N + 256 do
      rson^[i] := NUL;                 { root }
    For i := 0 to N do
      dad^[i] := NUL;                  { node }
  end;


  Procedure InsertNode(R : SmallInt);   { Inserting node to the tree }
  Var
    tmp, i, p, cmp : SmallInt;
    key : TBufPtr;
    c : SmallInt;
  begin
    cmp := 1;
    key := @Text_buf^[R];
    p := succ(N) + key^[0];
    rson^[R] := NUL;
    lson^[R] := NUL;
    match_length := 0;
    While match_length < F do
      begin
        if (cmp >= 0) then
          begin
            if (rson^[p] <> NUL) then
              p := rson^[p]
            else
              begin
                rson^[p] := R;
                dad^[R] := p;
                Exit;
              end;
          end
        else
          begin
            if (lson^[p] <> NUL) then
              p := lson^[p]
            else
              begin
                lson^[p] := R;
                dad^[R] := p;
                Exit;
              end;
          end;
        i := 0;
        cmp := 0;
        While (i < F) and (cmp = 0) do
          begin
            inc(i);
            cmp := key^[i] - Text_buf^[p + i];
          end;
        if (i > THRESHOLD) then
          begin
            tmp := pred((R - p) and pred(N));
            if (i > match_length) then
              begin
                match_position := tmp;
                match_length := i;
              end;
            if (match_length < F) and (i = match_length) then
              begin
                c := tmp;
                if (c < match_position) then
                  match_position := c;
              end;
          end;
      end;                             { While True do }
    dad^[R] := dad^[p];
    lson^[R] := lson^[p];
    rson^[R] := rson^[p];
    dad^[lson^[p]] := R;
    dad^[rson^[p]] := R;
    if (rson^[dad^[p]] = p) then
      rson^[dad^[p]] := R
    else
      lson^[dad^[p]] := R;
    dad^[p] := NUL;                    { remove p }
  end;


  Procedure DeleteNode(p : SmallInt);   { Deleting node from the tree }
  Var
    q : SmallInt;
  begin
    if (dad^[p] = NUL) then
      Exit;                            { unregistered }
    if (rson^[p] = NUL) then
      q := lson^[p]
    else if (lson^[p] = NUL) then
      q := rson^[p]
    else
      begin
        q := lson^[p];
        if (rson^[q] <> NUL) then
          begin
            Repeat
              q := rson^[q];
            Until (rson^[q] = NUL);
            rson^[dad^[q]] := lson^[q];
            dad^[lson^[q]] := dad^[q];
            lson^[q] := lson^[p];
            dad^[lson^[p]] := q;
          end;
        rson^[q] := rson^[p];
        dad^[rson^[p]] := q;
      end;
    dad^[q] := dad^[p];
    if (rson^[dad^[p]] = p) then
      rson^[dad^[p]] := q
    else
      lson^[dad^[p]] := q;
    dad^[p] := NUL;
  end;

  { Huffman coding parameters }

  Function GetBit(GetBytes : GetBytesProc) : SmallInt; { get one bit }
  Var
    i : Byte;
    i2 : SmallInt;
    res : Word;
  begin
    While (getlen <= 8) do
      begin
        GetBytes(i, 1, res);
        if res = 1 then
          i2 := i
        else i2 := 0;
        getbuf := getbuf or (i2 shl (8 - getlen));
        inc(getlen, 8);
      end;
    i2 := getbuf;
    getbuf := getbuf shl 1;
    dec(getlen);
    GetBit := SmallInt((i2 < 0));
  end;


  Function GetByte(GetBytes : GetBytesProc) : SmallInt; { get a Byte }
  Var
    j : Byte;
    i, res : Word;
  begin
    While (getlen <= 8) do
      begin
        GetBytes(j, 1, res);
        if res = 1 then
          i := j
        else
          i := 0;
        getbuf := getbuf or (i shl (8 - getlen));
        inc(getlen, 8);
      end;
    i := getbuf;
    getbuf := getbuf shl 8;
    dec(getlen, 8);
    GetByte := SmallInt(i shr 8);
  end;


  Procedure Putcode(l : SmallInt; c : Word;
                    PutBytes : PutBytesProc); { output c bits }
  Var
    Temp : Byte;
    Got : Word;
  begin
    putbuf := putbuf or (c shr putlen);
    inc(putlen, l);
    if (putlen >= 8) then
      begin
        Temp := putbuf shr 8;
        PutBytes(Temp, 1, Got);
        dec(putlen, 8);
        if (putlen >= 8) then
          begin
            Temp := lo(putbuf);
            PutBytes(Temp, 1, Got);
            inc(codesize, 2);
            dec(putlen, 8);
            putbuf := c shl (l - putlen);
          end
        else
          begin
            putbuf := putbuf shl 8;
            inc(codesize);
          end;
      end;
  end;


  { initialize freq tree }

  Procedure StartHuff;
  Var
    i, j : SmallInt;
  begin
    For i := 0 to pred(N_Char) do
      begin
        freq^[i] := 1;
        son^[i] := i + T;
        prnt^[i + T] := i;
      end;
    i := 0;
    j := N_Char;
    While (j <= R) do
      begin
        freq^[j] := freq^[i] + freq^[i + 1];
        son^[j] := i;
        prnt^[i] := j;
        prnt^[i + 1] := j;
        inc(i, 2);
        inc(j);
      end;
    freq^[T] := $ffff;
    prnt^[R] := 0;
  end;


  { reConstruct freq tree }

  Procedure reConst;
  Var
    i, j, k, tmp : SmallInt;
    F, l : Word;
  begin
    { halven cumulative freq For leaf nodes }
    j := 0;
    For i := 0 to pred(T) do
      begin
        if (son^[i] >= T) then
          begin
            freq^[j] := succ(freq^[i]) div 2; {@@ Bug Fix MOD -> div @@}
            son^[j] := son^[i];
            inc(j);
          end;
      end;
    { make a tree : first, connect children nodes }
    i := 0;
    j := N_Char;
    While (j < T) do
      begin
        k := succ(i);
        F := freq^[i] + freq^[k];
        freq^[j] := F;
        k := pred(j);
        While F < freq^[k] do
          dec(k);
        inc(k);
        l := (j - k) shl 1;
        tmp := succ(k);
        move(freq^[k], freq^[tmp], l);
        freq^[k] := F;
        move(son^[k], son^[tmp], l);
        son^[k] := i;
        inc(i, 2);
        inc(j);
      end;
    { connect parent nodes }
    For i := 0 to pred(T) do
      begin
        k := son^[i];
        if (k >= T) then
          begin
            prnt^[k] := i;
          end
        else
          begin
            prnt^[k] := i;
            prnt^[succ(k)] := i;
          end;
      end;
  end;


  { update freq tree }

  Procedure update(c : SmallInt);
  Var
    i, j, k, l : SmallInt;
  begin
    if (freq^[R] = MAX_FREQ) then
      begin
        reConst;
      end;
    c := prnt^[c + T];
    Repeat
      inc(freq^[c]);
      k := freq^[c];
      { swap nodes to keep the tree freq-ordered }
      l := succ(c);
      if (Integer(k) > freq^[l]) then
        begin
          While (Integer(k) > freq^[l]) do inc(l);
          dec(l);
          freq^[c] := freq^[l];
          freq^[l] := k;
          i := son^[c];
          prnt^[i] := l;
          if (i < T) then prnt^[succ(i)] := l;
          j := son^[l];
          son^[l] := i;
          prnt^[j] := c;
          if (j < T) then prnt^[succ(j)] := c;
          son^[c] := j;
          c := l;
        end;
      c := prnt^[c];
    Until (c = 0);                     { Repeat it Until reaching the root }
  end;


Var
  code, len : Word;

  Procedure EncodeChar(c : Word; PutBytes : PutBytesProc);
  Var
    i : Word;
    j, k : SmallInt;
  begin
    i := 0;
    j := 0;
    k := prnt^[c + T];
    { search connections from leaf node to the root }
    Repeat
      i := i shr 1;
 {
        if node's address is odd, output 1
        else output 0
        }
      if Boolean(k and 1) then inc(i, $8000);
      inc(j);
      k := prnt^[k];
    Until (k = R);
    Putcode(j, i, PutBytes);
    code := i;
    len := j;
    update(c);
  end;


  Procedure EncodePosition(c : Word; PutBytes : PutBytesProc);
  Var
    i, j : Word;
  begin
    { output upper 6 bits With encoding }
    i := c shr 6;
    j := p_code[i];
    Putcode(p_len[i], j shl 8, PutBytes);
    { output lower 6 bits directly }
    Putcode(6, (c and $3f) shl 10, PutBytes);
  end;


  Procedure Encodeend(PutBytes : PutBytesProc);
  Var
    Temp : Byte;
    Got : Word;
  begin
    if Boolean(putlen) then
      begin
        Temp := lo(putbuf shr 8);
        PutBytes(Temp, 1, Got);
        inc(codesize);
      end;
  end;


  Function DecodeChar(GetBytes : GetBytesProc) : SmallInt;
  Var
    c : Word;
  begin
    c := son^[R];
    {
     * start searching tree from the root to leaves.
     * choose node #(son[]) if input bit = 0
     * else choose #(son[]+1) (input bit = 1)
    }
    While (c < T) do
      begin
        c := c + GetBit(GetBytes);
        c := son^[c];
      end;
    c := c - T;
    update(c);
    DecodeChar := SmallInt(c);
  end;


  Function DecodePosition(GetBytes : GetBytesProc) : Word;
  Var
    i, j, c : Word;
  begin
    { decode upper 6 bits from given table }
    i := GetByte(GetBytes);
    c := Word(d_code[i] shl 6);
    j := d_len[i];
    { input lower 6 bits directly }
    dec(j, 2);
    While j <> 0 do
      begin
        i := (i shl 1) + GetBit(GetBytes);
        dec(j);
      end;
    DecodePosition := c or i and $3f;
  end;


  { Compression }

  Procedure InitLZH;
  begin
    getbuf := 0;
    getlen := 0;
    putlen := 0;
    putbuf := 0;
    TextSize := 0;
    codesize := 0;
    printcount := 0;
    match_position := 0;
    match_length := 0;
    new(lson);
    new(dad);
    new(rson);
    new(Text_buf);
    new(freq);
    new(prnt);
    new(son);
  end;


  Procedure endLZH;
  begin
    dispose(son);
    dispose(prnt);
    dispose(freq);
    dispose(Text_buf);
    dispose(rson);
    dispose(dad);
    dispose(lson);
  end;


  Procedure LZHPack(Var Bytes_Written : LongInt;
                        GetBytes : GetBytesProc;
                        PutBytes : PutBytesProc);
  Var
    ct : Byte;
    i, len, R, s, last_match_length : SmallInt;
    Got : Word;
  begin
    InitLZH;
    TextSize := 0;                     { rewind and rescan }
    StartHuff;
    InitTree;
    s := 0;
    R := N - F;
    fillChar(Text_buf^[0], R, ' ');
    len := 0;
    Got := 1;
    While (len < F) and (Got <> 0) do
      begin
        GetBytes(ct, 1, Got);
        if Got <> 0 then
          begin
            Text_buf^[R + len] := ct;
            inc(len);
          end;
      end;
    TextSize := len;
    For i := 1 to F do
      InsertNode(R - i);
    InsertNode(R);
    Repeat
      if (match_length > len) then
        match_length := len;
      if (match_length <= THRESHOLD) then
        begin
          match_length := 1;
          EncodeChar(Text_buf^[R], PutBytes);
        end
      else
        begin
          EncodeChar(255 - THRESHOLD + match_length, PutBytes);
          EncodePosition(match_position, PutBytes);
        end;
      last_match_length := match_length;
      i := 0;
      Got := 1;
      While (i < last_match_length) and (Got <> 0) do
        begin
          GetBytes(ct, 1, Got);
          if Got <> 0 then
            begin
              DeleteNode(s);
              Text_buf^[s] := ct;
              if (s < pred(F)) then
                Text_buf^[s + N] := ct;
              s := succ(s) and pred(N);
              R := succ(R) and pred(N);
              InsertNode(R);
              inc(i);
            end;
        end;
      inc(TextSize, i);
      While (i < last_match_length) do
        begin
          inc(i);
          DeleteNode(s);
          s := succ(s) and pred(N);
          R := succ(R) and pred(N);
          dec(len);
          if Boolean(len) then InsertNode(R);
        end;
    Until (len <= 0);
    Encodeend(PutBytes);
    endLZH;
    Bytes_Written := TextSize;
  end;


  Procedure LZHUnpack(TextSize : LongInt;
                      GetBytes : GetBytesProc;
                      PutBytes : PutBytesProc);
  Var
    c, i, j, k, R : SmallInt;
    c2 : Byte;
    count : LongInt;
    Put : Word;
  begin
    InitLZH;
    StartHuff;
    R := N - F;
    fillChar(Text_buf^[0], R, ' ');
    count := 0;
    While count < TextSize do
      begin
        c := DecodeChar(GetBytes);
        if (c < 256) then
          begin
            c2 := lo(c);
            PutBytes(c2, 1, Put);
            Text_buf^[R] := c;
            inc(R);
            R := R and pred(N);
            inc(count);
          end
        else
          begin
            i := (R - succ(DecodePosition(GetBytes))) and pred(N);
            j := c - 255 + THRESHOLD;
            For k := 0 to pred(j) do
              begin
                c := Text_buf^[(i + k) and pred(N)];
                c2 := lo(c);
                PutBytes(c2, 1, Put);
                Text_buf^[R] := c;
                inc(R);
                R := R and pred(N);
                inc(count);
              end;
          end;
      end;
    endLZH;
  end;

// Stream interface
procedure LZHStreamPutBytes(Var DTA; NBytes : Word; Var Bytes_Put : Word);
begin
  Bytes_Put := GlobalOutStream.Write(DTA, NBytes);
end;

procedure LZHStreamGetBytes(Var DTA; NBytes : Word; Var Bytes_Got : Word);
begin
  Bytes_Got := GlobalInStream.Read(DTA, NBytes);
end;

procedure LZHStreamPack(InStream, OutStream: TStream);
var s, BytesWritten, StartPos: LongInt;
begin
  LZHSection.Enter;
  try
    GlobalInStream := InStream;
    GlobalOutStream := OutStream;
    s := InStream.Size - InStream.Position;
    StartPos := OutStream.Position;
    OutStream.Size := StartPos + SizeOf(s) + s;
    OutStream.Seek(StartPos, soFromBeginning);
    OutStream.Write(s, SizeOf(s));
    LZHPack(BytesWritten, LZHStreamGetBytes, LZHStreamPutBytes);
    OutStream.Size := OutStream.Position;
  finally
    LZHSection.Leave;
  end;
end;

procedure LZHStreamUnpack(InStream, OutStream: TStream);
var s: LongInt;
begin
  LZHSection.Enter;
  try
    GlobalInStream := InStream;
    GlobalOutStream := OutStream;
    InStream.Read(s, SizeOf(s));
    OutStream.Size := OutStream.Position + s;
    LZHUnpack(s, LZHStreamGetBytes, LZHStreamPutBytes);
  finally
    LZHSection.Leave;
  end;
end;

initialization
  LZHSection := TOracleCriticalSection.Create;

finalization
  LZHSection.Free;

end.
