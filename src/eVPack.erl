-module(eVPack).
-include("eVPack.hrl").

-compile(inline).
-compile({inline_size, 128}).
-import(lists, [reverse/1]).
-import(maps, [iterator/1, next/1, keys/1]).

-export([
   encode/1
   , encodeBin/1
   , encode/3
   , encodeBin/3
   , decode/1
   , encodeAtom/1
   , encodeMap/3
   , encodeList/3
   , encodeFloat/1
   , encodeString/1
   , encodeInteger/1
   , buildIndexTable_1/2
   , buildIndexTable_2/2
   , buildIndexTable_4/2
   , buildIndexTable_8/2
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% encode %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode(term()) -> {ok, vpack()} | {error, any()}.
encode(Term) ->
   %%{VPack, _Size} = encoder(Term, ?VpDefArrOpt, ?VpDefObjOpt),
   %%VPack.
   try encoder(Term, ?VpArrDef, ?VpObjDef) of
      {VPack, _Size} ->
         VPack
   catch
      C:E:S ->
         {C, E, S}
   end.

-spec encodeBin(term()) -> {ok, vpack()} | {error, any()}.
encodeBin(Term) ->
   %%{VPack, _Size} = encoder(Term, ?VpDefArrOpt, ?VpDefObjOpt),
   %%iolist_to_binary(VPack).
   try encoder(Term, ?VpArrDef, ?VpObjDef) of
      {VPack, _Size} ->
         iolist_to_binary(VPack)
   catch
      C:E:S ->
         {C, E, S}
   end.

-spec encode(term(), vpOpt(), vpOpt()) -> {ok, vpack()} | {error, any()}.
encode(Term, ArrOpt, ObjOpt) ->
   % {VPack, _Size} = encoder(Term, ArrOpt, ObjOpt),
   % VPack.
   try encoder(Term, ArrOpt, ObjOpt) of
      {VPack, _Size} ->
         VPack
   catch
      C:E:S ->
         {C, E, S}
   end.

-spec encodeBin(term(), vpOpt(), vpOpt()) -> {ok, vpack()} | {error, any()}.
encodeBin(Term, ArrOpt, ObjOpt) ->
   % {VPack, _Size} = encoder(Term, ArrOpt, ObjOpt),
   % iolist_to_binary(VPack).
   try encoder(Term, ArrOpt, ObjOpt) of
      {VPack, _Size} ->
         iolist_to_binary(VPack)
   catch
      C:E:S ->
         {C, E, S}
   end.

encoder(Map, ArrOpt, ObjOpt) when erlang:is_map(Map) ->
   encodeMap(ObjOpt, Map, ArrOpt);
encoder(Atom, _, _) when erlang:is_atom(Atom) ->
   encodeAtom(Atom);
encoder(Binary, _, _) when erlang:is_binary(Binary) ->
   encodeString(Binary);
encoder(Integer, _, _) when erlang:is_integer(Integer) ->
   encodeInteger(Integer);
encoder(Float, _, _) when erlang:is_float(Float) ->
   encodeFloat(Float);
encoder(List, ArrOpt, ObjOpt) when erlang:is_list(List) ->
   encodeList(ArrOpt, List, ObjOpt);
encoder(_Value, _, _) ->
   {error, dataType(_Value)}.


dataType(Data) when is_list(Data) -> list;
dataType(Data) when is_integer(Data) -> integer;
dataType(Data) when is_binary(Data) -> binary;
dataType(Data) when is_function(Data) -> function;
dataType(Data) when is_tuple(Data) -> tuple;
dataType(Data) when is_atom(Data) -> atom;
dataType(Data) when is_bitstring(Data) -> bitstring;
dataType(Data) when is_boolean(Data) -> boolean;
dataType(Data) when is_float(Data) -> float;
dataType(Data) when is_number(Data) -> number;
dataType(Data) when is_pid(Data) -> pid;
dataType(Data) when is_port(Data) -> port;
dataType(_Data) -> not_know.

encodeAtom(undefined) -> {<<24/integer>>, 1};
encodeAtom(false) -> {<<25/integer>>, 1};
encodeAtom(true) -> {<<26/integer>>, 1};
encodeAtom(minKey) -> {<<30/integer>>, 1};
encodeAtom(maxKey) -> {<<31/integer>>, 1};
encodeAtom(illegal) -> {<<23/integer>>, 1};
encodeAtom(null) -> {<<24/integer>>, 1};
encodeAtom(nil) -> {<<24/integer>>, 1};
encodeAtom(Atom) ->
   encodeString(erlang:atom_to_binary(Atom, utf8)).

encodeInteger(0) ->
   {<<48/integer>>, 1};
encodeInteger(1) ->
   {<<49/integer>>, 1};
encodeInteger(2) ->
   {<<50/integer>>, 1};
encodeInteger(3) ->
   {<<51/integer>>, 1};
encodeInteger(4) ->
   {<<52/integer>>, 1};
encodeInteger(5) ->
   {<<53/integer>>, 1};
encodeInteger(6) ->
   {<<54/integer>>, 1};
encodeInteger(7) ->
   {<<55/integer>>, 1};
encodeInteger(8) ->
   {<<56/integer>>, 1};
encodeInteger(9) ->
   {<<57/integer>>, 1};
encodeInteger(-6) ->
   {<<58/integer>>, 1};
encodeInteger(-5) ->
   {<<59/integer>>, 1};
encodeInteger(-4) ->
   {<<60/integer>>, 1};
encodeInteger(-3) ->
   {<<61/integer>>, 1};
encodeInteger(-2) ->
   {<<62/integer>>, 1};
encodeInteger(-1) ->
   {<<63/integer>>, 1};
encodeInteger(Integer) ->
   if
      Integer < -9223372036854775808 ->
         erlang:throw(<<"Cannot encode integers less than -9223372036854775808">>);
      Integer < -36028797018963968 ->
         {<<39/integer, Integer:64/integer-little-signed>>, 9};
      Integer < -140737488355328 ->
         {<<38/integer, Integer:56/integer-little-signed>>, 8};
      Integer < -549755813888 ->
         {<<37/integer, Integer:48/integer-little-signed>>, 7};
      Integer < -2147483648 ->
         {<<36/integer, Integer:40/integer-little-signed>>, 6};
      Integer < -8388608 ->
         {<<35/integer, Integer:32/integer-little-signed>>, 5};
      Integer < -32768 ->
         {<<34/integer, Integer:24/integer-little-signed>>, 4};
      Integer < -128 ->
         {<<33/integer, Integer:16/integer-little-signed>>, 3};
      Integer < 0 ->
         {<<32/integer, Integer:8/integer-little-unsigned>>, 2};
      Integer < 256 ->
         {<<40/integer, Integer:8/integer-little-unsigned>>, 2};
      Integer < 65536 ->
         {<<41/integer, Integer:16/integer-little-unsigned>>, 3};
      Integer < 16777216 ->
         {<<42/integer, Integer:24/integer-little-unsigned>>, 4};
      Integer < 4294967296 ->
         {<<43/integer, Integer:32/integer-little-unsigned>>, 5};
      Integer < 1099511627776 ->
         {<<44/integer, Integer:40/integer-little-unsigned>>, 6};
      Integer < 281474976710656 ->
         {<<45/integer, Integer:48/integer-little-unsigned>>, 7};
      Integer < 72057594037927936 ->
         {<<46/integer, Integer:56/integer-little-unsigned>>, 8};
      Integer < 18446744073709551616 ->
         {<<47/integer, Integer:64/integer-little-unsigned>>, 9};
      true ->
         erlang:throw(<<"Cannot encode integers greater than 18446744073709551616">>)
   end.

encodeFloat(Float) ->
   {<<27/integer, Float:64/float-little>>, 9}.

encodeString(BinStr) ->
   StrSize = erlang:byte_size(BinStr),
   if
      StrSize =< 126 ->
         {<<(StrSize + 64)/integer, BinStr/binary>>, StrSize + 1};
      StrSize < 256 ->
         {<<192/integer, StrSize:8/integer-little-unsigned, BinStr/binary>>, StrSize + 2};
      StrSize < 65536 ->
         {<<193/integer, StrSize:16/integer-little-unsigned, BinStr/binary>>, StrSize + 3};
      StrSize < 16777216 ->
         {<<194/integer, StrSize:24/integer-little-unsigned, BinStr/binary>>, StrSize + 4};
      StrSize < 4294967296 ->
         {<<195/integer, StrSize:32/integer-little-unsigned, BinStr/binary>>, StrSize + 5};
      StrSize < 1099511627776 ->
         {<<196/integer, StrSize:40/integer-little-unsigned, BinStr/binary>>, StrSize + 6};
      StrSize < 281474976710656 ->
         {<<197/integer, StrSize:48/integer-little-unsigned, BinStr/binary>>, StrSize + 7};
      StrSize < 72057594037927936 ->
         {<<198/integer, StrSize:56/integer-little-unsigned, BinStr/binary>>, StrSize + 8};
      StrSize < 18446744073709551616 ->
         {<<199/integer, StrSize:64/integer-little-unsigned, BinStr/binary>>, StrSize + 9};
      true ->
         {<<191/integer, StrSize:64/integer-little-unsigned, BinStr/binary>>, StrSize + 9}
   end.

doEncodeMap(Iterator, ArrOpt, ObjOpt, AccList, SumSize) ->
   case maps:next(Iterator) of
      {Key, Value, NextIter} ->
         {KeyEn, KeySize} = encodeString(asKey(Key)),
         {ValueEn, ValueSize} = encoder(Value, ArrOpt, ObjOpt),
         doEncodeMap(NextIter, ArrOpt, ObjOpt, [ValueEn, KeyEn | AccList], SumSize + KeySize + ValueSize);
      none ->
         {AccList, SumSize}
   end.

doEncodeMap(Iterator, ArrOpt, ObjOpt, AccList, Offsets, SumSize) ->
   case maps:next(Iterator) of
      {Key, Value, NextIter} ->
         {KeyEn, KeySize} = encodeString(asKey(Key)),
         {ValueEn, ValueSize} = encoder(Value, ArrOpt, ObjOpt),
         doEncodeMap(NextIter, ArrOpt, ObjOpt, [ValueEn, KeyEn | AccList], [SumSize | Offsets], SumSize + KeySize + ValueSize);
      none ->
         {AccList, Offsets, SumSize}
   end.

doEncodeMap([], _Map, _ArrOpt, _ObjOpt, AccList, Offsets, SumSize) ->
   {AccList, Offsets, SumSize};
doEncodeMap([OneKeys | Left], Map, ArrOpt, ObjOpt, AccList, Offsets, SumSize) ->
   case Map of
      #{OneKeys := Value} ->
         {KeyEn, KeySize} = encodeString(OneKeys),
         {ValueEn, ValueSize} = encoder(Value, ArrOpt, ObjOpt),
         doEncodeMap(Left, Map, ArrOpt, ObjOpt, [ValueEn, KeyEn | AccList], [SumSize | Offsets], SumSize + KeySize + ValueSize);
      _ ->
         AtomKey = binary_to_atom(OneKeys, utf8),
         case Map of
            #{AtomKey := Value} ->
               {KeyEn, KeySize} = encodeString(OneKeys),
               {ValueEn, ValueSize} = encoder(Value, ArrOpt, ObjOpt),
               doEncodeMap(Left, Map, ArrOpt, ObjOpt, [ValueEn, KeyEn | AccList], [SumSize | Offsets], SumSize + KeySize + ValueSize);
            _ ->
               erlang:throw(<<"doEncodeMap not found the value ", OneKeys/binary>>)
         end
   end.

encodeMap(?VpObjNcNs, Map, ArrOpt) ->
   MapSize = erlang:map_size(Map),
   case MapSize == 0 of
      true ->
         {<<10/integer>>, 1};
      _ ->
         {AccList, Offsets, SumSize} = doEncodeMap(maps:iterator(Map), ArrOpt, ?VpObjNcNs, [], [], 0),
         IoData = lists:reverse(AccList),
         case MapSize >= 1000 of
            false ->
               encodeUnSortMapIndexTable(IoData, MapSize, Offsets, SumSize);
            _ ->
               encodeUnSortMapIndexTable(erlang:iolist_to_binary(IoData), MapSize, Offsets, SumSize)
         end
   end;
encodeMap(?VpObjYc, Map, ArrOpt) ->
   MapSize = erlang:map_size(Map),
   case MapSize == 0 of
      true ->
         {<<10/integer>>, 1};
      _ ->
         {AccList, SumSize} = doEncodeMap(maps:iterator(Map), ArrOpt, ?VpObjYc, [], 0),
         IoData = lists:reverse(AccList),
         case MapSize >= 1000 of
            false ->
               encodeCompactData(<<20/integer>>, IoData, SumSize, MapSize);
            _ ->
               encodeCompactData(<<20/integer>>, erlang:iolist_to_binary(IoData), SumSize, MapSize)
         end
   end;
encodeMap(?VpObjNcYs, Map, ArrOpt) ->
   MapSize = erlang:map_size(Map),
   case MapSize == 0 of
      true ->
         {<<10/integer>>, 1};
      _ ->
         Keys = maps:keys(Map),
         StrKeys = [asKey(OneKey) || OneKey <- Keys],
         {AccList, Offsets, SumSize} = doEncodeMap(lists:sort(StrKeys), Map, ArrOpt, ?VpObjNcYs, [], [], 0),
         IoData = lists:reverse(AccList),
         case MapSize >= 1000 of
            false ->
               encodeSortMapIndexTable(IoData, MapSize, Offsets, SumSize);
            _ ->
               encodeSortMapIndexTable(erlang:iolist_to_binary(IoData), MapSize, Offsets, SumSize)
         end
   end.

encodeSortMapIndexTable(IoData, Count, Offsets, SumSize) ->
   TemSize = SumSize + Count,
   if
      TemSize < 253 ->
         AllSize = TemSize + 3,
         Header = <<11/integer, AllSize:8/integer-unsigned, Count:8/integer-unsigned>>,
         {[Header, IoData, buildIndexTable_1(Offsets, 3)], AllSize};
      TemSize + Count < 65531 ->
         AllSize = TemSize + Count + 5,
         Header = <<12/integer, AllSize:16/integer-little-unsigned, Count:16/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_2(Offsets, 5)], AllSize};
      TemSize + Count * 3 < 4294967287 ->
         AllSize = TemSize + Count * 3 + 9,
         Header = <<13/integer, AllSize:32/integer-little-unsigned, Count:32/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_4(Offsets, 9)], AllSize};
      TemSize + Count * 7 < 18446744073709551599 ->
         AllSize = TemSize + Count * 7 + 17,
         Header = <<14/integer, AllSize:64/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_8(Offsets, 17), <<Count:64/integer-little-unsigned>>], AllSize};
      true ->
         erlang:throw(<<"too much size">>)
   end.

encodeUnSortMapIndexTable(IoData, Count, Offsets, SumSize) ->
   TemSize = SumSize + Count,
   if
      TemSize < 253 ->
         AllSize = TemSize + 3,
         Header = <<15/integer, AllSize:8/integer-unsigned, Count:8/integer-unsigned>>,
         {[Header, IoData, buildIndexTable_1(Offsets, 3)], AllSize};
      TemSize + Count < 65531 ->
         AllSize = TemSize + Count + 5,
         Header = <<16/integer, AllSize:16/integer-little-unsigned, Count:16/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_2(Offsets, 5)], AllSize};
      TemSize + Count * 3 < 4294967287 ->
         AllSize = TemSize + Count * 3 + 9,
         Header = <<17/integer, AllSize:32/integer-little-unsigned, Count:32/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_4(Offsets, 9)], AllSize};
      TemSize + Count * 7 < 18446744073709551599 ->
         AllSize = TemSize + Count * 7 + 17,
         Header = <<18/integer, AllSize:64/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_8(Offsets, 17), <<Count:64/integer-little-unsigned>>], AllSize};
      true ->
         erlang:throw(<<"encode map too much size">>)
   end.

buildIndexTable_1(Offsets, StartSize) ->
   <<<<(OneOff + StartSize):1/integer-little-unsigned-unit:8>> || OneOff <- lists:reverse(Offsets)>>.

buildIndexTable_2(Offsets, StartSize) ->
   <<<<(OneOff + StartSize):2/integer-little-unsigned-unit:8>> || OneOff <- lists:reverse(Offsets)>>.

buildIndexTable_4(Offsets, StartSize) ->
   <<<<(OneOff + StartSize):4/integer-little-unsigned-unit:8>> || OneOff <- lists:reverse(Offsets)>>.

buildIndexTable_8(Offsets, StartSize) ->
   <<<<(OneOff + StartSize):8/integer-little-unsigned-unit:8>> || OneOff <- lists:reverse(Offsets)>>.

compactIntegerList(Integer, AccList) ->
   case Integer < 128 of
      true ->
         [<<Integer:8/integer-unsigned>> | AccList];
      _ ->
         TemInteger = Integer band 127 bor 128,
         NewInteger = Integer bsr 7,
         compactIntegerList(NewInteger, [<<TemInteger:8/integer-unsigned>> | AccList])
   end.

compactInteger(Value, Reverse) ->
   CompactList = compactIntegerList(Value, []),
   case Reverse of
      false -> lists:reverse(CompactList);
      _ -> CompactList
   end.

compactSize(AllSize) ->
   TemByte = erlang:ceil(AllSize / 128),
   FinalSize = AllSize + TemByte,
   LastSize =
      case TemByte == erlang:ceil(FinalSize / 128) of
         false -> FinalSize + 1;
         true -> FinalSize
      end,
   {compactInteger(LastSize, false), LastSize}.

encodeCompactData(Type, IoData, SumSize, Count) ->
   CompactList = compactInteger(Count, true),
   AllSize = SumSize + 1 + erlang:length(CompactList),
   {TotalSize, FinalSize} = compactSize(AllSize),
   {[Type, TotalSize, IoData | CompactList], FinalSize}.


asKey(Value) when erlang:is_atom(Value) -> erlang:atom_to_binary(Value, utf8);
asKey(Value) when erlang:is_binary(Value) -> Value;
asKey(_Value) -> erlang:error(<<"Invalid key">>).

doEncodeList([], _ArrOpt, _ObjOpt, AccList, SumSize, Count) ->
   {AccList, SumSize, Count};
doEncodeList([One | Left], ArrOpt, ObjOpt, AccList, SumSize, Count) ->
   {ValueEn, ValueSize} = encoder(One, ArrOpt, ObjOpt),
   doEncodeList(Left, ArrOpt, ObjOpt, [ValueEn | AccList], SumSize + ValueSize, Count + 1).

doEncodeList([], _ArrOpt, _ObjOpt, AccList, Offsets, SumSize, Count, SizeOrIsNot) ->
   {AccList, Offsets, SumSize, Count, SizeOrIsNot};
doEncodeList([One | Left], ArrOpt, ObjOpt, AccList, Offsets, SumSize, Count, SizeOrIsNot) ->
   {ValueEn, ValueSize} = encoder(One, ArrOpt, ObjOpt),
   case SizeOrIsNot of
      true ->
         doEncodeList(Left, ArrOpt, ObjOpt, [ValueEn | AccList], [SumSize | Offsets], ValueSize + SumSize, Count + 1, SizeOrIsNot);
      init ->
         doEncodeList(Left, ArrOpt, ObjOpt, [ValueEn | AccList], [SumSize | Offsets], ValueSize + SumSize, Count + 1, ValueSize);
      _ ->
         doEncodeList(Left, ArrOpt, ObjOpt, [ValueEn | AccList], [SumSize | Offsets], ValueSize + SumSize, Count + 1, ValueSize =/= SizeOrIsNot orelse SizeOrIsNot)
   end.

encodeList(?VpArrNc, List, ObjOpt) ->
   case List of
      [] ->
         {<<1/integer>>, 1};
      _ ->
         {AccList, Offsets, SumSize, Count, IsNotSameSize} = doEncodeList(List, ?VpArrNc, ObjOpt, [], [], 0, 0, init),

         IoData = lists:reverse(AccList),
         case Count >= 1000 of
            false ->
               case IsNotSameSize of
                  true ->
                     encodeListWithIndexTable(IoData, Count, Offsets, SumSize);
                  _ ->
                     encodeListWithoutIndexTable(IoData, SumSize)
               end;
            _ ->
               case IsNotSameSize of
                  true ->
                     encodeListWithIndexTable(erlang:iolist_to_binary(IoData), Count, Offsets, SumSize);
                  _ ->
                     encodeListWithoutIndexTable(erlang:iolist_to_binary(IoData), SumSize)
               end
         end
   end;
encodeList(?VpArrYc, List, ObjOpt) ->
   case List of
      [] ->
         {<<1/integer>>, 1};
      _ ->
         {AccList, SumSize, Count} = doEncodeList(List, ?VpArrYc, ObjOpt, [], 0, 0),

         IoData = lists:reverse(AccList),
         case Count >= 1000 of
            false ->
               encodeCompactData(<<19/integer>>, IoData, SumSize, Count);
            _ ->
               encodeCompactData(<<19/integer>>, erlang:iolist_to_binary(IoData), SumSize, Count)
         end
   end.

encodeListWithoutIndexTable(IoData, SumSize) ->
   if
      SumSize < 254 ->
         AllSize = SumSize + 2,
         Header = <<2/integer, AllSize:1/integer-little-unsigned-unit:8>>,
         {[Header, IoData], AllSize};
      SumSize < 65533 ->
         AllSize = SumSize + 3,
         Header = <<3/integer, AllSize:2/integer-little-unsigned-unit:8>>,
         {[Header, IoData], AllSize};
      SumSize < 4294967291 ->
         AllSize = SumSize + 5,
         Header = <<4/integer, AllSize:4/integer-little-unsigned-unit:8>>,
         {[Header, IoData], AllSize};
      SumSize < 18446744073709551607 ->
         AllSize = SumSize + 9,
         Header = <<5/integer, AllSize:8/integer-little-unsigned-unit:8>>,
         {[Header, IoData], AllSize};
      true ->
         erlang:throw(<<"encode map too much size">>)
   end.

encodeListWithIndexTable(IoData, Count, Offsets, SumSize) ->
   TemSize = SumSize + Count,
   if
      TemSize < 253 ->
         AllSize = TemSize + 3,
         Header = <<6/integer, AllSize:8/integer-unsigned, Count:8/integer-unsigned>>,
         {[Header, IoData, buildIndexTable_1(Offsets, 3)], AllSize};
      TemSize + Count < 65531 ->
         AllSize = TemSize + Count + 5,
         Header = <<7/integer, AllSize:16/integer-little-unsigned, Count:16/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_2(Offsets, 5)], AllSize};
      TemSize + Count * 3 < 4294967287 ->
         AllSize = TemSize + Count * 3 + 9,
         Header = <<8/integer, AllSize:32/integer-little-unsigned, Count:32/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_4(Offsets, 9)], AllSize};
      TemSize + Count * 7 < 18446744073709551599 ->
         AllSize = TemSize + Count * 7 + 17,
         Header = <<9/integer, AllSize:64/integer-little-unsigned>>,
         {[Header, IoData, buildIndexTable_8(Offsets, 17), <<Count:64/integer-little-unsigned>>], AllSize};
      true ->
         erlang:throw(<<"encode map too much size">>)
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  decode  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec decode(vpack()) -> term().
decode(DataBin) ->
   % {Term, _} = decoder(DataBin),
   % Term.
   try decoder(DataBin) of
      {Term, _} ->
         Term
   catch
      C:E:S ->
         {C, E, S}
   end.

decoder(DataBin) ->
   case DataBin of
      <<Type/integer, RestBin/bitstring>> ->
         decoder(Type, RestBin);
      _ ->
         erlang:throw(unexpected_end)
   end.

decoder(0, RestBin) ->
   erlang:throw({unsupported_type, RestBin});
decoder(1, RestBin) ->
   {[], RestBin};
decoder(2, RestBin) ->
   parseArrayWithoutIndexTable(2, RestBin);
decoder(3, RestBin) ->
   parseArrayWithoutIndexTable(3, RestBin);
decoder(4, RestBin) ->
   parseArrayWithoutIndexTable(4, RestBin);
decoder(5, RestBin) ->
   parseArrayWithoutIndexTable(5, RestBin);
decoder(6, RestBin) ->
   parseArrayWithIndexTable(6, RestBin);
decoder(7, RestBin) ->
   parseArrayWithIndexTable(7, RestBin);
decoder(8, RestBin) ->
   parseArrayWithIndexTable(8, RestBin);
decoder(9, RestBin) ->
   parseArrayWithIndexTable(9, RestBin);
decoder(10, RestBin) ->
   {#{}, RestBin};
decoder(11, RestBin) ->
   parseObject(11, RestBin);
decoder(12, RestBin) ->
   parseObject(12, RestBin);
decoder(13, RestBin) ->
   parseObject(13, RestBin);
decoder(14, RestBin) ->
   parseObject(14, RestBin);
decoder(15, RestBin) ->
   parseObject(15, RestBin);
decoder(16, RestBin) ->
   parseObject(16, RestBin);
decoder(17, RestBin) ->
   parseObject(17, RestBin);
decoder(18, RestBin) ->
   parseObject(18, RestBin);
decoder(19, RestBin) ->
   parseCompactArray(RestBin);
decoder(20, RestBin) ->
   parseCompactObject(RestBin);
decoder(21, _RestBin) ->
   erlang:throw({unsupported_type, 21});
decoder(22, _RestBin) ->
   erlang:throw({unsupported_type, 22});
decoder(23, RestBin) ->
   {illegal, RestBin};
decoder(24, RestBin) -> {undefined, RestBin};
decoder(25, RestBin) -> {false, RestBin};
decoder(26, RestBin) -> {true, RestBin};
decoder(27, RestBin) ->
   <<Float:64/float-little, LeftBin/bitstring>> = RestBin,
   {Float, LeftBin};
decoder(28, RestBin) ->
   <<TimeMs:64/integer-little-unsigned, LeftBin/bitstring>> = RestBin,
   {TimeMs, LeftBin};
decoder(29, _RestBin) ->
   erlang:throw({unsupported_type, 29});
decoder(30, RestBin) ->
   {min_key, RestBin};
decoder(31, RestBin) ->
   {max_key, RestBin};
decoder(32, RestBin) ->
   parseInt(1, RestBin);
decoder(33, RestBin) ->
   parseInt(2, RestBin);
decoder(34, RestBin) ->
   parseInt(3, RestBin);
decoder(35, RestBin) ->
   parseInt(4, RestBin);
decoder(36, RestBin) ->
   parseInt(5, RestBin);
decoder(37, RestBin) ->
   parseInt(6, RestBin);
decoder(38, RestBin) ->
   parseInt(7, RestBin);
decoder(39, RestBin) ->
   parseInt(8, RestBin);
decoder(40, RestBin) ->
   parseUint(1, RestBin);
decoder(41, RestBin) ->
   parseUint(2, RestBin);
decoder(42, RestBin) ->
   parseUint(3, RestBin);
decoder(43, RestBin) ->
   parseUint(4, RestBin);
decoder(44, RestBin) ->
   parseUint(5, RestBin);
decoder(45, RestBin) ->
   parseUint(6, RestBin);
decoder(46, RestBin) ->
   parseUint(7, RestBin);
decoder(47, RestBin) ->
   parseUint(8, RestBin);
decoder(48, RestBin) ->
   {0, RestBin};
decoder(49, RestBin) ->
   {1, RestBin};
decoder(50, RestBin) ->
   {2, RestBin};
decoder(51, RestBin) ->
   {3, RestBin};
decoder(52, RestBin) ->
   {4, RestBin};
decoder(53, RestBin) ->
   {5, RestBin};
decoder(54, RestBin) ->
   {6, RestBin};
decoder(55, RestBin) ->
   {7, RestBin};
decoder(56, RestBin) ->
   {8, RestBin};
decoder(57, RestBin) ->
   {9, RestBin};
decoder(58, RestBin) ->
   {-6, RestBin};
decoder(59, RestBin) ->
   {-5, RestBin};
decoder(60, RestBin) ->
   {-4, RestBin};
decoder(61, RestBin) ->
   {-3, RestBin};
decoder(62, RestBin) ->
   {-2, RestBin};
decoder(63, RestBin) ->
   {-1, RestBin};
decoder(64, RestBin) ->
   {<<>>, RestBin};
decoder(65, RestBin) ->
   <<BinStr:1/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(66, RestBin) ->
   <<BinStr:2/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(67, RestBin) ->
   <<BinStr:3/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(68, RestBin) ->
   <<BinStr:4/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(69, RestBin) ->
   <<BinStr:5/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(70, RestBin) ->
   <<BinStr:6/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(71, RestBin) ->
   <<BinStr:7/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(72, RestBin) ->
   <<BinStr:8/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(73, RestBin) ->
   <<BinStr:9/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(74, RestBin) ->
   <<BinStr:10/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(75, RestBin) ->
   <<BinStr:11/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(76, RestBin) ->
   <<BinStr:12/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(77, RestBin) ->
   <<BinStr:13/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(78, RestBin) ->
   <<BinStr:14/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(79, RestBin) ->
   <<BinStr:15/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(80, RestBin) ->
   <<BinStr:16/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(81, RestBin) ->
   <<BinStr:17/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(82, RestBin) ->
   <<BinStr:18/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(83, RestBin) ->
   <<BinStr:19/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(84, RestBin) ->
   <<BinStr:20/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(85, RestBin) ->
   <<BinStr:21/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(86, RestBin) ->
   <<BinStr:22/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(87, RestBin) ->
   <<BinStr:23/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(88, RestBin) ->
   <<BinStr:24/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(89, RestBin) ->
   <<BinStr:25/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(90, RestBin) ->
   <<BinStr:26/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(91, RestBin) ->
   <<BinStr:27/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(92, RestBin) ->
   <<BinStr:28/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(93, RestBin) ->
   <<BinStr:29/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(94, RestBin) ->
   <<BinStr:30/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(95, RestBin) ->
   <<BinStr:31/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(96, RestBin) ->
   <<BinStr:32/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(97, RestBin) ->
   <<BinStr:33/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(98, RestBin) ->
   <<BinStr:34/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(99, RestBin) ->
   <<BinStr:35/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(100, RestBin) ->
   <<BinStr:36/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(101, RestBin) ->
   <<BinStr:37/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(102, RestBin) ->
   <<BinStr:38/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(103, RestBin) ->
   <<BinStr:39/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(104, RestBin) ->
   <<BinStr:40/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(105, RestBin) ->
   <<BinStr:41/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(106, RestBin) ->
   <<BinStr:42/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(107, RestBin) ->
   <<BinStr:43/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(108, RestBin) ->
   <<BinStr:44/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(109, RestBin) ->
   <<BinStr:45/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(110, RestBin) ->
   <<BinStr:46/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(111, RestBin) ->
   <<BinStr:47/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(112, RestBin) ->
   <<BinStr:48/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(113, RestBin) ->
   <<BinStr:49/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(114, RestBin) ->
   <<BinStr:50/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(115, RestBin) ->
   <<BinStr:51/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(116, RestBin) ->
   <<BinStr:52/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(117, RestBin) ->
   <<BinStr:53/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(118, RestBin) ->
   <<BinStr:54/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(119, RestBin) ->
   <<BinStr:55/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(120, RestBin) ->
   <<BinStr:56/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(121, RestBin) ->
   <<BinStr:57/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(122, RestBin) ->
   <<BinStr:58/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(123, RestBin) ->
   <<BinStr:59/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(124, RestBin) ->
   <<BinStr:60/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(125, RestBin) ->
   <<BinStr:61/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(126, RestBin) ->
   <<BinStr:62/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(127, RestBin) ->
   <<BinStr:63/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(128, RestBin) ->
   <<BinStr:64/binary, LeftBin/bitstring>> = RestBin,
   {BinStr, LeftBin};
decoder(129, RestBin) ->
   <<BinStr:65/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 65 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(130, RestBin) ->
   <<BinStr:66/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 66 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(131, RestBin) ->
   <<BinStr:67/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 67 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(132, RestBin) ->
   <<BinStr:68/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 68 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(133, RestBin) ->
   <<BinStr:69/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 69 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(134, RestBin) ->
   <<BinStr:70/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 70 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(135, RestBin) ->
   <<BinStr:71/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 71 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(136, RestBin) ->
   <<BinStr:72/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 72 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(137, RestBin) ->
   <<BinStr:73/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 73 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(138, RestBin) ->
   <<BinStr:74/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 74 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(139, RestBin) ->
   <<BinStr:75/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 75 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(140, RestBin) ->
   <<BinStr:76/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 76 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(141, RestBin) ->
   <<BinStr:77/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 77 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(142, RestBin) ->
   <<BinStr:78/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 78 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(143, RestBin) ->
   <<BinStr:79/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 79 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(144, RestBin) ->
   <<BinStr:80/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 80 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(145, RestBin) ->
   <<BinStr:81/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 81 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(146, RestBin) ->
   <<BinStr:82/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 82 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(147, RestBin) ->
   <<BinStr:83/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 83 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(148, RestBin) ->
   <<BinStr:84/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 84 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(149, RestBin) ->
   <<BinStr:85/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 85 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(150, RestBin) ->
   <<BinStr:86/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 86 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(151, RestBin) ->
   <<BinStr:87/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 87 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(152, RestBin) ->
   <<BinStr:88/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 88 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(153, RestBin) ->
   <<BinStr:89/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 89 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(154, RestBin) ->
   <<BinStr:90/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 90 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(155, RestBin) ->
   <<BinStr:91/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 91 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(156, RestBin) ->
   <<BinStr:92/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 92 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(157, RestBin) ->
   <<BinStr:93/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 93 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(158, RestBin) ->
   <<BinStr:94/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 94 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(159, RestBin) ->
   <<BinStr:95/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 95 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(160, RestBin) ->
   <<BinStr:96/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 96 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(161, RestBin) ->
   <<BinStr:97/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 97 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(162, RestBin) ->
   <<BinStr:98/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 98 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(163, RestBin) ->
   <<BinStr:99/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 99 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(164, RestBin) ->
   <<BinStr:100/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 100 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(165, RestBin) ->
   <<BinStr:101/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 101 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(166, RestBin) ->
   <<BinStr:102/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 102 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(167, RestBin) ->
   <<BinStr:103/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 103 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(168, RestBin) ->
   <<BinStr:104/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 104 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(169, RestBin) ->
   <<BinStr:105/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 105 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(170, RestBin) ->
   <<BinStr:106/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 106 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(171, RestBin) ->
   <<BinStr:107/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 107 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(172, RestBin) ->
   <<BinStr:108/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 108 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(173, RestBin) ->
   <<BinStr:109/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 109 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(174, RestBin) ->
   <<BinStr:110/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 110 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(175, RestBin) ->
   <<BinStr:111/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 111 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(176, RestBin) ->
   <<BinStr:112/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 112 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(177, RestBin) ->
   <<BinStr:113/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 113 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(178, RestBin) ->
   <<BinStr:114/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 114 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(179, RestBin) ->
   <<BinStr:115/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 115 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(180, RestBin) ->
   <<BinStr:116/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 116 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(181, RestBin) ->
   <<BinStr:117/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 117 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(182, RestBin) ->
   <<BinStr:118/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 118 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(183, RestBin) ->
   <<BinStr:119/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 119 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(184, RestBin) ->
   <<BinStr:120/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 120 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(185, RestBin) ->
   <<BinStr:121/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 121 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(186, RestBin) ->
   <<BinStr:122/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 122 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(187, RestBin) ->
   <<BinStr:123/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 123 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(188, RestBin) ->
   <<BinStr:124/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 124 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(189, RestBin) ->
   <<BinStr:125/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 125 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(190, RestBin) ->
   <<BinStr:126/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / 126 > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(191, RestBin) ->
   <<Length:64/integer-little-unsigned, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(192, RestBin) ->
   <<Length:1/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(193, RestBin) ->
   <<Length:2/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(194, RestBin) ->
   <<Length:3/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(195, RestBin) ->
   <<Length:4/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(196, RestBin) ->
   <<Length:5/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(197, RestBin) ->
   <<Length:6/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(198, RestBin) ->
   <<Length:7/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(199, RestBin) ->
   <<Length:8/integer-little-unsigned-unit:8, BinStr:Length/binary, LeftBin/bitstring>> = RestBin,
   RefSize = binary:referenced_byte_size(RestBin),
   case RefSize / Length > ?VpBinaryCopyRatio of
      true ->
         {binary:copy(BinStr), LeftBin};
      _ ->
         {BinStr, LeftBin}
   end;
decoder(_, _) ->
   erlang:throw(unexpected_end).


parseArrayElements(<<>>, AccList) -> lists:reverse(AccList);
parseArrayElements(DataBin, AccList) ->
   {Elem, Rest} = decoder(DataBin),
   parseArrayElements(Rest, [Elem | AccList]).

parseArrayWithIndexTable(9, <<SumSize:64/integer-little-unsigned, RestBin/binary>>) ->
   DataSize = SumSize - 1 - 8 - 8,
   <<DataBin:DataSize/binary, Length:64/integer-little-unsigned, LeftBin/binary>> = RestBin,
   IndexSize = Length * 8,
   DataSize = DataSize - IndexSize,
   <<ArrData:DataSize/binary, _Index:IndexSize/binary>> = DataBin,
   ArrList = parseArrayElements(ArrData, []),
   {ArrList, LeftBin};
parseArrayWithIndexTable(Type, DataBin) ->
   SizeBytes = 1 bsl (Type - 6),
   <<SumSize:SizeBytes/integer-little-unsigned-unit:8, Length:SizeBytes/integer-little-unsigned-unit:8, RestBin/binary>> = DataBin,
   IndexSize = SizeBytes * Length,
   DataSize = erlang:byte_size(RestBin),
   DataLeftBin = skipZeros(RestBin),
   ZerosSize = DataSize - erlang:byte_size(DataLeftBin),
   ArrSize = SumSize - 1 - 2 * SizeBytes - ZerosSize - IndexSize,
   <<ArrData:ArrSize/binary, _Index:IndexSize/binary, LeftBin/binary>> = DataLeftBin,
   ArrList = parseArrayElements(ArrData, []),
   {ArrList, LeftBin}.

parseArrayWithoutIndexTable(Type, DataBin) ->
   SizeBytes = 1 bsl (Type - 2),
   <<SumSize:SizeBytes/integer-little-unsigned-unit:8, RestBin/binary>> = DataBin,
   DataSize = erlang:byte_size(RestBin),
   DataLeftBin = skipZeros(RestBin),
   ZerosSize = DataSize - erlang:byte_size(DataLeftBin),
   ArrSize = SumSize - SizeBytes - 1 - ZerosSize,
   <<ArrData:ArrSize/binary, LeftBin/binary>> = DataLeftBin,
   ArrList = parseArrayElements(ArrData, []),
   {ArrList, LeftBin}.

parseCompactArray(DataBin) ->
   {ArrData, _Length, RestBin} = parseCompactHeader(DataBin),
   ArrList = parseArrayElements(ArrData, []),
   {ArrList, RestBin}.

parseCompactHeader(DataBin) ->
   {Size, RestBin} = parseLength(DataBin, 0, 0, false),
   DataSize = Size - (erlang:byte_size(DataBin) - erlang:byte_size(RestBin)) - 1,
   <<ArrData:DataSize/binary, LeftBin/binary>> = RestBin,
   {Length, HeaderBin} = parseLength(ArrData, 0, 0, true),
   {HeaderBin, Length, LeftBin}.

parseCompactObject(DataBin) ->
   {ObjData, Length, RestBin} = parseCompactHeader(DataBin),
   {Obj, <<>>} = parseObjectMembers(Length, #{}, ObjData),
   {Obj, RestBin}.

parseInt(IntegerSize, DataBin) ->
   <<Integer:IntegerSize/integer-little-signed-unit:8, LeftBin/binary>> = DataBin,
   {Integer, LeftBin}.

parseUint(IntegerSize, DataBin) ->
   <<Integer:IntegerSize/integer-little-unsigned-unit:8, LeftBin/binary>> = DataBin,
   {Integer, LeftBin}.

parseLength(DataBin, Len, Pt, Reverse) ->
   {LastV, LeftBin} =
      case Reverse of
         false ->
            <<V/integer, RestBin/binary>> = DataBin,
            {V, RestBin};
         _ ->
            Size = erlang:byte_size(DataBin) - 1,
            <<RestBin:Size/binary, V/integer>> = DataBin,
            {V, RestBin}
      end,
   NewLen = Len + (LastV band 127 bsl Pt),
   NewPt = Pt + 7,
   case LastV band 128 /= 0 of
      false -> {NewLen, LeftBin};
      true -> parseLength(LeftBin, NewLen, NewPt, Reverse)
   end.

parseObject(Type, DataBin) ->
   SizeBytes = 1 bsl (Type - 11),
   <<SumSize:SizeBytes/integer-little-unsigned-unit:8, Length:SizeBytes/integer-little-unsigned-unit:8, ObjBin/binary>> = DataBin,
   DataSize = SumSize - 1 - 2 * SizeBytes,
   <<ZerosBin:DataSize/binary, LeftBin/binary>> = ObjBin,
   IndexTableSize = Length * SizeBytes,
   {Obj, <<_IndexTable:IndexTableSize/binary>>} = parseObjectMembers(Length, #{}, skipZeros(ZerosBin)),
   {Obj, LeftBin}.

parseObjectMembers(0, Obj, DataBin) ->
   {Obj, DataBin};
parseObjectMembers(Length, Obj, DataBin) ->
   {Key, ValueDataBin} = decoder(DataBin),
   {Value, LeftBin} = decoder(ValueDataBin),
   NewObj = Obj#{Key => Value},
   parseObjectMembers(Length - 1, NewObj, LeftBin).

skipZeros(<<0/integer, LeftBin/binary>>) ->
   skipZeros(LeftBin);
skipZeros(DataBin) -> DataBin.



