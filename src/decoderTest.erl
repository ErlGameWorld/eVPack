-module(decoderTest).

-compile([no_auto_import]).

-export([
   test/0
]).

test() ->
   try do() of
      _ ->
         ok
   catch
      E:C:S ->
         {E, C, S}
   end.

do() ->
   [] = eVPack:decode(<<1>>),
   #{} = eVPack:decode(<<10>>),
   illegal = eVPack:decode(<<23>>),
   nil = eVPack:decode(<<24>>),
   false = eVPack:decode(<<25>>),
   true = eVPack:decode(<<26>>),
   1.33699999999999988631e+2 = eVPack:decode(<<27, 102, 102, 102, 102, 102, 182, 96, 64>>),
   -1.33699999999999988631e+2 = eVPack:decode(<<27, 102, 102, 102, 102, 102, 182, 96, 192>>),
   609976800000 = eVPack:decode(<<28, 0, 83, 115, 5, (-114), 0, 0, 0>>),
   min_key = eVPack:decode(<<30>>),
   max_key = eVPack:decode(<<31>>),

   0 = eVPack:decode(<<48>>),
   1 = eVPack:decode(<<49>>),
   2 = eVPack:decode(<<50>>),
   3 = eVPack:decode(<<51>>),
   4 = eVPack:decode(<<52>>),
   5 = eVPack:decode(<<53>>),
   6 = eVPack:decode(<<54>>),
   7 = eVPack:decode(<<55>>),
   8 = eVPack:decode(<<56>>),
   9 = eVPack:decode(<<57>>),
   -6 = eVPack:decode(<<58>>),
   -5 = eVPack:decode(<<59>>),
   -4 = eVPack:decode(<<60>>),
   -3 = eVPack:decode(<<61>>),
   -2 = eVPack:decode(<<62>>),
   -1 = eVPack:decode(<<63>>),
   -1 = eVPack:decode(<<32, 255>>),
   127 = eVPack:decode(<<32, 127>>),
   -128 = eVPack:decode(<<32, 128>>),
   -1 = eVPack:decode(<<33, 255, 255>>),
   -1 = eVPack:decode(<<34, 255, 255, 255>>),
   -1 = eVPack:decode(<<35, 255, 255, 255, 255>>),
   -1 = eVPack:decode(<<36, 255, 255, 255, 255, 255>>),
   -1 = eVPack:decode(<<37, 255, 255, 255, 255, 255, 255>>),
   -1 = eVPack:decode(<<38, 255, 255, 255, 255, 255, 255, 255>>),
   -1 = eVPack:decode(<<39, 255, 255, 255, 255, 255, 255, 255, 255>>),
   9223372036854775807 = eVPack:decode(<<39, 255, 255, 255, 255, 255, 255, 255, 127>>),
   255 = eVPack:decode(<<40, 255>>),
   65535 = eVPack:decode(<<41, 255, 255>>),
   16777215 = eVPack:decode(<<42, 255, 255, 255>>),
   4294967295 = eVPack:decode(<<43, 255, 255, 255, 255>>),
   1099511627775 = eVPack:decode(<<44, 255, 255, 255, 255, 255>>),
   281474976710655 = eVPack:decode(<<45, 255, 255, 255, 255, 255, 255>>),
   72057594037927935 = eVPack:decode(<<46, 255, 255, 255, 255, 255, 255, 255>>),
   18446744073709551615 = eVPack:decode(<<47, 255, 255, 255, 255, 255, 255, 255, 255>>),
   <<"Hallo Welt!">> = eVPack:decode(<<75, 72, 97, 108, 108, 111, 32, 87, 101, 108, 116, 33>>),
   <<"Hello World!">> = eVPack:decode(<<76, 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33>>),
   Str =
      <<"Lorem ipsum dolor sit amet, consectetuer "
      "adipiscing elit. Aenean commodo ligula "
      "eget dolor. ",
         "Aenean massa. Cum sociis natoque penatibus "
         "et magnis dis parturient montes, nascetur "
         "ridiculus mus. ",
         "Donec quam felis, ultricies nec, pellentesque "
         "eu, pretium quis, sem. Nulla consequat "
         "massa quis enim. ",
         "Donec pede justo, fringilla vel, aliquet "
         "nec, vulputate eget, arcu. In enim justo, "
         "rhoncus ut, imperdiet a, ",
         "venenatis vitae, justo. Nullam dictum "
         "felis eu pede mollis pretium. Integer "
         "tincidunt. Cras dapibus. ",
         "Vivamus elementum semper nisi. Aenean "
         "vulputate eleifend tellus.">>,
   StrBin = <<191, 55, 2, 0, 0, 0, 0, 0, 0, 76, 111, 114, 101, 109, 32, 105, 112, 115, 117, 109, 32, 100, 111, 108, 111, 114, 32, 115,
      105, 116, 32, 97, 109, 101, 116, 44, 32, 99, 111, 110, 115, 101, 99, 116, 101, 116, 117, 101, 114, 32, 97, 100,
      105, 112, 105, 115, 99, 105, 110, 103, 32, 101, 108, 105, 116, 46, 32, 65, 101, 110, 101, 97, 110, 32, 99, 111,
      109, 109, 111, 100, 111, 32, 108, 105, 103, 117, 108, 97, 32, 101, 103, 101, 116, 32, 100, 111, 108, 111, 114, 46,
      32, 65, 101, 110, 101, 97, 110, 32, 109, 97, 115, 115, 97, 46, 32, 67, 117, 109, 32, 115, 111, 99, 105, 105,
      115, 32, 110, 97, 116, 111, 113, 117, 101, 32, 112, 101, 110, 97, 116, 105, 98, 117, 115, 32, 101, 116, 32, 109,
      97, 103, 110, 105, 115, 32, 100, 105, 115, 32, 112, 97, 114, 116, 117, 114, 105, 101, 110, 116, 32, 109, 111, 110,
      116, 101, 115, 44, 32, 110, 97, 115, 99, 101, 116, 117, 114, 32, 114, 105, 100, 105, 99, 117, 108, 117, 115, 32,
      109, 117, 115, 46, 32, 68, 111, 110, 101, 99, 32, 113, 117, 97, 109, 32, 102, 101, 108, 105, 115, 44, 32, 117, 108, 116, 114, 105,
      99, 105, 101, 115, 32, 110, 101, 99, 44, 32, 112, 101, 108, 108, 101, 110, 116, 101, 115, 113, 117, 101, 32, 101,
      117, 44, 32, 112, 114, 101, 116, 105, 117, 109, 32, 113, 117, 105, 115, 44, 32, 115, 101, 109, 46, 32, 78, 117,
      108, 108, 97, 32, 99, 111, 110, 115, 101, 113, 117, 97, 116, 32, 109, 97, 115, 115, 97, 32, 113, 117, 105, 115, 32, 101, 110, 105,
      109, 46, 32, 68, 111, 110, 101, 99, 32, 112, 101, 100, 101, 32, 106, 117, 115, 116, 111, 44, 32, 102, 114, 105, 110, 103, 105, 108,
      108, 97, 32, 118, 101, 108, 44, 32, 97, 108, 105, 113, 117, 101, 116, 32, 110, 101, 99, 44, 32, 118, 117, 108,
      112, 117, 116, 97, 116, 101, 32, 101, 103, 101, 116, 44, 32, 97, 114, 99, 117, 46, 32, 73, 110, 32, 101, 110,
      105, 109, 32, 106, 117, 115, 116, 111, 44, 32, 114, 104, 111, 110, 99, 117, 115, 32, 117, 116, 44, 32, 105, 109, 112, 101, 114, 100,
      105, 101, 116, 32, 97, 44, 32, 118, 101, 110, 101, 110, 97, 116, 105, 115, 32, 118, 105, 116, 97, 101, 44, 32, 106, 117, 115, 116,
      111, 46, 32, 78, 117, 108, 108, 97, 109, 32, 100, 105, 99, 116, 117, 109, 32, 102, 101, 108, 105, 115, 32, 101,
      117, 32, 112, 101, 100, 101, 32, 109, 111, 108, 108, 105, 115, 32, 112, 114, 101, 116, 105, 117, 109, 46, 32, 73,
      110, 116, 101, 103, 101, 114, 32, 116, 105, 110, 99, 105, 100, 117, 110, 116, 46, 32, 67, 114, 97, 115, 32, 100,
      97, 112, 105, 98, 117, 115, 46, 32, 86, 105, 118, 97, 109, 117, 115, 32, 101, 108, 101, 109, 101, 110, 116, 117,
      109, 32, 115, 101, 109, 112, 101, 114, 32, 110, 105, 115, 105, 46, 32, 65, 101, 110, 101, 97, 110, 32, 118, 117,
      108, 112, 117, 116, 97, 116, 101, 32, 101, 108, 101, 105, 102, 101, 110, 100, 32, 116, 101, 108, 108, 117, 115, 46>>,
   Str = eVPack:decode(StrBin),
   ExBin = eVPack:decode(<<49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<192, 9, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<193, 9, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<194, 9, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<195, 9, 0, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<196, 9, 0, 0, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<197, 9, 0, 0, 0, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<198, 9, 0, 0, 0, 0, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = eVPack:decode(<<199, 9, 0, 0, 0, 0, 0, 0, 0, 49, 50, 51, 52, 53, 54, 55, 56, 57>>),
   ExBin = [1, 2, 3],
   Ex3 = eVPack:decode(<<6, 9, 3, 49, 50, 51, 3, 4, 5>>),
   Ex3 = eVPack:decode(<<6, 10, 3, 0, 49, 50, 51, 3, 4, 5>>),
   Ex3 = eVPack:decode(<<7, 14, 0, 3, 0, 49, 50, 51, 5, 0, 6, 0, 7, 0>>),
   Ex3 = eVPack:decode(<<8, 24, 0, 0, 0, 3, 0, 0, 0, 49, 50, 51, 9, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0>>),
   Ex3 = eVPack:decode(<<9, 44, 0, 0, 0, 0, 0, 0, 0, 49, 50, 51, 9, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 11, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0>>),
   Ex3 = eVPack:decode(<<6, 5, 1, 49, 3>>),
   Ex3 = [1],
   Ex4 = [1, 2, 3],
   Ex4 = eVPack:decode(<<2, 5, 49, 50, 51>>),
   Ex4 = eVPack:decode(<<2, 6, 0, 49, 50, 51>>),
   Ex4 = eVPack:decode(<<3, 6, 0, 49, 50, 51>>),
   Ex4 = eVPack:decode(<<4, 8, 0, 0, 0, 49, 50, 51>>),
   Ex4 = eVPack:decode(<<5, 12, 0, 0, 0, 0, 0, 0, 0, 49, 50, 51>>),
   [0.0] = eVPack:decode(<<2, 11, 27, 0:64/integer-little-unsigned>>),
   0 = eVPack:decode(<<2, 11, 28, 0:64/integer-little-unsigned>>),
   [1, 16] = eVPack:decode(<<19, 6, 49, 40, 16, 2>>),
   Ex5 = [[1, 2, 3], [1, 2, 3]],
   Ex5 = eVPack:decode(<<2, 12, 2, 5, 49, 50, 51, 2, 5, 49, 50, 51>>),
   Ex6 = [[1, 2, 3], [1, 2, 3]],
   Ex6 = eVPack:decode(<<2, 14, 19, 6, 49, 50, 51, 3, 19, 6, 49, 50, 51, 3>>),
   Ex7 = #{<<"a">> => <<"b">>},
   Ex7 = eVPack:decode(<<11, 8, 1, 65, 97, 65, 98, 3>>),
   Ex8 = #{<<"a">> => 12, <<"b">> => true, <<"c">> => <<"xyz">>},
   Ex8 = eVPack:decode(<<20, 16, 65, 97, 40, 12, 65, 98, 26, 65, 99, 67, 120, 121, 122, 3>>),
   Ex9 = [#{<<"a">> => 12, <<"b">> => true, <<"c">> => <<"xyz">>}, #{<<"a">> => 12, <<"b">> => true, <<"c">> => <<"xyz">>}],
   Ex9 = eVPack:decode(<<19, 35, 20, 16, 65, 97, 40, 12, 65, 98, 26, 65, 99, 67, 120, 121, 122, 3, 20, 16, 65, 97, 40, 12, 65, 98, 26, 65, 99, 67, 120, 121, 122, 3, 2>>),
   Ex10 = [#{<<"key">> => 42}, <<"fooooobar">>, <<"x">>, <<1, 2, 3, 4, 5, 6, 7, 8>>],
   Ex10Bin = <<2, 42, 11, 10, 1, 67, 107, 101, 121, 40, 42, 3, 73, 102, 111, 111, 111, 111, 111,
      98, 97, 114, 191, 1, 0, 0, 0, 0, 0, 0, 0, 120, 192, 8, 1, 2, 3, 4, 5, 6, 7, 8>>,
   Ex10 = eVPack:decode(Ex10Bin),
   _Ex11 = #{<<"0">> =>
   #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
      <<"2">> => <<"test">>, <<"3">> => <<"test">>,
      <<"4">> => <<"test">>},
      <<"1">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>},
      <<"2">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>},
      <<"3">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>},
      <<"4">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>}},

   _Ex12 = #{<<"0">> =>
   #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
      <<"2">> => <<"test">>, <<"3">> => <<"test">>,
      <<"4">> => <<"test">>, <<"5">> => <<"test">>,
      <<"6">> => <<"test">>, <<"7">> => <<"test">>,
      <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"1">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"2">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"3">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"4">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"5">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"6">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"7">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"8">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>},
      <<"9">> =>
      #{<<"0">> => <<"test">>, <<"1">> => <<"test">>,
         <<"2">> => <<"test">>, <<"3">> => <<"test">>,
         <<"4">> => <<"test">>, <<"5">> => <<"test">>,
         <<"6">> => <<"test">>, <<"7">> => <<"test">>,
         <<"8">> => <<"test">>, <<"9">> => <<"test">>}}.
