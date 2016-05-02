-module id_generation_tools.
 
-export([encode64/1,encodeCrockford/1,b64e/1,make_number/3,canoncicalize_crockford/1,
         encodeBinary/1,gen64BitRandomId/0,genWordadoplicusUnique/0,
        genWordadoplicusUnique/1]).

encode64(Num) ->
    encode(Num,64,fun b64e/1).
encodeCrockford(Num) ->
    encode(Num,32,fun crockford32e/1).
encodeBinary(Num) ->
    encode(Num,2,fun binary32e/1).

encode(Num,Radix,Fun) ->
    Indices = make_number(Num,Radix,[]),
    lists:map(fun(X) ->
                      Fun(X) end, Indices).
canoncicalize_crockford(CrockOSheet) when is_binary(CrockOSheet)-> 
    canoncicalize_crockford(binary_to_list(CrockOSheet));
canoncicalize_crockford(CrockOSheet) when is_atom(CrockOSheet)-> 
    canoncicalize_crockford(atom_to_list(CrockOSheet));
canoncicalize_crockford(CrockOSheet) when is_list(CrockOSheet)-> 
    Decode = [
      {$0,$0},
      {$1,$1},
      {$i,$1},
      {$I,$1},
      {$L,$1},
      {$l,$1},

      {$2,$2},
      {$3,$3},
      {$4,$4},
      {$5,$5},
      {$6,$6},
      {$7,$7},
      {$8,$8},
      {$9,$9},

      {$A,$a},
      {$B,$b},
      {$C,$c},
      {$D,$d},
      {$E,$e},
      {$F,$f},
      {$G,$g},
      {$H,$h},
      {$J,$j},
      {$K,$k},
      {$M,$m},
      {$N,$n},
      {$P,$p},
      {$Q,$q},
      {$R,$r},
      {$S,$s},
      {$T,$t},
      {$V,$v},
      {$W,$w},
      {$X,$x},
      {$Y,$y},
      {$Z,$z},

      {$a,$a},
      {$b,$b},
      {$c,$c},
      {$d,$d},
      {$e,$e},
      {$f,$f},
      {$g,$g},
      {$h,$h},
      {$j,$j},
      {$k,$k},
      {$m,$m},
      {$n,$n},
      {$p,$p},
      {$q,$q},
      {$r,$r},
      {$s,$s},
      {$t,$t},
      {$v,$v},
      {$w,$w},
      {$x,$x},
      {$y,$y},
      {$z,$z}
    ],
    lists:map(fun(X) ->    proplists:get_value(X,Decode) end, CrockOSheet).

genWordadoplicusUnique(crockford) ->
    Num = gen64BitRandomId(),
    encode(Num,32, fun crockford32e/1);
genWordadoplicusUnique(b64) ->
    Num = gen64BitRandomId(),
    encode(Num,64, fun b64e/1);
genWordadoplicusUnique(b62) ->
    Num = gen64BitRandomId(),
    encode(Num,62, fun b62e/1).

    

genWordadoplicusUnique() ->
    genWordadoplicusUnique(crockford).

gen64BitRandomId() ->
    %% http://engineering.custommade.com/simpleflake-distributed-id-generation-for-the-lazy/
    Last23Bits = crypto:rand_uniform(1,2#11111111111111111111111),
    April29th2010 = 1272546805* 1000,
    {Mega, Sec, Micro} = erlang:timestamp(),
    Timestamp = Mega * 1000000 *1000 + Sec *1000 + (Micro div 1000) ,    %% we only want millisecond precision
    case (Timestamp - April29th2010) of
        Diff when Diff < 0 ->
            throw(server_configured_before_april29th_2010);
        _ ->
            ok
    end,
    FirstPart = (Timestamp - April29th2010) bsl 23,  
    FirstPart bor Last23Bits.
    

make_number(Num,Radix,Accum) ->
    Remainder = Num rem Radix,
    Div = Num div Radix,
    Accum2 = [Remainder | Accum],     
    case {Div,Remainder} of 
        {0,0} ->
            Accum;
        _ ->
            make_number(Div,Radix,Accum2)
    end.
        

binary32e(X)  ->
    element(X+1,
            {
              $0, $1}).   
       
        
crockford32e(X)  ->
    element(X+1,
            {
              $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $a, $b, $c, $d, $e, $f, $g, $h,    $j, $k,    $m, $n,
               $p, $q, $r, $s, $t,    $v, $w, $x, $y, $z}).   
    

%% accessors 
b64e(X) ->
    element(X+1,
            {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
             $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
             $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
             $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
             $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $+, $/}).

b62e(X) ->
    element(X+1,
            {$A, $B, $C, $D, $E, $F, $G, $H, $I, $J, $K, $L, $M, $N,
             $O, $P, $Q, $R, $S, $T, $U, $V, $W, $X, $Y, $Z,
             $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n,
             $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z,
             $0, $1, $2, $3, $4, $5, $6, $7, $8, $9}).
