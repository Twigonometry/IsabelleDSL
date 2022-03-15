{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Calculator(Int, Char, Session, pp) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));
import qualified Prelude;

newtype Int = Int_of_integer Integer;

data Nat = Zero_nat | Suc Nat;

data Num = One | Bit0 Num | Bit1 Num;

data Char = Char Bool Bool Bool Bool Bool Bool Bool Bool;

data Session = GetResult | Clear Session | Add Int Session | Sub Int Session
  | Mul Int Session | Div Int Session;

integer_of_int :: Int -> Integer;
integer_of_int (Int_of_integer k) = k;

apsnd :: forall a b c. (a -> b) -> (c, a) -> (c, b);
apsnd f (x, y) = (x, f y);

divmod_integer :: Integer -> Integer -> (Integer, Integer);
divmod_integer k l =
  (if k == (0 :: Integer) then ((0 :: Integer), (0 :: Integer))
    else (if (0 :: Integer) < l
           then (if (0 :: Integer) < k then divMod (abs k) (abs l)
                  else (case divMod (abs k) (abs l) of {
                         (r, s) ->
                           (if s == (0 :: Integer)
                             then (negate r, (0 :: Integer))
                             else (negate r - (1 :: Integer), l - s));
                       }))
           else (if l == (0 :: Integer) then ((0 :: Integer), k)
                  else apsnd negate
                         (if k < (0 :: Integer) then divMod (abs k) (abs l)
                           else (case divMod (abs k) (abs l) of {
                                  (r, s) ->
                                    (if s == (0 :: Integer)
                                      then (negate r, (0 :: Integer))
                                      else (negate r - (1 :: Integer),
     negate l - s));
                                })))));

plus_nat :: Nat -> Nat -> Nat;
plus_nat (Suc m) n = plus_nat m (Suc n);
plus_nat Zero_nat n = n;

one_nat :: Nat;
one_nat = Suc Zero_nat;

nat_of_integer :: Integer -> Nat;
nat_of_integer k =
  (if k <= (0 :: Integer) then Zero_nat
    else (case divmod_integer k (2 :: Integer) of {
           (l, j) ->
             let {
               la = nat_of_integer l;
               lb = plus_nat la la;
             } in (if j == (0 :: Integer) then lb else plus_nat lb one_nat);
         }));

nat :: Int -> Nat;
nat = nat_of_integer . integer_of_int;

uminus_int :: Int -> Int;
uminus_int k = Int_of_integer (negate (integer_of_int k));

zero_int :: Int;
zero_int = Int_of_integer (0 :: Integer);

less_int :: Int -> Int -> Bool;
less_int k l = integer_of_int k < integer_of_int l;

minus_nat :: Nat -> Nat -> Nat;
minus_nat (Suc m) (Suc n) = minus_nat m n;
minus_nat Zero_nat n = Zero_nat;
minus_nat m Zero_nat = m;

equal_nat :: Nat -> Nat -> Bool;
equal_nat Zero_nat (Suc x2) = False;
equal_nat (Suc x2) Zero_nat = False;
equal_nat (Suc x2) (Suc y2) = equal_nat x2 y2;
equal_nat Zero_nat Zero_nat = True;

less_nat :: Nat -> Nat -> Bool;
less_nat m (Suc n) = less_eq_nat m n;
less_nat n Zero_nat = False;

less_eq_nat :: Nat -> Nat -> Bool;
less_eq_nat (Suc m) n = less_nat m n;
less_eq_nat Zero_nat n = True;

divmod_nat :: Nat -> Nat -> (Nat, Nat);
divmod_nat m n =
  (if equal_nat n Zero_nat || less_nat m n then (Zero_nat, m)
    else let {
           a = divmod_nat (minus_nat m n) n;
         } in (case a of {
                (q, aa) -> (Suc q, aa);
              }));

modulo_nat :: Nat -> Nat -> Nat;
modulo_nat m n = snd (divmod_nat m n);

divide_nat :: Nat -> Nat -> Nat;
divide_nat m n = fst (divmod_nat m n);

nat_of_num :: Num -> Nat;
nat_of_num (Bit1 n) = let {
                        m = nat_of_num n;
                      } in Suc (plus_nat m m);
nat_of_num (Bit0 n) = let {
                        m = nat_of_num n;
                      } in plus_nat m m;
nat_of_num One = one_nat;

string_of_digit :: Nat -> [Char];
string_of_digit n =
  (if equal_nat n Zero_nat
    then [Char False False False False True True False False]
    else (if equal_nat n one_nat
           then [Char True False False False True True False False]
           else (if equal_nat n (nat_of_num (Bit0 One))
                  then [Char False True False False True True False False]
                  else (if equal_nat n (nat_of_num (Bit1 One))
                         then [Char True True False False True True False False]
                         else (if equal_nat n (nat_of_num (Bit0 (Bit0 One)))
                                then [Char False False True False True True
False False]
                                else (if equal_nat n
   (nat_of_num (Bit1 (Bit0 One)))
                                       then [Char True False True False True
       True False False]
                                       else (if equal_nat n
          (nat_of_num (Bit0 (Bit1 One)))
      then [Char False True True False True True False False]
      else (if equal_nat n (nat_of_num (Bit1 (Bit1 One)))
             then [Char True True True False True True False False]
             else (if equal_nat n (nat_of_num (Bit0 (Bit0 (Bit0 One))))
                    then [Char False False False True True True False False]
                    else [Char True False False True True True False
                            False])))))))));

string_of_nat :: Nat -> [Char];
string_of_nat n =
  (if less_nat n (nat_of_num (Bit0 (Bit1 (Bit0 One)))) then string_of_digit n
    else string_of_nat (divide_nat n (nat_of_num (Bit0 (Bit1 (Bit0 One))))) ++
           string_of_digit
             (modulo_nat n (nat_of_num (Bit0 (Bit1 (Bit0 One))))));

string_of_int :: Int -> [Char];
string_of_int i =
  (if less_int i zero_int
    then [Char True False True True False True False False] ++
           string_of_nat (nat (uminus_int i))
    else string_of_nat (nat i));

pp :: Session -> [Char];
pp GetResult =
  [Char False True True True False True False False,
    Char True True True False False True True False,
    Char True False True False False True True False,
    Char False False True False True True True False,
    Char False True False False True False True False,
    Char True False True False False True True False,
    Char True True False False True True True False,
    Char True False True False True True True False,
    Char False False True True False True True False,
    Char False False True False True True True False,
    Char False False False True False True False False,
    Char True False False True False True False False];
pp (Clear ses) =
  [Char False True True True False True False False,
    Char True True False False False True True False,
    Char False False True True False True True False,
    Char True False True False False True True False,
    Char True False False False False True True False,
    Char False True False False True True True False,
    Char False False False True False True False False,
    Char True False False True False True False False] ++
    pp ses;
pp (Add i ses) =
  [Char False True True True False True False False,
    Char True False False False False True True False,
    Char False False True False False True True False,
    Char False False True False False True True False,
    Char False False False True False True False False] ++
    string_of_int i ++
      [Char True False False True False True False False] ++ pp ses;
pp (Sub i ses) =
  [Char False True True True False True False False,
    Char True True False False True True True False,
    Char True False True False True True True False,
    Char False True False False False True True False,
    Char False False False True False True False False] ++
    string_of_int i ++
      [Char True False False True False True False False] ++ pp ses;
pp (Mul i ses) =
  [Char False True True True False True False False,
    Char True False True True False True True False,
    Char True False True False True True True False,
    Char False False True True False True True False,
    Char False False False True False True False False] ++
    string_of_int i ++
      [Char True False False True False True False False] ++ pp ses;
pp (Div i ses) =
  [Char False True True True False True False False,
    Char False False True False False True True False,
    Char True False False True False True True False,
    Char False True True False True True True False,
    Char False False False True False True False False] ++
    string_of_int i ++
      [Char True False False True False True False False] ++ pp ses;

}
