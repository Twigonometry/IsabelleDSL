{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Calculator(Int, Session, pp) where {

import Prelude ((==), (/=), (<), (<=), (>=), (>), (+), (-), (*), (/), (**),
  (>>=), (>>), (=<<), (&&), (||), (^), (^^), (.), ($), ($!), (++), (!!), Eq,
  error, id, return, not, fst, snd, map, filter, concat, concatMap, reverse,
  zip, null, takeWhile, dropWhile, all, any, Integer, negate, abs, divMod,
  String, Bool(True, False), Maybe(Nothing, Just));{-# , Char); #-}
import qualified Prelude;
{-# import Data.Char; #-}

class Ord a where {
  less_eq :: a -> a -> Bool;
  less :: a -> a -> Bool;
};

instance Ord Integer where {
  less_eq = (\ a b -> a <= b);
  less = (\ a b -> a < b);
};

newtype Int = Int_of_integer Integer;

newtype Nat = Nat Integer;

data Num = One | Bit0 Num | Bit1 Num;

data Session = GetResult | Clear Session | Add Int Session | Sub Int Session
  | Mul Int Session | Div Int Session;

integer_of_int :: Int -> Integer;
integer_of_int (Int_of_integer k) = k;

max :: forall a. (Ord a) => a -> a -> a;
max a b = (if less_eq a b then b else a);

nat :: Int -> Nat;
nat k = Nat (max (0 :: Integer) (integer_of_int k));

uminus_int :: Int -> Int;
uminus_int k = Int_of_integer (negate (integer_of_int k));

zero_int :: Int;
zero_int = Int_of_integer (0 :: Integer);

less_int :: Int -> Int -> Bool;
less_int k l = integer_of_int k < integer_of_int l;

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

modulo_integer :: Integer -> Integer -> Integer;
modulo_integer k l = snd (divmod_integer k l);

integer_of_nat :: Nat -> Integer;
integer_of_nat (Nat x) = x;

modulo_nat :: Nat -> Nat -> Nat;
modulo_nat m n = Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));

divide_integer :: Integer -> Integer -> Integer;
divide_integer k l = fst (divmod_integer k l);

divide_nat :: Nat -> Nat -> Nat;
divide_nat m n = Nat (divide_integer (integer_of_nat m) (integer_of_nat n));

nat_of_integer :: Integer -> Nat;
nat_of_integer k = Nat (max (0 :: Integer) k);

equal_nat :: Nat -> Nat -> Bool;
equal_nat m n = integer_of_nat m == integer_of_nat n;

zero_nat :: Nat;
zero_nat = Nat (0 :: Integer);

one_nat :: Nat;
one_nat = Nat (1 :: Integer);

string_of_digit :: Nat -> String;
string_of_digit n =
  (if equal_nat n zero_nat then "0"
    else (if equal_nat n one_nat then "1"
           else (if equal_nat n (nat_of_integer (2 :: Integer)) then "2"
                  else (if equal_nat n (nat_of_integer (3 :: Integer)) then "3"
                         else (if equal_nat n (nat_of_integer (4 :: Integer))
                                then "4"
                                else (if equal_nat n
   (nat_of_integer (5 :: Integer))
                                       then "5"
                                       else (if equal_nat n
          (nat_of_integer (6 :: Integer))
      then "6"
      else (if equal_nat n (nat_of_integer (7 :: Integer)) then "7"
             else (if equal_nat n (nat_of_integer (8 :: Integer)) then "8"
                    else "9")))))))));

less_nat :: Nat -> Nat -> Bool;
less_nat m n = integer_of_nat m < integer_of_nat n;

string_of_nat :: Nat -> String;
string_of_nat n =
  (if less_nat n (nat_of_integer (10 :: Integer)) then string_of_digit n
    else string_of_nat (divide_nat n (nat_of_integer (10 :: Integer))) ++
           string_of_digit (modulo_nat n (nat_of_integer (10 :: Integer))));

string_of_int :: Int -> String;
string_of_int i =
  (if less_int i zero_int then "-" ++ string_of_nat (nat (uminus_int i))
    else string_of_nat (nat i));

pp :: Session -> String;
pp GetResult = ".getResult()";
pp (Clear ses) = ".clear()" ++ pp ses;
pp (Add i ses) = ((".add(" ++ string_of_int i) ++ ")") ++ pp ses;
pp (Sub i ses) = ((".sub(" ++ string_of_int i) ++ ")") ++ pp ses;
pp (Mul i ses) = ((".mul(" ++ string_of_int i) ++ ")") ++ pp ses;
pp (Div i ses) = ((".div(" ++ string_of_int i) ++ ")") ++ pp ses;

}
