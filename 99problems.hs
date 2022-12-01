import Data.List
import Data.Tuple
import System.Random

data NestedList a = Elem a | List [NestedList a]
data Duplicates a = Multiple Int a | Single a deriving(Show, Eq)
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving(Show, Eq)

-- 1~10
myLast [x] = x; myLast (_:xs) = myLast xs
myButLast = head.tail.reverse
elementAt l i | i==0=head l | otherwise=elementAt (tail l) (i-1)
myLength []=0;myLength(_:xs)=1+myLength xs
myReverse []=[];myReverse(x:xs)=myReverse xs++[x]
isPalindrome :: Eq a => [a] -> Bool;isPalindrome=reverse>>=(==)
flatten(Elem x)=[x];flatten(List x)=foldr(++)[]$map(flatten)x
compress []=[];compress [x]=[x];compress (x:xs) | x==(head xs)=(compress xs) | otherwise=[x]++(compress xs)
pack []=[];pack(x:xs)=(x:takeWhile (==x) xs):pack(dropWhile(==x)xs)
encode b = (map (\x->(length x,head x)) . pack) b

-- 11~20
encodeModified b=(map(\(x,y)->if x==1 then Single y else Multiple x y).encode)b
decodeModified b=concatMap(\x->case x of;Single x->[x];Multiple x y->replicate x y)b
encodeDirect b=map(\x->if (length x)==1 then Single$head x else Multiple (length x)(head x))$pack b
dupli=(replicate 2 =<<)
repli=(.replicate).(>>=)
dropEvery l n= (l!!) <$> [0..(length l)-1] \\ [n-1,2*n-1..(length l)]
split l n = (take n l,drop n l)
slice l a b = drop (a-1) $ take b l
rotate l n = let x=n`mod`(length l) in (drop x l)++(take x l)
removeAt n l = (l!!(n-1),l\\ [l!!(n-1)])

-- 21~28
insertAt e l n = (take (n-1) l)++[e]++(drop (n-1) l)
range a b = [a..b]
rndSelect l n = do gen <- newStdGen; return $ take n [l!!n|n<-randomRs(0,(length l)-1) gen]
diffSelect k n = do gen<-newStdGen;return$take k[ b|b<-randomRs(0,n)gen]
rndPermu k = rndSelect k (length k)
combinations k ns = filter ((k==).length) (subsequences ns)
group = undefined --this is hard! :(
lsort = undefined -- sorting :(

-- 31~41
isPrime 1=False;isPrime 2=True;isPrime n=0<(foldl1(min)$map(mod n)[2..n-1])
myGCD a b|b==0=a|a==0=b|otherwise=gcd b(a`mod`b)
coprime=((1==).).myGCD
totient a=(length.(filter (coprime a)))[1..a]
primeFactors a=let n=[ k|k<-[1..a],0==a`mod`k,isPrime k] in case n of[]->[];_->[head n]++(primeFactors$a`div`(head n))
primeFactorsMult=map swap.encode.primeFactors
totientImproved=(foldl1(*).(map(\(x,y)->(x-1)*(x^(y-1)))).primeFactorsMult)
-- Q38: totientImproved has better performance. try with 230347777!
primesR a b=filter isPrime [a..b]
goldbach a=head$filter(\(x,y)->isPrime x&&(isPrime y))$zipWith(,)[a-1,a-2..1][1..a-1]
goldbachList a b=[goldbach k|k<-[a..b],k`mod`2 == 0]
goldbachList' a b c=[ (x,y)|(x,y)<-goldbachList a b, x>50 && (y>50)]

-- 54A~60
-- there is no problem 54.
-- 55 is really confusing lol
symmetric (Branch _ a b)=a==b;symmetric Empty=True

