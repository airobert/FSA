FSA Lab Exercises Week 4
========================

> module FSAlab4
> where 
> import Data.List
> import System.Random

We need \verb^System.Random^ because we are going to explore a
probabilistic algorithms for efficient primality checking, plus
their uses in public key cryptography. 

---



[Public key cryptography](https://en.wikipedia.org/wiki/Public-key_cryptography)
is based on the assumption that factorization
of large numbers (products of large primes) is hard, while finding and
multiplying large primes is easy.  With large numbers, no efficient
integer factorization algorithm is known (unless we work with quantum
computers). See [integer factorization](http://en.wikipedia.org/wiki/Integer_factorization) for background. 

Here is a naive algorithm for factorization: 


     factors :: Integer -> [Integer]
     factors n = factors' n 2 where 
       factors' 1 _       = []
       factors' n m 
          | n `mod` m == 0 = m : factors' (n `div` m) m
          | otherwise      =     factors' n (m+1)


This can be improved somewhat by the observation that if all $m$ 
with $2 \leq m < n$ do not divide $n$, and $m^2 > n$, then $m$ and 
$n$ are relatively prime. This gives: 

> factors :: Integer -> [Integer]
> factors n = factors' n 2 where 
>   factors' 1 _       = []
>   factors' n m 
>     | n `mod` m == 0 =  m : factors' (n `div` m) m
>     | m^2 > n        = [n]
>     | otherwise      =      factors' n (m+1)

---

**Exercise 1**

Can you improve this further, using the fact that we are looking for
factors that are prime, and that instead of trying $(m+1)$ it is
enough to try out the next prime number?

---

In a sensational paper *PRIMES is in P* [@AgrawalCS2004:piip]
Manindra Agrawal, Neeraj Kayal, and Nitin Saxena presented a deterministic 
efficient algorithm for testing primality.
See [AKS primality test](http://en.wikipedia.org/wiki/AKS_primality_test). 

Previously known efficient algorithms were probabilistic. 
These exercises explore modular arithmetic on the integers, ending 
with primality testing using probabilistic algorithms, and the use of 
the fact that factorization is hard while primality testing is easy
for public key cryptography. 

We start with modular arithmetic, using the built in function `rem`
from Haskell. A key ingredient of efficient testing for primality is 
efficient exponentiation modulo a prime number. 

Modular addition: 

> addM :: Integer -> Integer -> Integer -> Integer
> addM x y = rem (x+y)

Modular multiplication: 

> multM :: Integer -> Integer -> Integer -> Integer
> multM x y = rem (x*y) 

Modular exponentiation: 

> expM ::  Integer -> Integer -> Integer -> Integer
> expM x y = rem (x^y)

This is not efficient, for we first compute $x^y$, and then reduce the
result modulo $N$.  Instead, we should have performed the intermediate
computation steps for $x^y$ modulo $N$.

---

**Exercise 2**

Implement a function 

     exM :: Integer -> Integer -> Integer -> Integer

that does modular exponentiation of $x^y$ in polynomial time, by 
repeatedly squaring modulo $N$.

E.g., $x^{33} \mod 5$ can be computed by means of 

$$      
  x^{33} \pmod 5 = x^{32} \pmod 5 \times x \pmod 5. 
$$
        
$x^{32} \pmod N$ is computed in five steps by means of repeatedly squaring modulo $N$: 

$$        
  x \pmod N \rightarrow x^2 \pmod N \rightarrow  x^4 \pmod N \rightarrow  \ldots 
   \rightarrow  x^{32} \pmod N.
$$
        
If this explanation is too concise, look up relevant literature.

---

**Exercise 3**

Check that your implementation is more efficient than `expM` by
running a number of relevant tests and documenting the results.

---      

Primality testing using a probabilistic algorithm is based on
efficient exponentiation modulo.

This uses Fermat's Little Theorem, which states the following. 

**Theorem (Fermat)**
          
If $p$ is prime, then for every $1 \leq a < p$: 
$a^{p-1} \equiv 1 \pmod p$.


To see what this says, first look at some examples:
Assume $p = 5$. Let us check $2^4$, $3^4$ and $5^4$. 

$$       
  2^4 = 16 \equiv 1 \pmod 5, 
  3^4 = 81 \equiv 1 \pmod 5, 
  4^4 = 256 \equiv 1 \pmod 5. 
$$

It turns out that for every  $1 \leq a < p$, the set of remainders 
modulo $p$ is equal to their product with $a$ modulo $p$. In other words, 
multiplying the set $\{ 1, \ldots, p-1 \}$ with $a$ modulo $p$ is simply 
to permute the set. You should try this out for $p = 5$ and $a = 3$, and 
you will find that

$$    
  \{ 1, 2, 3, 4 \} = \{ 3 \times 1 \pmod{5},  3 \times 2 \pmod{5}, 3 \times 3 \pmod{5}, 3 \times 4 \pmod{5} \}. 
$$
        
Multiplying the numbers in the sets, we get $4! \equiv 3^4 \times 4! \pmod 5$. 
Dividing both sides by $4!$, this gives $3^4 \equiv 1 \pmod 5$. 

Now for the general case. We have to show that if the numbers in the
set $S = \{ 1, \ldots, p-1 \}$ get multiplied by $a$ modulo $p$, the
resulting numbers are distinct and $\neq 0$. So let $i \neq j \in S$,
and consider $ai$ and $aj$. Suppose $ai \equiv aj \pmod p$. Then $i
\equiv j \pmod p$ and contradiction with $i \neq j$. So $ai$ and $aj$
are distinct modulo $p$. If $ai \equiv 0$ then, since $a$ and $p$ are
relatively prime, we can divide by $a$ and get $i \equiv 0$, and
contradiction. So the resulting numbers are $\neq 0$.  This means the
result of multiplying the numbers in $S$ with $a$ modulo $p$ is a
permutation of $S$.

This gives

$$     
  S = \{ 1, \ldots, p-1 \} = \{ a \times 1 \mod{p},  \ldots, a \times p-1 \mod{p} \}. 
$$
        
Multiplying the numbers left and right gives: 

$$            
  (p-1)! = a^{p-1} \times (p-1)! \pmod{p}. 
$$

We can divide both sides by $(p-1)!$ because $(p-1)!$ and $p$ are
relatively prime. This gives $a^{p-1} \equiv 1 \pmod{p}$.

**Fermat's Test for Primality**

  * Pick $a$ with $1 < a < N$ at random. 
  * Compute $a^{N-1} \pmod N$ using fast exponentiation. 
  * If the outcome is $1$, output "Prime", otherwise output "Composite".

Problem with this: if $N$ is indeed prime then $a^{N-1} \equiv 1 \pmod
N$, and the test works fine.

If $N$ is composite, it may still happen that $a^{N-1} \equiv 1 \pmod
N$, for Fermat's Little Theorem does not specify what happens for
composite numbers ...

In any case, here is an implementation, using fast exponentiation
(your function `exM`, but for now we replace the definition by
a fake definition):

> exM = expM  

> prime_test :: Integer -> IO Bool
> prime_test n = do 
>    a <- randomRIO (1, n-1) :: IO Integer
>    return (exM a (n-1) n == 1)

We can improve this by performing a sequence of $k$ tests: 

> prime :: Int -> Integer -> IO Bool
> prime k n = do
>  as <- sequence $ fmap (\_-> randomRIO (1,n-1)) [1..k]
>  return (all (\ a -> exM a (n-1) n == 1) as)

Unfortunately, there are numbers that fool this test, so called 
[Carmichael numbers](https://en.wikipedia.org/wiki/Carmichael_number).

---
                    
**Exercise 4**

Implement the algorithm of Rabin and Miller
[@Miller1976:rhatfp,@Rabin1980:paftp], which gives a version of
probabilistic primality testing that cannot by fooled by Carmichael
numbers. See [Rabin primality test](http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test).

---

**Exercise 5**

For testing our primality test algorithms the list of prime numbers
generated by Eratosthenes' sieve is useless, for the algorithms all
correctly classify the primes as primes. Where they can go wrong is on
classifying composite numbers; these can slip through the Fermat test,
and also through the Rabin/Miller test, although the probability of
this can be made arbitrarily small. Give an implementation of the
infinite list of composite numbers. The start is
         
    [4,6,8,9,10,12,14,15,16,18,20,21,22,24,25,26,27,28,30,32,33,34,35,36,38,39,40,42,

Use this to implement a test that finds *false positives* for
the Fermat test and for the Rabin/Miller test.     
        
---    
        
**Exercise 6**

Use the above to implement a trapdoor function: a bijection 

$$    
  f : {\mathbb Z}_p \to {\mathbb Z}_p
$$
        
for a large prime $p$ that is efficient to 
compute, but for which $f^{-1}$ is hard to compute. 

See [trapdoor function](http://en.wikipedia.org/wiki/Trapdoor_function)
and [@DiffieHellman1976:ndic]. You may assume that you have
a large prime available that you can keep secret. 

---

For the final exercise you will need modular division.
Modular division is the same as multiplication by 
modular inverse. Modular inverses only exist for 
numbers that are co-prime with their modulus. 

> invM :: Integer -> Integer -> Integer
> invM x n = let 
>    (u,v) = fct_gcd x n
>    copr  = x*u + v*n == 1
>    i     = if signum u == 1 then u else u + n  
>  in 
>    if copr then i else error "no inverse"

This uses the extended Euclidean algorithm for GCD: 

> fct_gcd :: Integer -> Integer -> (Integer,Integer) 
> fct_gcd a b = 
>   if b == 0 
>   then (1,0) 
>   else 
>      let 
>        (q,r) = quotRem a b
>        (s,t) = fct_gcd b r 
>      in (t, s - q*t)


In the Diffie-Hellman style trapdoor function
the secret prime has to be available, so we cannot make
the full recipe for constructing the encoding function publicly
available.  In public key cryptography, it turns out we can do much
better than this. We can generate a key and make it publicly
available, and from that key anyone can construct an encoding
function. Decoding, given only this public key information, is thought
to be hard. This is the basis for
[RSA public key cryptography](https://en.wikipedia.org/wiki/RSA_%28cryptosystem%29) [@RivShaAdl78:amfods].

---

**Exercise 7**

Implement RSA coding and decoding.

The type declarations you need are:

     rsa_public :: Integer -> Integer -> (Integer,Integer)

`rsa_public` takes a pair of large primes and generates a public key pair
of which the second element is the product of the primes.

     rsa_private ::  Integer -> Integer -> (Integer,Integer)

`rsa_private` should generates a private key pair from a pair of
large primes. Again, the second element of the output is the
product of the input primes.

     rsa_encode :: (Integer,Integer) -> Integer -> Integer

`rsa_encode` should use a public key pair to encode a message (represented
by an integer).     

RSA decoding using a private key is just a matter of using the private 
key to encode again, so we have:

      rsa_decode = rsa_encode                              

Explain why `rsa_encode` is a genuine trapdoor function.

---           
