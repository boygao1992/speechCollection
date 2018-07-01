# Science

## 1.[一勺思想圆桌会·定位中国](https://www.bilibili.com/video/av25338475?from=search&seid=15132341747733330668)

Presenter: Lanfeng Yuan
- current researcher at National Laboratory for Physical Sciences at the Microscale
- postdoctoral chemistry in Princeton University

# Programming

## 1.[Profunctor Optics: The Categorical Approach - Bartosz Milewski](https://www.youtube.com/watch?v=l1FCXUi6Vlw)

```haskell
class Profunctor p where
  dimap :: (b <- a) -> (c -> d) -> (p b c -> p a d)
```

`Profunctor` is a `Functor` from `C_op x C -> Set`

"primitive category" (programming language primitive types: Boolean, Int, Float, Char, String)

In the "primitive category", arrows between two primitive types are `Function`s, which can also be denoted as an ordered pair of primitive types with function constraint enforced.

```haskell
Profunctor p => Arrow p where
  dimap :: (a -> b) -> (c -> d) -> (p b c -> p a d)
                                ((b -> c) -> (a -> d))
```

```haskell
type Lens s t a b = forall p. Strong p => p a b -> p s t
```

### Yoneda Lemma

```haskell
Yo f a ~ f a

forall x. (a -> x) -> f x ~ fa
```
where
```haskell
type Reader a x = a -> x

type Yo f a = Functor f => Reader a ~> f
```

(given `a`, `Reader a x` is a `Functor` on `x`)

### Yoneda Embedding

```haskell
forall x. (a -> x) -> (b -> x) ~> (b -> a)
```

## 2.[parallel and concurrent programming in haskell](https://www.youtube.com/watch?v=N6sOMGYsvFA&list=PLbgaMIhjbmEm_51-HWv9BQUXcmHYtl4sw)

### 1.1 
De-imperative, Why functional

### 1.2
1. GHCi basic commands
| command | functionality |
|---------|---------------|
| `:t`    | expression    |
| `:i`    | info          |
| `:l`    | filename      |
| `:r`    | reload        |
| `:q`    | quit          |

2. type system essentials

3. [operators' precedence and associativity](https://www.haskell.org/onlinereport/decls.html#prelude-fixities)

| Precedence | Left-associative   | Non-associative             | Right-associative |
|------------|--------------------|-----------------------------|-------------------|
|         10 |                    |                             | ` `               |
|          9 | `!!`               |                             | `.`               |
|          8 |                    |                             | `^`,`^^`,`**`     |
|          7 | `*`,`/`,`div`      |                             |                   |
|            | `mod`,`rem`,`quot` |                             |                   |
|          6 | `+`,`-`            |                             |                   |
|          5 |                    |                             | `:`,`++`          |
|          4 |                    | `==`,`/=`,`<`,`<=`,`>`,`>=` |                   |
|            |                    | `elem`,`notElem`            |                   |
|          3 |                    |                             | `&&`              |
|          2 |                    |                             | `||`              |
|          1 | `>>`,`>>=`         |                             |                   |
|          0 |                    |                             | `$`,`$!`,`seq`    |

4. primitive `IO`
```haskell
main :: IO ()

print :: Show a => a -> IO ()

putStrLn :: String -> IO ()
```

### 2.1
1. `$` to save parenthesis
```haskell
print(sqDist (3,4) + (0,1))

print $ sqDist $ (3,4) + (0,1)
```

In F# or Elm
```elm
print <| sqDist <| (3,4) + (0,1)

(3,4) + (0,1) |> sqDist |> print
```

2. function composition by `.`

3. `id`, polymorphism (not ad-hoc polymorphism in other languages), a formula works for **every** Type including `Void`

```haskell
id :: a -> a
id x = x
```

Like in OOP language, if define a new class/type, it automatically has this `id` method attached.

4. simplest pattern matching: pair

5. currying (product type, sequentially ordered)

### 2.2

1. point-free, canceling the right most applied argument

Partial application of an operator is called *operator section*.
Left and right sections may be different for non-symmetric (non-commutative) operators.
```haskell
inc x = x + 1 = 1 + x = (+) 1 x
inc = (+ 1) = (1 +) = (+) 1

-- |Vector multiplication is non-commutative.
vectorMultiplication v2 = v1 * v2 = (*) v1 v2
vectorMultiplication = (v1 *) = (*) v1
```

2. Type in programming languages is a `Set` of values

value: a unique symbol which is distinct from "others"

3. `Void`, empty set, no type constructor (cannot hold value/symbol)

4. Bottom, a calculation that never ends / infinite calculation (for non-strict/lazy evaluation), is a special element in every Type, so even `Void` is not strictly empty

5. `Unit`

# Computer Vision

## 1.[Stanford CS231n](https://www.youtube.com/playlist?list=PLf7L7Kg8_FNxHATtLwDceyh72QQL9pvpQ)
