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

6. Type and its corresponding Type constructor are of the same name.

# Computer Vision

## 1.[Stanford CS231n](https://www.youtube.com/playlist?list=PLf7L7Kg8_FNxHATtLwDceyh72QQL9pvpQ)

### lec 1

1. History of computer vision
  - Hubel & Wiesel, 1959
  - Block world, Larry Roberts, 1963
  - Vision, David Marr, 1970s
  - Stages of Visual Representation, David Marr, 1970s
  - Pictorial Structure, Fischler and Elschlager, 1973
  every object is composed of simple geometric primitives
  - Generalized Cylinder, Brooks & Binford, 1979
  - Razor edge detection, David Lowe, 1987
  - Normalized Cut, Shi & Malik, 1997
  Image segmentation
  - Face Detection, Viola & Jones, 2001
  - "SIFT" & Object Recognition, David Lowe, 1999
  - Spatial Pyramid Matching, Lazebnik, Schmid & Ponce, 2006
  - Histogram of oriented gradients (HoG), Dalal & Triggs, 2005
  - Deformable Part Model, Felzenswalb, McAllester, Ramanan, 2009
  - Pascal Visual Object Challenge (20 object categories), 2006-2012
  - [ImageNet (22K categories, 14M images)](www.image-net.org), 2009
  - Image Classification Challenge (1K object classes), Russakovsky et al. arXiv, 2014

2. Basic tasks
  - object detection
  - action classification / activity recognition
  - image captioning
    - semantic segmentation, perceptual grouping
    Image Retrieval using Scene Graph, Johnson et al., 2015
  - 3d reconstruction

3. CNN
  - LeCun et al., 1998
  - NEC-UIUC, Lin CVPR 2011
  - SuperVision, Krizhevsky NIPS 2012 (break through)
  - GoogLeNet, Szegedy arxiv 2014
  - VGG, Simonyan arxiv 2014
  - MSRA, 2015

### lec 2
1. classifier
2. training
3. K-NN
4. hyperparameter, validation set, n-fold cross validation
5. curse of dimensionality
6. L2 distance on pixel space not informative
7. linear classifier
8. nonlinearity

CIFAR10

### lec 3
1. loss function
2. SVM loss, L1/L2 regulization
3. softmax (logistic regression)
4. stochastic gradient descent

### lec 4
1. gradient calcuation for backpropagation, chain rule
2. vectorize matrix operations
