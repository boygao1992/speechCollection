# Science

## 1.[一勺思想圆桌会·定位中国](https://www.bilibili.com/video/av25338475)

Presenter: Lanfeng Yuan
- current researcher at National Laboratory for Physical Sciences at the Microscale
- postdoctoral chemistry in Princeton University

## 2.[Before the Big Bang 7: An Eternal Cyclic Universe, CCC revisited & Twistor Theory](https://www.youtube.com/watch?v=FVDJJVoTx7s)

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

## 2.[Morten Rand-Hendriksen: CSS Grid Changes Everything (About Web Layouts)](https://www.youtube.com/watch?v=txZq7Laz7_4)

## 3.[PS Unscripted - Code Reuse in PS: Fns, Classes, and Interpreters](https://www.youtube.com/watch?v=GlUcCPmH8wI)

A bottom-up way of explaining `Functor` and Type Classes.
Show their power of generalization by examples.
Also, the pain without compiler's auto dispatch.
Inconsistency is inevitable when manually injecting implementations especially facing "diamond problem" (multiple inheritance).

"Dependency injection" in FP.
> P.77

Independent reusable functionality in Type Classes.

Comparison of reusability by type signatures.

Abstract out assumptions by introducing wild card type signature.

- initial encoding (inject implementation, Factory?)
- final encoding

``` purescript
data RealCode a
  = ReadFile String (Maybe String -> a)
  | Done a
```

`a` abstracts the way you want to inspect the return type
e.g. `Identity`(debug), `Aff`(real implementation)

This operator-like data structure is a tree / `Free`.

`realCodeToAff`, interpreter, `CoFree`?

## 4.[Front-End Development with PureScript and Thermite](https://www.youtube.com/watch?v=-l2ySRCjihc&t=1233s)

A React wrapper for Purescript

[purescript-thermite](https://github.com/paf31/purescript-thermite)

Local state management following React model.
The denotational syntax for view/render function is pretty close to Elm.
The way it manipulates `EventHandler`s may be useful.
Use `coroutine` to handle IO effects.

```purescript
newtype Spec eff State Action = Spec
  { performAction :: PerformAction eff State Action
  , render        :: Render State Action
  }

type PerformAction eff State Action
  = Action
  -> State
  -> CoTransformer (Maybe a) (State -> State) (Aff eff) Unit
  
type Render State Action
  = (Action -> EventHandler)
  -> State
  -> Array ReactElement
  -> Array ReactElement

simpleSpec
  :: forall eff State Action
  . PerformAction eff State Action
  -> Render State Action
  -> Spec eff State Action
```

coroutine
``` purescript
type Co f m = FreeT f m

data Emit o a = Emit o a
type Producer o = Co (Emit o)

newtype Await i a = Await (i -> a)
type Consumer i = Co (Await i)
```

## 5.[Lambda Calculus for People Who Can’t Be Bothered to Learn It](https://www.youtube.com/watch?v=c_ReqkiyCXo)

[github](https://github.com/sjsyrek/presentations/tree/master/lambda-calculus)

#### definition 
- `<expression>  := <name> | <function> | <application>`
- `<function>    := λ <name>. <expression>`
- `<application> := <expression> <expression>`

> capture the nature of computation in the simplest possible way

examples
- `(λx. x)(λy. y)`,
function composition of two identity functions, `f ◦ g` where `f` and `g` are both `id :: a -> a`.

``` javascript
const func = x => (y => y)

const id = a => a
const f = id
const g = id
const fg = f(g)
```

#### reduction
- α-conversion/renaming, allows bound variable names to be changed
  - `(λa. a) ≡ (λz. z)`
- β-reduction, applying functions to their arguments
  - `(λx. λy. x y) p q → (λy. p y) q → p q`
  - `(λx. x x)(λx. x x) → (λx. x x)(λx. x x) → …` (recursion)
- η-conversion, captures a notion of extensionality
  - `(λx . f x) ⟷ f` (point-free for a unary function)

#### identity combinator (universal fixed-point combinator)
- `id ≡ λx. x` (I combinator)

#### boolean combinators
Church Encoding, represent data and operators by lambda functions

- `true ≡ (λx. λy. x)` (K combinator)
- `false ≡ (λx. λy. y)`
- `and ≡ (λa. λb. a b false)`
- `or  ≡ (λa. λb. a true b)`
- `not ≡ (λa. a false true)`

example
```
and true false
= (λa. λb. a b false) true false
= (λb. true b false) false
= (λb. (λx. λy. x) b false) false
= (λb. b) false
= false
```

Since `true` is a function (K combinator), we can have "non-boolean algebra"
```
true false false
= (λx. λy. x) false false
= false
```
This is actually how `and` operator and other operators work. 
Internally, it takes advantage of the "forgetting" pattern of K combinator (throwing away the 2nd argument) and its dual (throwing away the 1st argument).

`Bool` under `and` is a `Semigroup`(`Bool`, `and`)
```elm
type Bool 
    = True
    | False

append : Bool -> Bool -> Bool
append b1 b2 =
    case ( b1, b2 ) of
        ( True, True ) ->
            True
        _ ->
            False

(<>) : Bool -> Bool -> Bool
(<>) = append
```

#### numbers (Church numerals)
- `0 ≡ (λf. λx. x)` (`≡ (λx. λy. y) ≡ false`)
- `1 ≡ (λf. λx. f(x))`
- `2 ≡ (λf. λx. f(f(x)))`
- `3 ≡ (λf. λx. f(f(f(x))))`
- `4 ≡ (λf. λx. f(f(f(f(x)))))`
- `…`

#### enumeration
- `succ ≡ (λn. λf. λx. f(n f x))`
- `pred ≡ (λn. n (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)`

another definition: `PRED := λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)`

example
```
succ 1
= (λn. λf. λx. f(n f x)) 1
= (λf. λx. f(1 f x))
= (λf. λx. f((λf. λx. f(x)) f x))
= (λf. λx. f(f(x))
= 2
```

example
```
pred 1
= (λn. n (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false) 1
= (1 (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)
= ((λf. λx. f(x)) (λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)
= ((λp. λz. z(succ (p true))(p true))(λz. z 0 0) false)
= ((λz. z(succ ((λz. z 0 0) true))((λz. z 0 0) true)) false)
= ((λz. z(succ (true 0 0))(true 0 0)) false)
= ((λz. z(succ 0)(0)) false)
= ((λz. z(1)(0)) false)
= (false (1)(0)) 
= 0
```

```
PRED 1
= (λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) 1
= (λf.λx. 1 (λg.λh.h (g f)) (λu.x) (λu.u))
= (λf.λx. (λf. λx. f(x)) (λg.λh.h (g f)) (λu.x) (λu.u))
= (λf.λx. (λx. (λg.λh.h (g f)) (x)) (λu.x) (λu.u))
= (λf.λx. (λx. (λh.h (x f))) (λu.x) (λu.u))
= (λf.λx. (λh.h ((λu.x) f)) (λu.u))
= (λf.λx. (λu.u) ((λu.x) f))
= (λf.λx. (λu.x) f)
= (λf.λx. x)
= 0
```

#### predicates
- `isZero ≡ (λn. n false not false)`

example
```
isZero 0
= (λn. n false not false) 0
= 0 false not false
= (λf. λx. x) false not false
= not false
= true

isZero 1
= 1 false not false
= (λf. λx. f(x)) false not false
= false not false
= false
```

- `isTrue ≡ id`
- `isFalse = (λx. x false true)`

- `if ≡ (λp. λx. λy. p x y)`

example
```
if true
= (λp. λx. λy. p x y) true
= true x y
= x

if false
= (λp. λx. λy. p x y) false
= false x y
= y
```

in a different perspective, `true` and `false` are less generic `if` (obvious, since `if` has one additional bound variable)
- `true ≡ (λx. λy. x) ≡ if(true)`
- `false ≡ (λx. λy. y) ≡ if(false)`

```javascript
if (predicate) {
  // true
} {
  // false
}

if (true) {
  // always pick the first branch
} else {
}

if (false) {
} else {
  // always pick the second branch
}
```

#### arithmetic

- `add ≡ (λn. λm. λf. λx. n f(m f x))`
- `add ≡ (λn. λm. m succ n)`
- `sub ≡ (λm. λn. n pred m)`
- `mult ≡ (λn. λm. λf. n(m f))`
- `mult ≡ (λn. λm. m (add n) 0)`
- `exp ≡ (λx. λy. y x)`

#### recursion

- `fix ≡ (λy. (λx. y(x x))(λx. y(x x)))`
- `F ≡ (λf. λn. ((isZero n) 1 (mult n (f(pred n)))))`

#### factorial

- `fact ≡ (fix F)`

## 6.[Async Programming in Purescript - LA PureScript Meetup 12_05_17](https://www.youtube.com/watch?v=dbM72ap30TE)

## 7.[Thai Pangsakulyanont: Smells In React Apps - JSConf.Asia 2018](https://www.youtube.com/watch?v=xBa0_b-5XDw)


> Generic components

fully independent state for reusability

inject dependencies/constraints by decoration

> Information leaks
> "Does this really need to know about that?"

strive for minimal dependencies between components

> Cohesion loss
> Symptom: Making a simple change requires changing code that is further apart. 
> Related things should stay close together.
> aiming for functional cohesion instead of logical cohesion

code organization, especially view functions
- logical cohesion: similarity in locality (in DOM)
- functional cohesion: similarity in source of information

## 8. Elm Europe 2017

### 1. [Elm Europe 2017 - Matthew Griffith - Understanding style](https://www.youtube.com/watch?v=NYb2GDWMIm0)

### 2. [Elm Europe 2017 - Jakub Hampl - Visualizing data with elm](https://www.youtube.com/watch?v=Pf1xQ76JgmQ)

### 3. ["Infecting the visualization design process with the elm philosophy" by Alexander Kachkaev](https://www.youtube.com/watch?v=K-yoLxnm95A)

### 4. [Elm Europe 2017 - Andrey Kuzmin - Bringing the fun to graphics programming](https://www.youtube.com/watch?v=Z-6ETEBNlMs)

WebGL

### 5. ["Lessons from 100k LOC elm at Futurespace" by Mark Skipper and Decio Ferreira](https://www.youtube.com/watch?v=0AosqGTEa0Q)

### 6. [Elm Europe 2017 - Richard Feldman - Scaling Elm Apps](https://www.youtube.com/watch?v=DoA4Txr4GUs&t=986s)

> purpose of update function: routes messages to logic
> "recommend sharing code with helper functions instead of recursively calling the update function"
> "make sure the only way the update function gets called is by the runtime, not by us"

> file system analogy to elm state tree
> "data without attached methods" -> not OOP -> no parent-child communication
wrong, if the state is organized as the view (DOM tree), the parent-child relationship in view is inherited by the state which further forces the update function to structure in the same nested way.
Using "detached methods" doesn't means not OOP.
Actually, there is an isomorphism between "attached" (OOP) and "detached" (FP) style.
The approach the lecturer presented by returning additional value to the caller (higher-order update function) is exactly backward (child-parent) propagation of message.


### 7.[Elm Europe 2017 - Greg Ziegan - Building Reorderable UI in Elm](https://www.youtube.com/watch?v=UiLGIQUGFQg)

## 9. Kevin Yank on Elm
[Elm in Production: Surprises & Pain Points](https://www.youtube.com/watch?v=LZj_1qVURL0)

> the DOM is off-limits
`FFI` by `port` to utilized native JS libraries

> selective Event Handling
filter `KeyCode` on parent component with a customized `Attribute` with `onWithOptions` provided `preventDefault = True` to prevent default browser behaviors for a targeted subset of primitive events

> CSS Modules
- wrap the Elm app in a React Component and inject the CSS modules as part of the payload.
- define corresponding data model for the imported css modules and write a JSON `Decoder` for it

a bit of boilerplate here

[Elm at Scale: More Surprises, More Pain Points](https://www.youtube.com/watch?v=uQjivmLan0E)

> CSS Modules
[cultureamp/elm-css-modules-loader - Reference CSS modules in Elm source files with Webpack](https://github.com/cultureamp/elm-css-modules-loader)

> compiling time
> will be solve by 0.19 release

> large SPAs
[rtfeldman/elm-spa-example](https://github.com/rtfeldman/elm-spa-example)

> missing features will be delivered in 0.19
> - code splitting
> - lazy loading
> - tree shaking
> - server-side rendering

> team adoption: from 10% to 50%
> start with embedding Elm app in React and gradually ship more things in Elm

> animate to auto
> cannot ask for `.scrollHeight` in Elm
> custom event handler hack by `jsEventDecoder`
> treating the Event Object as a JSON data so then able to fetch the rendered view properties

> ```elm
> viewToggleButton =
>   button
>     [
>       class .toggle,
>       on "click" (
>         Json.map
>           Expand -- Msg Constructor
>           (
>               Json.at 
>                 [ "currentTarget"
>                 , "parentNode"
>                 , "firstChild"
>                 , "lastChild"
>                 , "scrollHeight"
>                 ]
>                 Json.int
>           )
>       )
>     ]
>     [ text "Show more"]
> ```

## 10. [The API Design Session video w/ Evan Czaplicki (@evancz)](https://www.youtube.com/watch?v=KSuCYUqY058)

do not buffer and synchronize state variables like the way dealing with external state in the DOM

take the state variables of interest as arguments

expose a set of public messages for component users to handle in their app

let users inject configurations to modify the view function and state handling logic

let users inject translator to translate messages defined in this component to their messages

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

# Math

## 1.[John Conway: Surreal Numbers - How playing games led to more numbers than anybody ever thought of](https://www.youtube.com/watch?v=1eAmxgINXrE)

> Surreal number
> Combinatorial game theory
> Impartial games, Partisan game
> Transfinite number
> Dedekind cut, Conway cut

`{|} = 0`
`{0|}= -1, {|0}= 1`
`{-1,0|} = -2, {|0,1} = 2`
`{0|1} = 1/2`

## 2. Category Theory III - Bartosz Milewski

### 1.1 [Overview part 1](https://www.youtube.com/watch?v=F5uEpKwHqdk)

> Hunter and gathering era, no necessity for number system
> distinguishing "few" (one) and "many" (two) is enough
> Geometry = Geo- (Earth) -Metry (Measure)
> rooted in agriculture, people started to have personal belonging

> `Functor` <-> Expression
`map :: Functor m => (a -> b) -> m a -> m b`
> `Monad` <-> Evaluation (substitute Variables by Expressions)
`bind :: Monad m => (a -> m b) -> m a -> m b`

> Free algebra

> `Free Monoid` is the most general construct that satisfies `Monoid` laws
> `List` is a `Free Monoid`, so you don't need to enforce `Monoid` laws on it when constructing one

### 1.2 [Overview part 2](https://www.youtube.com/watch?v=CfoaY2Ybf8M)

### 2.1 [String Diagrams part 1](https://www.youtube.com/watch?v=eOdBTqY3-Og)
> Poincaré duality

### 2.2 [String Diagrams part 2](https://www.youtube.com/watch?v=lqq9IFSPp7Q)

## 3. Math History - NJ Wildberger

### 16.[Differential Geometry | Math History | NJ Wildberger](https://www.youtube.com/watch?v=6xgtMQ7WSzQ)

> Planar curves

> involute, Huygens, unwrap a string tightly
> all involutes are disjoint and perpendicular to tangents

> envolute, at any given point on a curve, take another two points in the close neighbor to form a circle (osculating circle), as both points approaching the given point, the trajectory of the center of the circle (locus of centers) is the evolute
> the stablized circle (two points infinitely close to the given point), the product inverse of its radius is the curvature at that given point

> the evolute of an involute is the original curve

> curvature formula, Huygens/Newton

> catenary (cosh) = evolute of tractrix
> tractrix = involute of a catenary

> parabola
> semicubical parabola = evolute of parabola

> curves in 3D space
> osculating circle (2D) -> osculating plane (3D), normal vector

> surfaces in 3D space
> a point on the surface, tangent plane, normal vector
> a plane rotates around a normal vector intersecting with the surface to get a set of closed curves (planar cross-section curves)
> their curvatues are bounded, max & min => principle curvatues of the surface at that given point

> Theorema Egregium, Gauss
> the product of principle curvatues is determinable from the surface itself
> (direct measurement on the surface without embedding it in 3D)

> sphere, constant curvature
> psudosphere, tractrix revolved, constant curvature

### 17.[Topology | Math History | NJ Wildberger](https://www.youtube.com/watch?v=aaXk23JHFck)

> topology 
> study properties invariant to continuous deformation

> 2D surface
> Euler characteristics

> Descartes, curvature of polyhedra
> total curvature of a polyhedron = Euler characteristics

> Genus

> Riemann, topological analysis of Complex functions
> Riemann sphere (2-sphere, S^2) is homeomorphic to the Complex plane union with { Infinity }
> `f(z) = z^2`
> `f(z) = \sqrt(z)`, 2 full turns, gluing two 2-spheres with one hole on each (2 jumps to match the sign)
> `f(z) = \sqrt(z(z-a)(z-b))`, 2 full turns, gluing two 2-spheres with two holes on each (4 jumps)

> 2-sphere is the only simply connected 2-dimensional surface.
> simply connected: any close loop on the surface can contract to a point.

> 3D surface
> Poincare Conjecture, 3-sphere is the only simply connected 3-dimensional surface.


## 4. MathFoundations - NJ Wildberger

### 252.[A brief history of logic: Stoics and other thinkers](https://www.youtube.com/watch?v=-8IVhNrwNdg)

> Stocism: a school of Greek philosophy founded by Zeno of Citium (334-262 BCE)
> - a way of living: a formal philosophy, often associated with asceticism, Spartan life style, persevere through difficult time maintaining an equal balance
> - a way of thinking
>   - Logic: rhetoric, grammar, thought
>   - (meta-)Physics: science, universe
>   - Ethics: the way to obtain happiness was through peace of mind by living in sync with / in accordance with the law of the nature and striving to be virtuous

> Zeno's logic
> its development of an alternative way of thinking of logic
> distinct from Aristotle's point of view
> almost forgotten until recently we found out they had their finger on some actually quite modern ideas

> influenced by the Megarians:
> - (four-step ascendance to be a wise person) perception => assent => comprehension => knowledge
> Chrysippus (279-206 BCE), a leader of the Stoic school and highly influential and regarded logician in the Greek times, founded a propositional logic

> Aristotle's logic:
> All A (subject) are B (predicate)
> A,B are terms of Aristotle sentences
> closely associated with grammatical construction in a sentence

> Stoic logic:
> logical connectives
> - implication: if p then q
> - conjunction: both p and q
> - disjunction: either p or q (xor in modern formalism)

> The five indemonstrable forms
> 1. `if p then q`; `p`; therefore `q` (Modus Ponens)

```
p => q
T T  T (*) (*)
T F  F
F T  T (*)
F T  F (*)
```

If `p => q` is true, then there are three possibilities, marked above.
If `p` is true, given `p => q` is true, then the only possibility is `q` is true.

> 2. `if p then q`; `not q`; therefore `not p` (Modus Tollens)

```
p => q
T T  T (*)
T F  F
F T  T (*)
F T  F (*) (*)
```

> 3. `not (p and q)`; `p`; therefore `not q`
> 4. `either p or q`; `p`; therefore `not q`

```
p xor q
T  F  T 
T  T  F (*) (*)
F  T  T (*)
F  F  F
```

> 5. `either p or q`; `not p`; therefore `q`

```
p xor q
T  F  T 
T  T  F (*)
F  T  T (*) (*)
F  F  F
```

> **The law of excluded middle**
> Is the statement `p or not p` necessarily true?
> Aristotle: Yes
> Stoics: determinism (limited "free will")
```
p | not p
T   F
F   T
```

```
p xor q
T  F  T 
T  T  F (*)
F  T  T (*)
F  F  F
```

> The temporal aspect of "truth"
> truth value varies over time

> The nature of implication `if p then q`
> The Stoic and modern interpretations agree.
```
p => q
T T  T 
T F  F
F T  T 
F T  F
```

> The Megarian school of philosophy
> founded by Euclides of Megara, followed by Ichthyas and Stilpo (4th century BCE)
> Diodorus: what is possible is limited to what is, or will be true in the future.

# Politics

## 1.[Kishore Mahbubani and Weiwei Zhang - new world order](https://www.bilibili.com/video/av27093393)
