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


### De-Assumption Process

```purescript
sayHello :: List String -> List String
sayHello [] = []
sayHello (name : names) =
  ("Hello, " <> name) : sayHello names

{- step1: parametrize Values by Type -}
-- "Hello, " :: String
prependAll :: String -> List String -> List String
prependAll [] = []
prependAll prefix (name : names) =
  (prefix <> name) : prependAll names

sayHello = prependAll "Hello, "
sayGoodbye = prependAll "Goodbye, "

{- step2: parametrize Function by Function Type -}
-- given (name :: String), (prefix <> name) :: String -> String
transform :: (String -> String) -> List String -> List String
transform editFunc [] = []
transform editFunc (name : names) =
  (editFunc name) : transform editFunc names
  
prepend :: String -> String
prepend prefix = prefix <> _
prependAll :: String -> List String -> List String
prependAll prefix = transform (prepend prefix)

append :: String -> String
append suffix = _ <> suffix
appendAll :: String -> List String -> List String
appendAll suffix = transform (append suffix)

{- step3: parametrize Type by Kind -}
transform :: forall a. (a -> a) -> List a -> List a
transform endoFunction [] = []
transform endoFunction (x : xs) =
  (endoFunction x) : transform endoFunction xs

prependAll :: String -> List String -> List String
appendAll :: String -> List String -> List String
add :: Int -> Int
add num = _ + num
addAll :: Int -> List Int -> List Int
addAll num = transform (add num)
negateAll :: List Boolean -> List Boolean
negateAll = transform not -- not :: Boolean -> Boolean

{- step4: remove unnecessary equality constraints on Kind -}
transform :: forall a b. (a -> b) -> List a -> List b
transform f [] = []
transform f (x : xs) =
  (f x) : transform f xs

formatAll :: List Number -> List String
formatAll = transform Number.toString -- toString :: Number -> String

{- step5: parametrize Type (Constructor) Functions by Arrow Kind -}
transform :: forall f a b. (a -> b) -> f a -> f b
transform f ??? = ??? -- no information about the structure of `f` and how to extract Values from `f`, thus no implementation can be derived

-- define a Type Function
-- Transform :: ( (Type -> Type), Type, Type ) -> Type
type Transform f a b =
  (a -> b) -> f a -> f b
  
-- for each `f`, need a separate implementation
transformList :: forall a b. Transform List a b
transformList f [] = []
transformList f (x : xs) =
  (f x) : transformList f xs

transformArray :: forall a b. Transform Array a b
transformTree :: forall a b. Transform Tree a b

-- for each `f`, need to manually inject the corresponding implementation
addAll :: forall f. Transform f Int Int -> Int -> f Int -> f Int
addAll transform num = transform (add num)
negateAll :: forall f. Transform f Boolean Boolean -> f Boolean -> f Boolean
negateAll = transform not
formatAll :: forall f. Transform f Number String -> f Number -> f String
formatAll transform = transform Number.toString


{- step6: reduce Type dependency to minimum by Higher-order Type Functions (that return Type Functions) -}

-- automatic currying for Type Functions is missing in purescript's type system

-- define a Higher-order Type Function
-- Transform :: (Type -> Type) -> ( (Type, Type) -> Type )
type Transform f =
  forall a b. (a -> b) -> f a -> f b -- actual Type of a and b can be derived from the injected function (:: a -> b)
  
transformList :: Transform List
transformArray :: Transform Array
transformTree :: Transform Tree

addAll :: forall f. Transform f -> Int -> f Int -> f Int
negateAll :: forall f. Transform f -> f Boolean -> f Boolean
formatAll :: forall f. Transform f -> f Number -> f String
formatAll transform fa = transform Number.toString fa
-- actual Type Constructor Function of f can be derived from the injected data (fa :: f Number, e.g. List Number, Tree Number)
-- but this polymorphic function requires at least manually injecting the Type Constructor Function of f

-- Before
sayHello :: List String -> List String
-- After
sayHello :: Transform List -> List String -> List String

{- step7: Type Class to rescue -}

class Transform f where
  transform :: forall a b. (a -> b) -> f a -> f b
  
instance transformList :: Transform List where
  transform f [] = []
  transform f (x : xs) =
    (f x) : transform f xs

instance transformArray :: Transform Array where ...
instance transformTree :: Transform Tree where ...

-- The compiler maintains a dictionary of all these implementations and automatically injects the correct implementation when a unique Type Constructor Function of f can be inferred

sayHello :: List String -> List String -- f = List
sayHello = transform ("Hello ," <> _)
negateAll :: forall f. Transform f => f Boolean -> f Boolean
negateAll = transform not
formatAll :: forall f. Transform f => f Number -> f String
formatAll = transform Number.toString

-- well-known official name: Functor
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b
  
```

```purescript
flubble :: forall f. Traversable t => Applicative t => f String -> f String
flubble traversableDict applicativeDict xs =
  applicativeDict.pure
  applicativeDict."Apply".apply
  applicativeDict."Apply"."Functor".map -- implementation of map 1
  traversableDict.traverse
  traversableDict.sequence
  traversableDict."Functor".map -- implementation of map 2
  traversableDict."Foldable".foldr
  traversableDict."Foldable".foldl
  traversableDict."Foldable".foldMap
-- compiler enforces cohesion among instances in the same Type Class hierarchy: map 1 = map 2
```

```purescript
class Monad m <= MonadCache m where
  read :: String -> m (Maybe String)
  write :: String -> m Unit

class Monad m <= MonadRequest req m | m -> req where -- multiple Type variables while the *Type* of req is the same as the Type variable of the Type Function of m (*Type* -> Type), thus can be derived automatically by specifying a functional dependency `m -> req`
  request :: req -> m String

requestWithCache ::
  forall m req.
  MonadCache m =>
  MonadRequest req m =>
  String ->
  req ->
  m String
requestWithCache key req = do
  cached <- read key
  case cached of
    Nothing -> do
      content <- request req
      write key content
      pure content
    
    Just content ->
      pure content

instance monadCacheAppMonad :: MonadCache AppMonad where ...
instance monadRequestAppMonad :: MonadRequest AppMonad where ...
instance monadCacheTestMonad :: MonadCache TestMonad where ...
instance monadRequestTestMonad :: MonadRequest TestMonad where ...

realCode ::
  forall m.
  MonadCache m =>
  MonadRequest String m =>
  m String
realCode = requestWithCache "/cache" "https://purescript.org"

realCodeApp :: AppMonad String
realCodeApp = realCode

realCodeTest :: TestMonad String
realCodeTest = realCode
```

```purescript
realCode :: Aff String
realCode = do
  cached <- FS.readFile "/cache"
  case cached of
    Nothing -> do
      content <- HTTP.get "https://purescript.org"
      FS.writeFile "/cache" content
      pure content
    
    Just content ->
      pure content

data RealCode a
  = ReadFile String (Maybe String -> RealCode a)
  | WriteFile String String (Unit -> RealCode a)
  | HttpGet String (String -> RealCode a)
  | Done a
  
realCode :: RealCode String
realCode =
  ReadFile "/cache" \cached ->
    case cached of
      Nothing ->
        HttpGet "https://purescript.org" \content ->
          WriteFile "/cache" content \_ ->
            Done content
        
      Just content ->
        Done content
        
realCodeToAff :: forall a. RealCode a -> Aff a
realCodeToAff = case _ of
  ReadFile path next -> do
    content <- FS.readFile path
    realCodeToAff (next content)
    
  WriteFile path content next -> do
    FS.writeFile path content
    realCodeToAff (next unit)
    
  HttpGet url next -> do
    content <- HTTP.get url
    realCodeToAff (next content)
  
  Done result ->
    pure result
    
realCodeToIdentity :: forall a. RealCode a -> Identity a
realCodeToIdentity = case _ of
  ReadFile path next ->
    realCodeToIdentity (next (Just "Test content"))
  
  WriteFile path content next ->
    realCodeToIdentity (next unit)
  
  HttpGet url next ->
    realCodeToIdentity (next "<html><title>Purescript.org</title></html>")
    
  Done result ->
    pure result
```

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

## 11. [A Categorical View of Computational Effects](https://www.youtube.com/watch?v=6t6bsWVOIzs)

> T-computations
> - list(X) := finite lists of elements of X
> - partial(X) := X + {err/bottom}
> - state(X) := S -> (S,X)
> - continuation(X) := (X -> R) -> R
> - non-det(X) := P(X), the set of all subsets of X (non-determinant)
> - prob-dist(X) := the set of probability functions X -p-> [0,1] so that $\sum_{x \in X} p(x) = 1$ (probabilistic distribution)

> a T-program from A to B is a function A -f-> T(B), from the set of values of type A to the set of T-computations of type B.


## 12. [The Actor Model - Carl Hewitt](https://www.youtube.com/watch?v=7erJ1DV_Tlo)

> Actor is the minimum primitive unit that embodies all 3 essential elements of computation :
> - processing
> - storage
state
> - communication
protocol, encryption/guard

> when an actor receive a message, it can
> 1. create some more actors

like Promise in JS

any computation happening (resolve) or not (exception/reject) in the future
- a producer which executes this computation
- a receiver which once receives the message from the producer, will inform all the other actors that subscribed to this receiver by sending a message
(the receiver stores all the "subscriber" actors' addresses)


> 2. send new messages to actors whose addresses it knows
> 3. designate how it's gonna handle the next message it receives

pass any modified state to the recursive call of itself so the "mutated" state comes into play from the next message

> continuation passing:
> von normann machine
> lambda expression (callback function)
> single-threaded (sequential)

> channels:
> two-phase commit protocol
> e.g. process calculi
> you can implement a channel by an actor with PUT and GET message handling
> whose state is the buffer

> conceptually, actors process one message at a time
> optimization in implementation: pipeline messages and process them batch by batch

> protection over the address space
> - within machine: integrity of addresses is maintained by COR
> - between machines: guarded by encryption

[Capability-based addressing](https://en.wikipedia.org/wiki/Capability-based_addressing)

[Best-effort delivery](https://en.wikipedia.org/wiki/Best-effort_delivery)
> Best-effort delivery describes a network service in which the network does not provide any guarantee that data is delivered or that delivery meets any quality of service.

> Cross-machine communication through actors entails best-effort delivery.
> Best you can do: persist the message at the sender actor, if no response after a predefined time window, resend the message.

> nondeterminism vs indeterminism
> nondeterministic turing machine: multiple outgoing edges from the same state with the same name/event
>   - bounded: the number of steps to the terminal state is bounded
>   - unbounded: otherwise
the uncertainty comes from the system itself / the inside
which is algorithmically described
> indeterminism
the uncertainty comes from the outside
which cannot be algorithmically described
like an oracle

Axion of Choice vs Algorithmic methodology in mathematics

[Actor model](https://en.wikipedia.org/wiki/Actor_model)
> ## Unbounded nondeterminism controversy

>The first models of computation (e.g., Turing machines, Post productions, the lambda calculus, etc.) were based on mathematics and made use of a global state to represent a computational step (later generalized in [McCarthy and Hayes 1969] and [Dijkstra 1976] see Event orderings versus global state).
> Each computational step was from one global state of the computation to the next global state. 

> Edsger Dijkstra further developed the nondeterministic global state approach.

> Hewitt argued otherwise: there is no bound that can be placed on how long it takes a computational circuit called an arbiter to settle (see metastability in electronics).

[Arbiter](https://en.wikipedia.org/wiki/Arbiter_(electronics))
> Arbiters are electronic devices that allocate access to shared resources. 

> The actor model features unbounded nondeterminism which was captured in a mathematical model by Will Clinger using domain theory.

> ## Direct communication and asynchrony

> there is no requirement for a synchronous handshake with the recipient. 

> ## Actor creation plus addresses in messages means variable topology

> For example, an Actor might need to send a message to a recipient Actor from which it later expects to receive a response, but the response will actually be handled by a third Actor component that has been configured to receive and handle the response (for example, a different Actor implementing the Observer pattern).
> The original Actor could accomplish this by sending a communication that includes the message it wishes to send, along with the address of the third Actor that will handle the response.
> This third Actor that will handle the response is called the resumption (sometimes also called a continuation or stack frame).
> When the recipient Actor is ready to send a response, it sends the response message to the resumption Actor address that was included in the original communication. 
stated above about implementation of Promise in Actor-based system

> ## Inherently concurrent

> ## No requirement on order of message arrival

> ## Locality

> - in processing a message, an Actor can send messages only to addresses that 
>   - it receives in the message, 
>   - it already had before it received the message, 
>   - it creates while processing the message.
> - there is no simultaneous change in multiple locations.

> In this way it differs from some other models of concurrency, e.g., the **Petri net** model in which tokens are simultaneously removed from multiple locations and placed in other locations. 

> ## Composing Actor systems

## 13. [Scalable Inconsistency Robust Information Systems - Carl Hewitt](https://www.youtube.com/watch?v=_R65RrishcY)

## 14. [Designing Fluid Interfaces](https://www.youtube.com/watch?v=gttSJA-kDmQ)

[Building Fluid Interfaces - Medium](https://medium.com/@nathangitter/building-fluid-interfaces-ios-swift-9732bb934bf5)

[Designing Fluid Interfaces - Mr Why blog](https://mr-why.com/design/designing-fluid-interfaces)

## 15. Recursion Scheme

### 1. [Going banana with recursion schemes for fixed point data-types](https://www.youtube.com/watch?v=I-5yvVp74Vk)

> Recursive structures: inductively defined data-types
```purescript
data List a
  = Cons a (List a)
  | Nil
  
data BTree a
  = Node a (BTree a) (BTree a)
  | Leaf a
```
> e.g. file systems, 3d graphics (scene graph), databases

```haskell
data Exp
  = IntVal Int
  | DecVal Double
  | Sum Exp Exp
  | Multiply Exp Exp
  | Divide Exp Exp
  | Square Exp
```

```scala
sealed trait Exp
final case class IntVal(v: Int) extends Exp
final case class DecVal(v: Double) extends Exp
final case class Sum(exp1: Exp, exp2: Exp) extends Exp
final case class Multiply(exp1: Exp, exp2: Exp) extends Exp
final case class Divide(exp1: Exp, exp2: Exp) extends Exp
final case class Square(exp: Exp) extends Exp

val evaluate: Exp => Double = {
  case IntVal(v) => v.toDouble
  case DecVal(v) => v
  case Sum(exp1, exp2) => evaluate(exp1) + evaluate(exp2)
  case Multiply(exp1, exp2) => evaluate(exp1) * evaluate(exp2)
  case Square(exp)
    =>
      val v = evaluate(exp)
      v * v
  case Divide(exp1, exp2) => evaluate(exp1) / evaluate(exp2)
}
```

> separation of concern:
> 1. recursively traversing the structure
> 2. evaluation at nodes
```
sealed trait Exp[A]
final case class IntVal[A](v: Int) extends Exp[A]
final case class DecVal[A](v: Double) extends Exp[A]
final case class Sum[A](exp1: A, exp2: A) extends Exp[A]
final case class Multiply[A](exp1: A, exp2: A) extends Exp[A]
final case class Divide[A](exp1: A, exp2: A) extends Exp[A]
final case class Square[A](exp: A) extends Exp[A]

val evaluate: Exp[Double] => Double = {
  case IntVal(v) => v.toDouble
  case DecVal(v) => v
  case Sum(exp1, exp2) => exp1 + exp2
  case Multiply(exp1, exp2) => exp1 * exp2
  case Square(exp1, exp2) => exp * exp
  case Divide(exp1, exp2) => exp1 / exp2
}

// 10 + 5
val exp1: Exp[Exp[Unit]] =
  Sum[Exp[Unit]]
    ( InvVal[Unit](10)
    , IntVal[Unit](5)
    )

// 5.2 / (10 + 5)
val exp2: Exp[Exp[Exp[Unit]]] =
  Divide[Exp[Exp[Unit]]]
    ( DecVal[Exp[Unit]](5,2)
    , Sum[Exp[Unit]]
        ( InvVal[Unit](10)
        , IntVal[Unit](5)
        )
    )
```

> fix point data-types
```haskell
newtype Mu f = Mu { fold :: f (Mu f) }
```
```scala
case class Fix[F[_]](unFix: F[Fix[F]])

val exp1: Fix[Exp] =
  Fix( Sum[Fix[Exp]]
        ( Fix(IntVal[Fix[Exp]](10))
        , Fix(IntVal[Fix[Exp]](5))
        ))
  
val exp2: Fix[Exp] =
  Fxp(Divide[Fix[Exp]]
        ( Fix(DecVal[Fix[Exp]](5.2))
        , Fix(Sum[Fix[Exp]]
                ( Fix(IntVal[Fix[Exp]](10))
                , Fix(IntVal[Fix[Exp]](5))
                ))
        ))
```

> Catamorphism

> need to derive Functor instance for the data-type
```scala
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

implicit val functor: Functor[Exp] = new Functor[Exp] {
  def map[A,B](exp: Exp[A])(f: A => B): Exp[B] = exp match {
    case Sum(a1, a2) => Sum(f(a1), f(a2))
    case Multiply(a1, a2) => Multiply(f(a1), f(a2))
    case Divide(a1, a2) => Divide(f(a1), f(a2))
    case Square(a) => Square(f(a))
    case IntVal(v) => IntVal(v)
    case DecVal(v) => DecVal(v)
  }
}
```

> F-algebra
```scala
type Algebra[F[_], A] = F[A] => A

val evaluate: Algebra[Exp, Double] = { // Exp[Double] => Double
  case IntVal(v) => v.toDouble
  case DecVal(v) => v
  case Sum(a1, a2) => a1 + a2
  case Multiply(a1, a2) => a1 * a2
  case Square(a) => a * a
  case Divide(a1, a2) => a1 / a2
}

exp2.cata(evaluate)
```

```scala
val optimize: Algebra[Exp, Fix[Exp]] = { // Exp[Fix[Exp]] => Fix[Exp]
  case Multiply(Fix(a1), Fix(a2))
    if (a1 == a2) => Fix(Square(Fix(a1))) // How to derive (==) for Exp?
  case other => Fix(other)
}

val aTimesAExp: Fix[Exp] =
  Fix(Multiply
    ( Fix(Sum
      ( Fix(IntVal(10))
      , Fix(IntVal(20))
      ))
    , Fix(Sum
      ( Fix(IntVal(10))
      , Fix(IntVal(20))
      ))
    ))

aTimesAExp.cata(optimize)
/*
  Fix(Square(
    Fix(Sum
      ( Fix(IntVal(10))
      , Fix(IntVal(20))
      ))
  ))
 */
```

> Anamorphism: dual of Catamorphism
> constructs a structure from a value
> `Algebra[F[_], A]` vs `Coalgebra[F[_], A]`
```scala
type Coalgebra[F[_], A] = A => F[A]

// factorize a Int into multiples of 2
val divisors: Coalgebra[Exp, Int] = { // Int => Exp[Int]
  case n if (n % 2 == 0 && n != 2) => Multiply(2, n / 2)
  case n => IntVal(n)
}

12.ana[Fix, Exp](divisors)
/*
  Fix(Multiply
    ( Fix(IntVal(2))
    , Fix(Multiply
      ( Fix(IntVal(2))
      , Fix(IntVal(3))
      ))
    ))
 */
```

> Hylomorphism
> constructs from a value and then deconstructs the structure into a value
> Anamorphism followed by Catamorphism
> evaluated in a single pass

```scala
val divisors: Coalgebra[Exp, Int] = { ... } // Int => Exp[Int]
val evaluate: Algebra[Exp, Double] = { ... } // Exp[Double] => Double

n.hylo(evaluate, divisors)
```

### 2.[Recursion Schemes - London Haskell](https://www.youtube.com/watch?v=Zw9KeP3OzpU)

#### Catamorphisms
> A catamorphism (cata meaning "downwards") is a generalization of the concept of a `fold`
> models the fundamental pattern of (internal) iteration
> e.g.
> - for a `List`, it describes processing from the right
> - for a `Tree`, it describes a bottom-up traversal, i.e. children first

> `foldr` from the Haskell Prelude is a specialized catamorphism:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> [b]
foldr f z [] = z
foldr f z (x : xs) = x `f` foldr f z xs
```

> can be expressed as a single F-algebra `f b -> b` over a functor `f` and carrier `b`
```haskell
foldr :: (Maybe (a, b) -> b) -> [a] -> b
foldr alg [] = alg $ Nothing
foldr alg (x : xs) = alg $ Just (x, foldr alg xs)
```

> could factor out the `List a` to `Maybe (a, List a)` isomorphism as `unList`
```haskell
foldr :: (Maybe (a, b) -> b) -> [a] -> b
-- (***) :: (a -> a) -> ([a] -> b) -> (a, [a]) -> (a, b)
-- id *** foldr alg :: (a, [a]) -> (a, b)
-- fmap (id *** foldr alg) :: Maybe (a, [a]) -> Maybe (a, b)
-- alg :: Maybe(a, b) -> b
foldr alg = alg . fmap (id *** foldr alg) . unList
  where
    unList :: [a] -> Maybe (a, [a])
    unList [] = Nothing
    unList (x : xs) = Just (x, xs)

length :: [a] -> Int
-- b = Int
length = foldr alg
  where
    alg :: Maybe (a, Int) -> Int
    alg Nothing = 0
    alg (Just (_, xs)) = xs + 1
```

```haskell
data Branch a
  = Just (a, Branch a)
  | Nothing

newtype Node a
  = Node a -- Identity Functor
```

> commutative diagram
```
Maybe (a,[a]) --fmap(id *** foldr alg)--> Maybe (a,b)
  ^                                       |
  |                                       |
  unList                                  alg
  |                                       |
  |                                       v
 [a] -----------foldr alg---------------> b
```

> can write a left `fold` using an algebra with a higher-order carrier `b -> b`
```haskell
foldl :: forall a b. (b -> a -> b) -> [a] -> b -> b
-- foldr :: (Maybe (a, b) -> b) -> [a] -> b
-- b = (b -> b)
-- foldr :: (Maybe (a, b -> b) -> (b -> b)) -> [a] -> (b -> b)
-- fmap (id *** foldr alg) :: Maybe (a, [a]) -> Maybe (a, b -> b)
foldl f = foldr alg
  where
    alg :: Maybe (a, b -> b) -> (b -> b)
    alg Nothing = id
    alg (Just (x, xs)) = \r -> xs (f r x)
```

> Fixed points of Functors
> an idea from category theory which gives:
> - data-type generic functions
> - compositional data

```haskell
--| the least fixpoint of functor f
newtype Fix f = Fix { unFix :: f (Fix f) }
```

> A functor `f` is a data-type of kind `* -> *` (Arrow Kind, the kind of unary Type Functions) together with an `fmap` function

`Fix` is also a data-type of kind `* -> *`, thus by nested alternating:
`Fix f ~= f(Fix(f(Fix(f(...))))) ~= f(f(f(...))) ~= (f . f . f(...) )`

`Fix f` is the least fixed point of endo Type Function / EndoFunctor `f :: * -> *`

`fix f` is the least fixed point of endo function `f :: a -> a`
```haskell
fix :: (a -> a) -> a
fix f =
  let
    x = f x
  in
    x
```

take `List` as an example
```haskell
(:) :: a -> [a] -> [a]
(a : _) :: [a] -> [a] -- endofunction
(a : _) . (a : _) = (a : a : _) :: [a] -> [a] -- function composition
(a : _) . (a : _) . (a : _) = (a : a : a : _) :: [a] -> [a]
-- it's possible to compose infinite number of endofunctions of the same type
-- because endofunctions with composition `(.)` form a monoid

-- derive `sequence` based on this special property of endofunction

sequence :: Traversable t => Applicative m => t (m a) -> m (t a)
-- original implementation of `sequence`
   sequence :: Traversable List => Applicative Maybe => [Maybe a] -> Maybe [a]
0. sequence $ Just(1) : Just(2) : []
        (:) :: Int -> [Int] -> [Int]
1. fmap (:) Just(1) <*> sequence (Just(2) : [])
       (1 : _) :: [Int] -> [Int]
              (<*>) :: Functor f => f (a -> b) -> f a -> f b
            -- left-associative
2. Just(1 : _) <*> (fmap (:) Just(2) <*> sequence [])
3. Just(1 : _) <*> (Just(2 : _) <*> (pure []))
4. Just(1 : _) <*> (Just(2 : _) <*> Just([]))
5. Just(1 : _) <*> Just(2 : [])
6. Just(1 : 2 : [])

-- use endofunction composition
-- disclaimer: not formal
1. Just(1 : _) <*> Just(2 : _)  <*> Just([])
2. Just(1 : 2 : _) <*> Just([])
3. Just(1 : 2 : [])

data ListF a r
  = C a r
  | N
  
instance Functor (ListF a) where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons x (f xs)

forall a r. ListF a r :: Type -> Type -> Type
(given a.) forall r. ListF a r :: Type -> Type -- endofunctor
                -- (.) , functor composition
forall r. (ListF a) . (ListF a) r = forall r. ListF a (ListF a r) :: Type -> Type
forall r. ListF a (ListF a (ListF a (... (ListF a r) ...))) :: Type -> Type
-- similar to endofunction

data NatF r
  = Zero
  | Succ r
  deriving Functor
```

> Limitations
> - The set of data-types that can be represented by means of `Fix` is limited to regular data-types
> - Nested data-types and mutually recursive data-types require higher-order approaches

[Data.Functor.Fixedpoint](http://hackage.haskell.org/package/unification-fd-0.10.0.1/docs/Data-Functor-Fixedpoint.html)
> For more on the utility of two-level recursive types, see:
> - Tim Sheard (2001) Generic Unification via Two-Level Types and Paramterized Modules, Functional Pearl, ICFP.
> - Tim Sheard & Emir Pasalic (2004) Two-Level Types and Parameterized Modules. JFP 14(5): 547--587. This is an expanded version of Sheard (2001) with new examples.
> - Wouter Swierstra (2008) Data types a la carte, Functional Pearl. JFP 18: 423--436.

> Data-type generic programming
> - allows as to parametrize functions on the structure, or shape, of a data-type
> - useful for large complex data-types, where boilerplate traversal code often dominates, especially when updating a small subset of constructors
> - for recursion schemes, we can capture the pattern as a standalone combinator

> Catamorhpisms
> - we would like to write `foldr` once for all data-types
> - category theory shows us how to define its data-type generically for a functor fixed-point
```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

> Commutative diagram for Catamorphism
```
f(Fix x) --fmap(cata alg)--> f a
  |                           |
  |                           |
 Fix                         alg
  |                           |
  v                           v
Fix f ----cata alg----------> a
```

> The catamorphism-fusion law
> can be used to transform the composition of a function with a catamorphism into single catamorphism,
> eliminating intermediate data structures.

```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
p :: Functor f => f a -> a
q :: Functor f => f b -> b
r :: a -> b

if
  r . p :: f a -> b
              fmap r :: f a -> f b
          q . fmap r :: f a -> b
  r . p = q . fmap r
then
      cata p :: Fix f -> a
  r . cata p :: Fix f -> b
               cata q :: Fix f -> b
  r . cata p = cata q
```

> Example: a simple expression language
```haskell
type Id = String

-- pattern functor ExprF
-- represents the structure of type Expr
data ExprF r
  = Const Int
  | Var Id
  | Add r r
  | Mul r r
  | IfNeg r r r
    deriving ( Show, Eq, Ord, Functor, Foldable, Traversable )

newtype Fix f = Fix { unFix :: f (Fix f) }

-- the isomorphism between a data-type Expr and its pattern functor type ExprF
-- is witnessed by the functions `Fix` and `unFix`
type Expr = Fix ExprF

type Env = Map Id Int

eval :: Env -> Expr -> Maybe Int
eval env = cata (evalAlg env)

evalAlg :: Env -> ExprF (Maybe Int) -> Maybe Int -- carrier b = Maybe Int
evalAlg env = alg where
  alg (Const c)     = pure c
  alg (Val i)       = M.lookup i env -- Maybe
  alg (Add x y)     = (+) <$> x <*> y
  alg (Mul x y)     = (*) <$> x <*> y
  alg (IfNeg t x y) = t >>= bool x y . (<0)
  
textEnv :: Env
textEnv = M.fromList [("a",1), ("b",3)] -- Map Id Int

e1 :: Expr
-- \a b -> (If (1 * a < 0) then (b + 0) else (b + 2)) * 3
e1 =
  Fix(Mul
    (Fix(IfNeg
      (Fix(Mul
        (Fix (Const 1))
        (Fix (Var "a"))
      ))
      (Fix(Add
        (Fix(Var "b"))
        (Fix(Const 0))
      ))
      (Fix(Add
        (Fix(Var "b"))
        (Fix(Const 2))
      ))
    ))
    (Fix (Const 3)))

-- eval testEnv e1 => Just 9
```

> Composing Algebras
> Example: an optimization pipeline
```haskell
optAdd :: ExprF Expr -> Expr
optMul :: ExprF Expr -> Expr

optimizeSlow :: Expr -> Expr
               cata :: Functor f => (f a -> a) -> Fix f -> a
               cata optAdd :: Fix ExprF -> Expr
               Fix ExprF = Expr
               cata optAdd :: Expr -> Expr
                             cata optMul :: Expr -> Expr
optimizeSlow = cata optAdd . cata optMul
```
> We need an algebra composition operator that gives us **short-cut fusion**:
```haskell
cata p . cata q = cata (p `comp` q)
```
> For the special case:
```haskell
p :: f a -> a
q :: g (Fix f) -> Fix f -- carrier b = Fix f
newtype Fix f = Fix { unFix :: f (Fix f) }

y :: g (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
x :: f (Fix f) -> Fix f
comp x y = x . unFix . y
```

similar to CoYoneda
`map` over the same structure multiple times, you can compose the functions and then do one single `map`

> The catamorphism compose law

```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
p :: f a -> a
r :: g a -> f a

     p :: f (Fix f) -> Fix f -- carrier a = Fix f
cata p :: Fix f -> Fix f
                     r :: g (Fix f) -> f (Fix f)
               Fix . r :: g (Fix f) -> Fix f
cata p . cata (Fix . r) :: g (Fix f) -> Fix f
cata p . cata (Fix . r) = cata (p . r)
                                p . r :: g (Fix f) -> Fix f -- a = Fix f
                          cata (p . r) :: Fix f -> Fix f
```

> Combining Algebras
> Given the following two algebras, 
```haskell
p :: f a -> a
q :: f b -> b
```
> we want an algebra of type `f (a, b) -> (a, b)`
two algebras working in parallel

> banana-split theorem:
```haskell
      (&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
cata p :: Fix f -> a
           cata q :: Fix f -> b
cata p &&& cata q :: Fix f -> (a, b)
cata p &&& cata q =
             fmap fst :: f (a, b) -> f a
         p . fmap fst :: f (a, b) -> a
                              fmap snd :: f (a, b) -> f b
                          q . fmap snd :: f (a, b) -> b
         p . fmap fst &&& q . fmap snd :: f (a, b) -> (a, b)
  cata ( p . fmap fst &&& q . fmap snd ) :: Fix f -> (a, b)
  cata ( p . fmap fst &&& q . fmap snd )
```
> rewrite the **Product** of two algebra using `funzip`
```haskell
(***) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f *** g) (a, c) = (f a, g c)
funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd

-- p `algProd` q = p . fmap fst && q . fmap snd
algProd :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
algProd f g = (f *** g) . funzip
```
> we can also combine two algebras over different functors
> but the same carrier type into a **CoProduct**
```haskell
(|||) :: (a -> c) -> (b -> c) -> Either a b -> c
(f ||| g) e =
  case e of
    Left a ->
      f a
    Right b ->
      g b

algCoprod :: Functor f => Functor g => (f a -> a) -> (g a -> a) -> Either (f a) (g a) -> a
algCoprod = (|||)
```

> Working with fixed data-types
```haskell
class Functor f => Fixpoint f t | t -> f where -- t = Fix f, unFix t = f(Fix f)
  inF :: f t -> t -- f(Fix f) -> Fix f
  outF :: t -> f t -- Fix f -> f(Fix f)
  
-- before
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

-- now
cata :: Fixpoint f t => (f a -> a) -> t -> a
cata alg = alg . fmap (cata alg) . outF

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Functor f => Fixpoint f (Fix f) where
  inF = Fix
  outF = unFix

data ListF a r
  = C a r
  | N
  
instance Fixpoint (ListF a) [a] where
  inF :: ListF a [a] -> [a]
  inF N        = []
  inF (C x xs) = x : xs

  outF :: [a] -> List a [a]
  outF []       = N
  outF (x : xs) = C x xs
  
data NatF r
  = Zero
  | Succ r
  deriving Functor

instance Fixpoint NatF Integer where
  inF :: NatF Integer -> Integer
  inF Zero     = 0
  inF (Succ n) = n + 1
  
  outF :: Integer -> NatF Integer
  outF n | n > 0     = Succ (n - 1)
         | otherwise = Zero
```

#### Anamorphisms
> An anamorphism (ana meaning "upwards") is generalization of the concept of an `unfold`
> - The **corecursive** dual of catamorphisms
> - produces `Stream` and other regular structures from a seed
> - `ana` for `List` is `unfoldr`, view patterns help see the duality

```haskell
foldr :: (Maybe (a, b) -> b) -> [a] -> b
foldr f [] = f $ Nothing
foldr f (x : xs) = f $ Just (x, foldr f xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f (f -> Nothing) = []
unfoldr f (f -> Just (x, unfoldr f -> xs)) = x : xs
```

> Example: `replicate` the supplied seed by a given number
```haskell
replicate :: Int -> a -> [a]
replicate n x = unfoldr c n where
  c 0 = Nothing
  c n = Just (x, n-1)
  
-- replicate 4 '*' => "****"
```

> Example: split a list using a predicate
[Data.Text](http://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text.html)

```haskell
drop :: Int -> Text -> Text
-- O(n) drop n, applied to a Text, returns the suffix of the Text after the first n characters, or the empty Text if n is greater than the length of the Text. Subject to fusion.

break :: (Char -> Bool) -> Text -> (Text, Text) 
-- O(n) break is like span, but the prefix returned is over elements that fail the predicate p.

linesBy :: (t -> Bool) -> [t] -> [[t]]
linesBy p = unfoldr c where
  c [] = Nothing
  c xs = Just $ second (drop 1) $ break p xs
  
-- linesBy (== ',') "foo,bar,baz" => ["foo", "bar", "baz"]
```

> Example: merging two ordered lists
```haskell
mergeLists :: forall a. Ord a => [a] -> [a] -> [a]
mergeLists = curry $ unfoldr c where
  c :: ([a], [a]) -> Maybe (a, ([a], [a]))
  c ([], []) = Nothing
  c ([], y : ys) = Just (y, ([], ys))
  c (x : xs, []) = Just (x, (xs, []))
  c (x : xs, y : ys) | x <= y = Just (x, (xs, y : ys))
                     | x > y  = Just (y, (x : xs, ys))

-- mergeLists [1,4] [2,3,5] => [1,2,3,4,5]
```

> Corecursion
> An anamorphism is an example of corecursion, the dual of recursion.
> Corecursion produces (potentially infinite) codata,
> whereas oridinary recursion consumes (necessarily finite) data.
> - using `cata` and `ana` only, our program is guaranteed to terminate
> - However, not every program can be written in terms of just `cata` or `ana`

> There is no enforced distinction between data and codata in Haskell,
> so we can make use of `Fix` again:
```haskell
--| anamorphisms
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) .coalg

-- for comparison
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

> However, it is often useful to try to enforce this distinction,
> especially when working with streams.
```haskell
--| The greatest fixpoint of functor f
newtype Cofix f = Cofix { unCofix :: f (Cofix f) }

-- for comparison
newtype Fix f = Fix { unFix :: f (Fix f) }
```
> an alternative anamorphism typed for codata
```haskell
ana' :: Functor f => (a -> f a) -> a -> Cofix f
ana' coalg = Cofix . fmap (ana' coalg) . coalg
```
> Commutative diagram
```
f (Cofix f) <--fmap(ana coalg)-- f a
   ^                              ^
   |                              |
 unFix                          coalg
   |                              |
   |                              |
Cofix f <------ana coalg----------a
```

> Example : coinductive `Stream`
```haskell
data StreamF a r = S a r deriving Show
type Stream a = Cofix (StreamF a)

instance Functor (StreamF a) where
  fmap f (S x xs) = S x (f xs)
  
--| Stream constructor
consS :: a -> Stream a -> Stream a
consS x xs = Cofix (S x xs)

--| Stream deconstructors
headS :: Stream a -> a
headS (unCofix -> (S x _)) = x
tailS :: Stream a -> Stream a
tailS (unCofix -> (S _ xs)) = xs
```

> the function `iterateS` generates an infinite stream using the supplied iterator and seed
```haskell
iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana' c where
  c x = S x (f x)

s1 :: Stream Int
s1 = iterateS (+1) 1

-- takeS 6 $ s1 => [1,2,3,4,5,6]
```


## 16. [Lenses, Folds, Traversals - 2nd NY Haskell meetup](https://www.youtube.com/watch?v=cefnmjtAolY)

> @PLT_Borat: Costate Comonad Coalgebra is equivalent of Java's member variable update technology for Haskell.
> Record accessing syntax is not composable.

> a simple definition of Lens
```haskell
data Lens s a = Lens
  { set  :: s -> a -> s
  , view :: s -> a
  }
view :: Lens s a -> s -> a
set :: Lens s a -> s -> a -> s

-- Laws
-- 1. set l (view l s) s = s
-- 2. view l (set l s a) = a
-- 3. set l (set l s a ) b = set l s b -- set twice at the same place, the first one doesn't matter
```

> a cleaner definition,
> ```haskell
> data Lens s a = Lens (s -> (a -> s, a))
> ```
which "merges" `set` and `view` by `fanout`(`set &&& view`) since they both share an input argument with type `s`

> happen to be isomorphic to the construction of a `Store` Monad which is theoretically a Costate Comonad, thus Lens itself is a Costate Comonad Coalgebra
```haskell
data Store s a = Store (s -> a) s

newtype Lense s a = Lens (s -> Store a s)

instance Category Lens where
  id = Lens (Store id)
  Lens f . Lens g = Lens $ \r -> case g r of
    Store sr s -> case f s of
      Store ts t -> Store (sr . ts) t
```

[`ekmett/lens > Wiki > Derivation`](https://github.com/ekmett/lens/wiki/Derivation)
> The power of (.)
[Compositions of compositions](https://gist.github.com/rebcabin/375cc457241ba7d8ee90cb48312b5c9d)
> ```haskell
> (.)         :: (b -> c) -> (a              -> b) -> a              -> c
> (.).(.)     :: (b -> c) -> (a1 -> a2       -> b) -> a1 -> a2       -> c
> (.).(.).(.) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
> ```

```haskell
(.) :: forall a b c. (b -> c) -> Arrow a1 b -> Arrow a1 c
(.).(.) :: forall a1 a2 b c. (b -> c) -> Arrow a1 (Arrow a2 b) -> Arrow a1 (Arrow a2 c)
(.).(.).(.) :: forall a1 a2 a3 b c. (b -> c) -> Arrow a1 (Arrow a2 (Arrow a3 b)) -> Arrow a1 (Arrow a2 (Arrow a3 c))

fmap :: Functor f => (b -> c) -> f b -> f c
fmap.fmap :: (Functor f, Functor g) => (b -> c) -> f(g b) -> f(g c)
fmap.fmap.fmap :: (Functor f, Functor g, Functor h) => (b -> c) -> f(g(h b)) -> f(g(h c))

instance Functor (Arrow e) where
  fmap = (.)

-- composition of Functors is still a Functor
-- thus, Arrow a1 (Arrow a2 (Arrow a3 _)) ~= (Arrow a1).(Arrow a2).(Arrow a3) _
-- is a Functor
```

> [Semantic Editor Combinators - Conal Elliott](http://conal.net/blog/posts/semantic-editor-combinators)
> ```haskell
> type SEC s t a b = (a -> b) -> s -> t
>
> fmap :: Functor f => (a -> b) -> f a -> f b
> fmap :: Functor f => Sec (f a) (f b) a b
>
> result :: Sec (e -> a) (e -> b) a b
> result = (.)
>
> -- result :: Sec (Arrow e a) (Arrow e b) a b
> -- result = fmap
>
> element :: Sec [a] [b] a b
> element = fmap
>
> second :: Sec (c, a) (c, b) a b
> second = fmap
>
> first :: Sec (a, c) (b, c) a b
> first f (a, b) = (f a, b)
> ```

> Setters


## 17. Hexagonal Architecture

#### 17.1 [Matthias Noback - Hexagonal Architecture - Message-Oriented Software Design](https://www.youtube.com/watch?v=K1EJBmwg9EQ)

[Web Services Description Language (WSDL)](https://en.wikipedia.org/wiki/Web_Services_Description_Language)
> an XML-based interface definition language that is used for describing the functionality offered by a web service.

message/event can be defined as method (with desired payloads as its arguments) in an object interface
but method is directional edges (`Input :: StateTransitionObject ~> State -> (State, Output)`) and hard to be shared
then defined as data-type / object

command - DSL implemented in object system
command handlers - interpreters

> "A good software architecture allows decisions to be deferred and delayed"
> - Robert Martin, Screaming Architecture

#### 17.2 Alistair in the "Hexagone"

> configurable dependency
> dependency injection strategy is one of different ways to achieve this

[1/3](https://www.youtube.com/watch?v=th4AgBcrEHA)
[2/3](https://www.youtube.com/watch?v=iALcE8BPs94)

```c#
using NFluent;
using NUnit.Framework;
using NSubstitute; // .Net mocking frameworks

namespace HexagonalThis.Tests
{
  public class AcceptanceTests
  {
    [Test]
    public void Should_give_Verses_when_asked_for_Poetry()
    {
      // IRequestVerses : left-side Port
      // PoetryReader : the Hexagon
      IRequestVerses poetryReader = new PoetryReader();
      var verses = poetryReader.giveMeSomePoetry();

      Check.That(verses).IsEqualTo("I want to sleep\r\nSwat the flies\r\nSoftly, please.\r\n\r\n-- Masaoka Shiki (1867-1902)");
    }
    
    [Test]
    public void Should_give_Verses_when_asked_for_Poetry_with_the_support_of_a_PoetryLibrary()
    {
      IObtainPoems poetryLibrary = Substitute.For<IObtainPoems>();
      poetryLibrary.getMeAPoem().Returns("If you could read a leaf or tree\r\nyou'd have no need of books.\r\n-- Alistair Cockburn (1987)");
      
      var poetryReader = new PoetryReader(poetryLibrary);
      var verses = poetryReader.giveMeSomePoetry();
      
      Check.That(verses).IsEqualTo("If you could read a leaf or tree\r\nyou'd have no need of books.\r\n-- Alistair Cockburn (1987)");
      
    }

    [Test]
    public void Should_give_Verses_when_asked_for_Poetry_with_the_support_of_a_Console()
    {
      // 1. Instantiate right-side Adapter(s)
      IObtainPoems poetryLibrary = Substitute.For<IObtainPoems>();
      poetryLibrary.getMeAPoem().Returns("If you could read a leaf or tree\r\nyou'd have no need of books.\r\n-- Alistair Cockburn (1987)");
      
      // 2. Instantiate the Hexagon
      var poetryReader = new PoetryReader(poetryLibrary);
      
      IWriteLines publicationStrategy = Substitute.For<IWriteLines>();
      var consoleAdapter = new ConsoleAdapter(poetryReader, publicationStrategy);
      
      consoleAdapter.ask(); // Input
      
      // Check that the Console.writeline has been called
      publicationStrategy.Received().writeLine("If you could read a leaf or tree\r\nyou'd have no need of books.\r\n-- Alistair Cockburn (1987)");
    }
    
    [Test]
    public void Sould_give_verses_when_asked_for_poetry_with_the_support_of_a_FileAdapter
    {
      var fileAdapter = new PoetryLibraryFileAdapter(@"./Rimbaud.txt");
      
      var poetryReader = new PoetryReader(fileAdapter);
      
      var verses = poetryReader.giveMeSomePoetry();
      
      Check.That(verses).IsEqualTo("Come je descendais ...");
    }
  }
  
  public class PoetryLibraryFileAdapter : IObtainPoems
  {
    private string poem;
    
    public PoetryLibraryFileAdapter(string filePath)
    {
      this.poem = File.ReadAllText(filePath);
    }
    
    public string getMeSomePoetry()
    {
      return this.poem;
    }
    
  }
  
  public interface IWriteLines
  {
    void writeLine(string text);
  }
  
  public class ConsoleAdapter // both Input and Output
  {
    private readonly IRequestVerses poetryReader;
    private readonly IWriteLines publicationStrategy;

    public ConsoleAdapter(IRequestVerses poetryReader, IWriteLines publicationStrategy)
    {
      this.poetryReader = poetryReader; -- assume the dependencies for PoetryReader are fulfilled
      this.publicationStrategy = publicationStrategy;
    }
    
    public void ask()
    {
      // from infra to domain
      // currently input is Unit, so nothing to be transformed
      
      // business logic
      var verses = this.poetryReader.giveMeSomePoetry(); // PoetryReader, Input Effect
      
      // from domain to infra
      var transformedVerses = $"Poem: {verses}"
      this.publicationStrategy.writeLine(transformedVerses); // IWriteLines, Output Effect
    }
  }
  
  public class HardCodedPoetryLibrary : IObtainPoems
  {
    public string getMeAPoem()
    {
      return "I want to sleep\r\nSwat the flies\r\nSoftly, please.\r\n\r\n-- Masaoka Shiki (1867-1902)";
    }
  }
  
  // <summary>
  // right-side Port
  // </summary>
  public interface IObtainPoems
  {
    string getMeAPoem();
  }
  
  public class PoetryReader : IRequestVerses
  {
    private readonly IObtainPoems poetryLibrary; -- first dependency
  
    public PoetryReader() : this(new HardCodedPoetryLibrary()) {}
    public PoetryReader(IObtainPoems poetryLibrary)
    {
      this.poetryLibrary = poetryLibrary;
    }
  
    public string giveMeSomePoetry()
    {
      return this.poetryLibrary.getMeAPoem();
    }
  }
  
  public interface IRequestVerses
  {
    string giveMeSomePoetry(); -- () -> IO String
  }
}
```

[3/3](https://www.youtube.com/watch?v=DAe0Bmcyt-4)


Hexagonal Architecture emphasizes the distinction between domain logic and IO effects.
But it doesn't highlight potentially multiple representations of the same domain model.
The different representations are buried in Adapters (/translators) and directly shipped into/outside the application.

The rationale might be the tight connection between the IO medium and the corresponding representation in the old days.
e.g. 
Console -- Text
HTTP -- BitString
UI -- HTML

But now, the same application can target multiple platforms which all have their own UI conventions where the distinction between different representations is wide enough to motivate a separate domain concept.

CQRS has this in mind.

[CQRS - Martin Fowler](https://martinfowler.com/bliki/CQRS.html)
> The mainstream approach people use for interacting with an **information system** is to treat it as a **CRUD** datastore.
> By this I mean that we have mental model of some record structure where we can create new records, read records, update existing records, and delete records when we're done with them.
> In the simplest case, our interactions are all about storing and retrieving these records.

> As our needs become more sophisticated we steadily move away from that model.
> As this occurs we begin to see multiple representations of information.
> When users interact with the information they use **various presentations** of this information, each of which is a different representation.

> Developers typically build their own conceptual model which they use to manipulate the core elements of the model.
> If you're using a Domain Model, then this is usually the conceptual representation of the domain.
> You typically also make the persistent storage as close to the conceptual model as you can.

> This structure of multiple layers of representation can get quite complicated, but when people do this they still resolve it down to **a single conceptual representation** which acts as **a conceptual integration point** between all the presentations.

> The change that CQRS introduces is to split that conceptual model into separate models for update and display, 

> A web example would see a user looking at a web page that's rendered using the query model.
> If they initiate a change that change is routed to the separate command model for processing, the resulting change is communicated to the query model to render the updated state.

Another problem doesn't solve by the Hexagonal Architecture, even without nested layers, is that 
- the cascading pathways of events through the object graph are implicit (there is no one place in the code base to check all possible pathways,
- and messages can be passed back and forth between objects (feedback loops) so there's no way programmers can reason about
  - whether it will halt or not,
  - where the message will end up being and get consumed by which driver, by just staring at the code) 
and different pathways are not independent (one integration point failed, everything collapses; to make it robust, the reuse components should be stateless/context-free).

Well, this could be a problem around modulization of domain model which is not HA's focus.

#### 17.3 [Chris Fidao - Hexigonal Architecture](https://www.youtube.com/watch?v=6SBjKOwVq0o)

CommandBus on top of Alistair's model for concurrency (I suppose)

Framework layer (Port)
- translate inbound IO effects (through callbacks) as inbound Command and push it into CommandBus in Application layer
- executors of outbound IO effects from Application layer
Application layer (Adapter)
- CommandBus: dispatch Commands in the CommandBus to Domain layer
- Dispatcher: translate outbound Event from Domain layer as outbound IO effects
- dispatch different types of IO effects to corresponding drivers in the Framework layer
Domain layer (Hexagon)
- Handler: (current) State x (inbound) Command -> (new) State x (outbound) Events
- push outbound Events into Dispatcher in Application layer

dependency through interface ~= callback in languages with lambda functions


> Domain layer example
```php
class Ticket extends Model { // Domain layer
  public function assignStaffer(Staffer $staffer) {
    if(! $staffer->categories->contains( $this->category ) ) {
      throw new DomainException("Staffer can't be assigned to " $this->category);
    }
    $this->staffer()->associates($staffer); // Set Relationship
    return $this;
  }

  public function setCategory(Category $category) {
    if( $this->staffer instanceof Staffer && ! this->staffer->categories->contains( $category ) ) {
      $this->staffer = null; // Unset staffer if can't be assigned to set category
    }
    $this->category()->associate( $category ); // Set Relationship
    return $this;
  }
  
  public function save(array $options = array()) {
    /* Integrity Checks, and then: */
    if ( ! $this->exists ) {
      $this->raise( new TicketCreatedEvent($this) ); // create Command and push it into CommandBus
    }
    return parent->save($options);
  }
}

class CreateTicketCommand {
  protected $data;
  
  public function __constructs($data) {
    $this->data = $data;
  }
  
  public function __get($property) {
    // simplified example
    return $this->data[$property];
  }
}

class SimpleCommandBus { // Application layer
  // ...
  public function execute( $command ) {
    return $this->getHandler( $command )
                ->handle( $command )
  }
}

class CreateTicketHandler implements HandlerInterface { // Domain layer
  public function handle( $command ) {
    $this->validate( $command ); // Throw ValidationException
    $this->save( $command );
  }
  
  protected function save( $command ) {
    $ticket = new Ticket;
    $ticket->setCategory( $this->catRepo->find($command->category_id) );
    $ticket->setStaffer( $this->staffRepo->find($command->staffer_id) );
    $ticket->addMessage( $command->message );
    
    $this->ticketRepo->save( $ticket ); // Use Repositories
    
    $this->dispatcher->dispatch( $ticket->flushEvents() ); //Fire Events
  }
}

class DbTicketRepository implements RepositoryInterface {
  public function getStaffOpenTickets(Staffer $staffer, $limit=10) {
    return $this->ticket->where('staff_id', $staffer->id)
                ->take($limit)->get();
  }
  
  public function save(Ticket $ticket) {
    $ticket->save();
  }
}

class TicketController extends BaseController {
  public function createTicket() {
    $command = new CreateTicketCommand( Input::all() );
    
    try {
      $this->bus->execute($command);
    }
    catch(ValidationException $e) {
      return Redirect::to('/tickets/new')->withErrors( $e->getErrors() );
    }
    catch(DomainException $e) {
      return Redirect::to('/tickets/new')->withErrors( $e->getErrors() );
    }
    
    return Redirect::to('/tickets');
  }
}

class SetEmailNotifier implements NotifierInterface {
  public function __construct(SesClient $client) {
    $this->client = $client;
  }
  
  public function send(Message $message) {
    $to = [$message->to()];
    $message = ['Data' => $message->message()];
    
    $this->client->sendEmail([
      'Destination' => ['ToAddresses' => $to],
      'Message' => ['Body' => ['Html' => $message]]
    ]);
  }
}

class LaravelDispatcher implements Dispatcher {
  public function __construct(EventDispatcher $dispatcher) {
    $this.dispatcher = $dispatcher;
  }
  
  public function dispatch(Array $events) {
    $this->dispatcher->fire( $event->name(), $event );
  }
}
```



## 18. [PS Unscripted - Comonads for UIs](https://www.youtube.com/watch?v=EoJ9xnzG76M)

> ```haskell
> class Functor w => Comonad m where
>   extract   :: w a -> a
>   duplicate :: w a -> w (w a)
>   (=>>)     :: w a -> (w a -> b) -> w b
>
> (=>=) :: Comonad m => (w a -> b) -> (w b -> c) -> w a -> c -- Cokleisli composition
> (f =>= g) w = g (w =>> f)
>
> -- laws
> f :: w a -> b
>       extract :: w b -> b
> f =>= extract = f
> extract :: w a -> a
>             f :: w a -> b
> extract =>= f = f
> f =>= (g =>= h) = (f =>= g) =>= h
>
> -- example: Store Comonad
> data Store s a = Store s (s -> a)
>
> instance Comonad (Store s) where
>   extract :: Store s a -> a
>   extract (Store here go) = go here
>   duplicate :: Store s a -> Store s (Store s a)
>   duplicate (Store here go) =
>     Store here $ \there -> Store there go
>
> -- example: Traced
> data Traced w a = Traced (w -> a)
> instance Monoid w => Comonad (Traced w) where
>   extract :: Traced w a -> a
>   extract (Traced f) = f mempty
>   duplicate :: Traced w a -> Traced w (Traced w a)
>   duplicate (Traced f) =
>     Traced $ \w -> Traced (f . (w <>))
> ```

[The Dual of Substitution is Redecoration](https://pdfs.semanticscholar.org/b994/9ae4a04fb43a9279ade8615d448e77d563e5.pdf)

[Cofree meets Free](http://blog.sigfpe.com/2014/05/cofree-meets-free.html)

[Comonads in Everyday Life](https://fmapfixreturn.wordpress.com/2008/07/09/comonads-in-everyday-life/)

> The virtual DOM API
> ```haskell
> data VDOM e -- e for Events
> data Patch
>
> diff :: VDOM e -> VDOM e -> Patch
> apply :: Patch -> Effect Unit
> ```

> Components
> ```haskell
> data Component model = Component
>   { initialState :: model
>   , render       :: model -> VDOM model
>   }
> ```

> can be modeled as Store
> ```haskell
> type Component model = Store model (VDOM model) -- s = model, a = VDOM model
>
> instance Comonad (Store s) where
>   -- renders the component's current state
>   extract :: Store s a -> a
>   extract :: Component model -> VDOM model
>
>   -- captures the possible future states of the component
>   duplicate :: w a -> w (w a)
>   duplicate :: Store s a -> Store s (Store s a)
>   duplicate :: Component model -> Store model (Component model)
> ```

> How can we explore the future?
> ```haskell
> future :: Store model (Component model)
>
> explore :: Store model (Component model) -> Component model
> -- join :: Monad w => w (w a) -> w a
> ```

Component is both Comonad and Monad

> we can
> - read the current state
> - move to a new state
> which can be packaged up using the `State` monad
> ```haskell
> explore :: State model () -> Store model (Component model) -> Component model
> explore state (Store here go) = go here
>   where
>     (_, there) = runState state here -- state transition from `here` to `there`
> ```

> redefine `Component`
> ```haskell
> data Component model = Component
>   { initialState :: model
>   , render       :: model -> VDOM (State model ())
>   }
>
> -- Component is both a Store Comonad and a State Monad
> type Component model = Store model (VDOM (State model ()))
> ```

> Pairings
> ```haskell
> data Component model
>   -- w (VDOM (m ()))
>   = Store model (VDOM (State model ()))
>
> explore :: m () -> w a -> a
>
> -- a more general concept
> pairing :: m (a -> b) -> w a -> b
> ```

> | Left Adjoint   | Right Adjoint     | Framework   |
> |----------------|-------------------|-------------|
> | `State s`      | `Store s`         | React       |
> | `Writer w`     | `Traced w`        | Incremental |
> | `Reader e`     | `Env e`           |             |
> | `Free f`       | `Cofree g` (\*)   | Halogen     |
> | `Free ((,) i)` | `Cofree ((->) i)` | Elm, Redux  |
>
> \* when `f` pairs with `g`

## 19.[LambdaConf 2015 - A Practical Introduction to Haskell GADTs Richard Eisenberg](https://www.youtube.com/watch?v=6snteFntvjM)

```haskell
{-# LANGUAGE GADTs #-}

data STy ty where -- a Type function where ty \in { Int, Bool }, not universally polymorphic but bounded by a set of Types
  SInt :: STy Int -- two Data constructors with separated Types
  SBool :: STy Bool

zero :: STy ty -> ty
zero SInt = 0
zero SBool = False

-- alternatively
data STy
  = SInt -- two Data constructors sharing the same (product) Type
  | SBool

data STyVal
  = SIntV Int
  | SBoolV Bool

zero :: STy -> STyVal
zero SInt = SIntV 0
zero SBool = SBoolV False

```

## 20.[React Fiber | Andrew Clark: What's Next for React — ReactNext 2016](https://www.youtube.com/watch?v=aV1271hd9ew)

concurrent scheduling of renders

e.g. prioritize animation frames over data update

> `react/react-reconciler/src/ReactUpdateQueue.js`
>
> a form of double buffering
> - a current queue, which represents the visible state of the screen 
> - a work-in-progress queue, which can be mutated asynchronously before it's committed
>
> Updates are not sorted by priority, but by insertion;
> new updates are always appended to the end of the list.

each render phase has a priority bar

updates are processed sequentially following the insertion order but drop updates without sufficient priority (lower than the priority bar)

the same update can be processed multiple times across multiple render phases

[acdlite/react-fiber-architecture](https://github.com/acdlite/react-fiber-architecture)

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


## 5. Category Theory I - Bartosz Milewski

### 10.2 [Monoid in the category of endofunctors](https://www.youtube.com/watch?v=GmgoPd7VQ9Q)

## 6. Category Theory II - Bartosz Milewski

### 1.2 [Limits](https://www.youtube.com/watch?v=sx8FELiIPg8)

generalization from a categorical Product to Limit

a constant functor from I to C

a functor from I to C

a natural transformation from a constant functor to a functor
forms a cone (all "walls" commute) (also a hom-set in C)

from any cone to Limit, there is a unique morphism that all "walls" commute

in the category of cones with only these unique morphisms, Limit is the terminal object

TODO: replace sets of commutations by introducing a natural transformation at a higher abstraction level

### 2.1 Limits, Higher order functors

### 2.2 Limits, Naturally

### 3.1 Examples of Limits and Colimits

### 3.2 Free Monoid

### 4.1 Representable Functors

### 4.2 [The Yoneda Lemma](https://www.youtube.com/watch?v=BiWqNdtptDI&index=9)

### 5.1 [Yoneda Embedding](https://www.youtube.com/watch?v=p_ydgYm9-yg&index=10)

> LHS: a polymorphic higher-order function
> RHS: a data-type

> example
> `alpha :: (a -> x) -> [x]` is isomorphic to `[a]`
> the function must encapsulate/memorize `[a]` to construct `[x]`

similarly,
`type Lens s t a b = (a -> f b) -> (s -> f t)` is isomorphic to `s -> (a, b -> t)`

### 5.2 Adjunctions

### 6.1 Examples of Adjunctions

### 6.2 Free-Forgetfu Adjuction, Monads from  Adjunctions


### 7.1 [Category Theory II 7.1: Comonads](https://www.youtube.com/watch?v=C5oogxdX_Bo)

```haskell
(=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)

extract :: w a -> a

extend :: (w a -> b) -> w a -> w b
extend f = fmap f . duplicate -- fmap :: (w a -> b) -> w (w a) -> w b, for a = w a

duplicate :: w a -> w (w a)
duplicate = extend id -- extend :: (w a -> w a) -> w a -> w (w a), for b = w a

```

### 7.2 [Category Theory II 7.2: Comonads Categorically and Examples](https://www.youtube.com/watch?v=7XQZJ4TLgX8)

Comonad, philosophically, can be treated as a world (source of information) with a focus (cursor) where the piece of information local to the focus is instantly accessible.

examples
1. `Product` (isomorphic to `Reader`)
2. Game of life
3. Stream

```haskell
data Stream a = Cons a (Stream a) -- infinite(endless) stream

extract :: Stream a -> a
extract ( Cons a _ ) = a

tail :: Stream a -> Stream a
tail ( Cons a as ) = as

duplicate :: Stream a -> Stream(Stream a)
duplicate steam = Cons stream (duplicate(tail stream))
```

Comonad is suitable for signal processing e.g. sampling, filtering (by convolution)

moving average of n elements (of a discrete signal)
```haskell
sumN :: Num a => Int -> Stream a -> a
sumN n ( Cons a as )
  | n > 0     = a + (sumN (n - 1) as)
  | otherwise = 0

averageN :: Fractional a => Int -> Stream a -> a
averageN n stream = (sumN n stream) / (fromIntegral n)

movingAverageN :: Fractional a => Int -> Stream a -> Stream a
-- extend :: (w a -> a) -> w a -> w a
-- averageN :: w a -> a
movingAverageN n = extend (avgN n)
```

From Haskell
```haskell
extract :: w a -> a
duplicate :: w a -> w (w a)
```
to category theory
```
epsilon (natural transformation) :: W (endofunctor) -> I (identity endofunctor)
delta (natural transformation) :: W -> W ∘ W
```

### 7.2 Comonads Categorically and Examples

### 8.1 [F-Algebra, Lambek's lemma](https://www.youtube.com/watch?v=zkDVCQiveEo&list=PLbgaMIhjbmElia1eCEZNvsVscFef9m0dm&index=16)

```haskell
type Algebra f a = f a -> a
```

### 8.2 [Catamorphisms and Anamorphisms](https://www.youtube.com/watch?v=PAqzQMzsUU8&list=PLbgaMIhjbmElia1eCEZNvsVscFef9m0dm&t=0s&index=17)

### 9.1 [Lenses](https://www.youtube.com/watch?v=9_iYlp8smc8&list=PLbgaMIhjbmElia1eCEZNvsVscFef9m0dm&t=0s&index=18)

### 9.2 [Lenses categorically](https://www.youtube.com/watch?v=rAa3pGp97IM&list=PLbgaMIhjbmElia1eCEZNvsVscFef9m0dm&t=564s&index=19)

## 7 [Seven Sketches in Compositionality: An Invitation to Applied Category Theory - David I. Spivak and Brendan Fong](https://www.youtube.com/playlist?list=PLKvKTRWejv0KqyzCH9gUD4k1QIKj10stE)

[web page](http://math.mit.edu/~dspivak/teaching/sp18/)

### Chapter 1: Cascade effects: posets and adjunctions.

`mpv --softvol yes --softvol-max 400` for volume amplification

#### 1.1
> Poset (partially-ordered set)
> Cascade of effects (generative effects)
> e.g. cascading failure in a system

> poset example: graph partition
> ordering of connectivity in a graph
> Partition of a set S is a set of disjoint nonempty sets whose union is S
> partition function is surjective

> `join` of `a` and `b` is the smallest element greater than (`>=`) both `a` and `b`
> (Colimit in category)
> `meet` of `a` and `b` is the greatest element smaller than (`<=`) both `a` and `b`
> (Limit in category)

> Definition of preorder set:
> Let S be a set, a pre-order on S is a relation `R \subset S x S`
> where `R(s1, s2)` if `(s1, s2) \in R`, e.g. `s1 <= s2`
> laws:
> 1. reflexivity: `R(s, s)` forall `s \in S`
> 2. transitivity: if `R(s1, s2), R(s2, s3)`, then `R(s1, s3)`

> partial order is an antisymmetric preorder
> 3. antisymmetry: if R(a, b) and R(b, a), then `a = b`

> Example: natural number set under `<=`
> `join` = `max`
> `meet` = `min`

> Example: natural number set under `\`
> Hasse diagram:
```
8
 \
  4   6   9
   \ / \ /
  5 2   3 7
   \ \ / /
      1
```
> `join` = `lcm`, least common multiple
> `meet` = `gcd`, greatest common divider

> Example: tree of life

### Chapter 2: Data transformations: categories, functors, universal constructions.

### Chapter 3: Resource theories and navigation: Monoidal posets and enrichment. 

### Chapter 4: Collaborative design: Profunctors, categorification, and monoidal categories.

### Chapter 5: Signal flow diagrams: Props, presentations, and proofs.

### Chapter 6: Electric circuits: hypergraph categories and operads.

### Chapter 7: Logic of behavior: sheaves toposes, and internal languages.


# Politics

## 1.[Kishore Mahbubani and Weiwei Zhang - new world order](https://www.bilibili.com/video/av27093393)

## 2.[Dirty Money](https://www.netflix.com/title/80118100)

### Ep 6. The Confidence Man

> Tim O'Brien
> "Fred Trump was authentically self-made, and Fred ultimately passed all that along to Donald."
> "Trump claimed he only borrowed a million dollars from his father."
> "He inherited, conservatively, tens of millions of dollars from his father."

>   1. Trump Tower, 1980s
> Tim O'Brien
> "Trump has this reptilian, knowing, street-smart awareness of the powers of media."
> "Donald gets his first taste of real independent fame."
> A.J. Benza, Gossip Columnist
> "Long before Internet, especially the '80s and '90s, the way people's images were built, was how they were treated in the newspapers, the tabloids.
> Anything floating around that became a big story, usually started in the gossip columns."
> Tim O'Brien
> "He uses media to keep this financial ping-pong game going about how rich he is."

>   2. Trump Plaza
> Tim O'Brien
> "he had everything to gain from using the notoriety, and banks were willing to throw humongous loans at him, as if they were snowballs."
> "He ended up using that money to go on this shopping spree for anything he wanted to do."
> "He got into businesses he didn't understand."
>   3. Trump Shuttle, airline
> "Losing 85 million dollars a year."
>   4. Trump Taj Mahal, casino, 1990s
> Mark Etess, former president & COO of Trump Taj Mahal
> "I was very surprised at his lack of understanding the moving pieces in operating a casino."
> "One of the fundamental things that you always have to know about operation of a casino is how much money do you have in the house.
>  They could not even answer that question."
> "is a financial disaster and it was the first of Trump's casinos to go into bankruptcy."
> "Trump's story in Atlantic City, should have been the most successful story in the history of gambling, had he not screwed it up."
> Tim O'Brien
> "It was a project so expensive and big that it was beyond both his managerial and financial abilities to run properly."
> "By the early 1990s, The Trump Organization was facing over 3 billion of debt.
>  The Taj Mahal, Trump Castle, and Trump Plaza and Casino all went bankrupt."

>   6. licensing, '90s - 2004
> Tim O'Brien
> "What Trump was doing from the early '90s to about 2004"
> ,from a business perspective, he really wasn't doing very much."
> "He kept himself in the public eye
> , by being a ubiquitous and easy-to-get presence on talk shows."
> "Donald Trump is notoriously short-term in his thinking and making cash quickly without being discerning is one of the great Achilles' heels of Donald Trump."
> "It's what's gotten him into bed at times with questionable people."
> "It's what's made him plaster his name on so many different products, he's essentially become a human shingle."
> 
> "he got into trouble with the banks.
> But they decided he was worth more alive than dead.
> His name had a tremendous amount of value, and he ended up licensing his name
> on these projects, and got a lot of money for that."
> "Donald Trump's name is on a lot of the buildings
> , but that doesn't mean he owns those buildings."
> "He was never the biggest real estate developer in New York, as he claimed, by any measure.
> By either the value of the property that he had sold or the square footage he owned."
> "But it was the West Side Yards that launched him in his new role as a licensing and development operation."
> Adam Davidson
> "Certainly by the 2000s, no major bank is gonna lend him any money.
> He defaulted so many times, declared bankruptcy so many times.
> And you can't be a real estate developer if you can't borrow money."
> "And this licensing deal solved that problem, bigly."
> "He didn't have to borrow any money, and he didn't have to do any work."
> "A key change in the Trump Organization's business model starts sort of accidentally in the late 1990s.
> The business starts turning international."
> "Trump was a famous name in Korea."

>   7. The Apprentice, 2004
> "The Apprentice overnight repositioned him in the American imagination as the embodiment of deal-making savvy, capable entrepreneur"
> "I think most of the Republican Party and a good portion of Democrats failed to recognize how much The Apprentice 
> solidified his image in the minds of the very voters who ended up supporting him."

>   8. Trump Towers SoHo
> Bayrock Group
>   Tevfik Arif, from Kazakhstan, associated with some of Kazakhstan's more notorious oligarchs who are known as being unbelievably corrupt, met Felix Sater at Russia.
>   Felix Sater, born in the Soviet Union, grew up in Brooklyn, had a few months as a successful trader on Wall Street, but went in jail after a bar fight, later starts doing one of these pump-and-dump schemes like in The Wolf of Wall Street.
> Adam Davidson
> "So this Kazakh refugee, basically with a sketchy past, meets this hustler from the streets of Brooklyn, and they create this company Bayrock."
> "for Trump SoHo, what Felix Sater arranged was for the main money to come from a man named Tamir Sapir."
> "this cab driver from Brooklyn is doing these massive deals in the former Soviet Union, where he becomes a multi-billionaire very quickly."

> "while Trump is on The Apprentice pitching Trump SoHo as his project that is wildly successful, it is the exact opposite."
> "And the building does eventually fail and then sold on to other owners,
> although it maintains the name Trump SoHo."

>   9. Trump Tower, Baku
> "In 2010, Felix Sater goes into the Trump Organization and becomes sort of an official deal maker."
> "So they start doing business with some of the sketchiest people in some of the sketchiest countries."
> "Trump Organization does this deal with the Mammadov family for Trump Tower, Baku."
> "The Trump Tower was in a pretty lousy neighborhood in Baku to have a luxury tower, so the building made no sense economically. That's a huge red flag."
> "the US officials considered Ziya Mammadov to likely be laundering money for the Iranian Revolutionary Guard."
> "This is the single biggest state sponsor for terrorism in the world."

>   10. Trump Tower, Moscow
> "Trump has expressed many times he wants a Trump Tower, Moscow."
> "So Sater starts talking to a guy he grew up with, named Michael Cohen,
>  who became a high-ranking executive at the Trump Organization
>  and is now Trump's personal attorney."
> "But then, they're not getting the right permissions from the Russian government, and Michael Cohen sends an e-mail to the PR department at the Kremlin,
> I represent Donald Trump and we wanna make this a deal."
> "The thing is, this all happens in January 2016, in the middle of the presidential campaign."
> "Trump knew, because Michael Cohen says he spoke to Trump about it three times,
> that his own high-ranking executive was reaching out to the Kremlin
> to try and get political favors to get a Trump Moscow built."

>   11. Trump University, Trump Network
> "it became kind of fly-by-night-ish type of seminar program. And we were now selling $1,500 to $35,000 seminars."
> "Playbooks for the sales team coach them on how to market the courses, even to single mothers with three children who, quote, may need money for food."
> "And it was very clear that, ethically, we were not in line."
>
> "He made you feel like, through him, you could have the life that you wanted and when people are looking for a way, it's easy to believe him."
>
> The mentor was supposed to be handpicked by Trump. It was supposed to be somebody that he had taught. Truth is they never met him and some of them didn't even have a license not to mention being successful.
>
> "The only person who is guilty of a cheap publicity stunt is America's leading expert on cheap publicity stunts, Donald Trump."
>
> "We just learned that Donald Trump has agreed to settle the lawsuits related to Trump University. It's for $25 million dollars."
> "Trump University took in close to $50 million in tuition. Under the RICO statute, the damages are tripled."
> "So if the case goes to trial, he's pushing $200 million in exposure."

>   12. Presidency
> Walt Shaub, former director of the U.S. Office of Government Ethics
> "I worked under three different presidents. Bush, Obama and Trump."
> "There were times when things would get heated with folks from both the Bush and the Obama Administrations."
> "But they always respected you for standing up and doing your job,
>  and in the end, we were always able to work it out."
> "Everything changed when the Trump Administration came in."
> "One of the issues with the Government Ethics program is
>  it's based on an assumption that the president cares about ethics, and he's gonna hold his staff accountable."
> "But from the start, when the president declared he was not going to divest his financial interest,
>  he departed from an ethical norm established by every president before him since the 1970s."
> "One of the biggest scams that President Trump has tried to pull is making it sound like he's stepped back from his businesses.
>  The president misusing the presidency to give free advertising to his own properties whenever possible.
>  It's just the complete monetization of the presidency.
>  The thing I'm most worried about regarding the intersection between the Trump brand and Federal Government is that we now no longer have the ability to assess whether his decisions are based on his policy aims or his financial interests."
> "And then you learn that he's got Trump-branded properties in Turkey and in the Philippines."
> "It was a difficult decision to quit, but it was a lot more difficult to watch the ethics program being dismantled.
>  And I now had been dealing with a White House that was determined to drive a Mack Truck through any loopholes they could find in the ethics program.
>  Every deal I've looked at contains unbelievable ethical lapses and many of the warning signs of criminal activity."
> "But sometimes I wonder, if maybe his greatest fear is, he doesn’t know what he's done.
> There's a concept, called willful blindness.
> You are guilty if you knowingly engage in an illegal action or you're with somebody who's performing an illegal action.
> That doesn't mean you just don't happen to know. It means you have actively structured your organization to stop yourself from finding out."

## 3. [Elections in Taiwan became its largest entertainment shows](https://www.bilibili.com/video/av39823917)

> - Internet gives rise to global prevalence of Populism
> The public focus can be easily distracted by funny things on the Internet, especially those happened to political figures in the context of local/national elections.
> Judicious evaluation over candidates based off their political background, legitimacy of their solutions, etc., is not a social norm.
> It requires formal education, consistent practice of critical thinking, background knowledge around politics and economics, undistorted sources of information, etc.

> - Gambling around election results becomes a solid business in Taiwan

> - Internet narrows people's choices of information intake
> People are more willing to read things appealing to their prior preferences and thus these preferences are further reinforced by such a positive feedback loop which eventually persist to the same level as unchallengable religious believes.
> Internet drastically reduces the opportunity cost for people of similar opinions to group up and solidify each other's opinion with constant positive feedback.

