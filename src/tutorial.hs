{- 
######################
#                    #   
# Active.hs tutorial #
#                    #
######################

In this tutorial we will explain how to use Active.hs commands to produce animations.

----------------

-- Essentials

----------------
-- Things to import

At the top of your Haskell file add the following lines:
-- {- # LANGUAGE DataKinds # -} (remove the -- from this line)

import           Text.Printf

import qualified Animation                   as A
import           Control.IApplicative
import           Control.Monad               (forM_)
import           Data.Active
import           Data.Finitude

import           Diagrams.Backend.Rasterific

----------------
-- Running Actives

Before we dive into Active.hs and making animations, we first need to know how to run Actives. In your terminal cd to the directory that contains your Haskell files, once you get there enter one of the following commands:

1) ghc --make FileName && rm -f out/*.png &&  ./FileName && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..

Replace FileName by the name of the source file you're editing. This command removes and overwrites all existing diagrams from previous Actives.

2) ghc --make Test && ./Test && cd out && ffmpeg -i frame%03d.png -vcodec mpeg4 test.mov && open test.mov && cd ..

Replace FileName by the name of the source file you're editing. This command removes some of already existing files and links the newest diagrams with them. This command can be useful when used correctly. 

Lastly, before you can run any actives, you need the following chunk of code too:

main :: IO ()
main = do
  let frames = simulate 30 activeTitle
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

Replace activeTitle by the active you want to run, and you can always change the number of frames for simulate. It is set to 30 now, but feel free to change it to any non-zero value.

----------------
-- What are Actives and how do they work?

data Active :: * -> Finitude -> * -> * where
  Active   :: Duration f n -> (n -> a) -> Active n f a

Actives, or animations, are dynamic diagrams that change over time. After you produce an animation, when it is compiling you will notice "Linking Test". This is the phase in which for each image produced (look inside your out folder) your PC is linking all these diagrams to produce a movie (.mov format).

An Actives takes in two arguments:
 * A duration that the actives runs for. Durations are either finite or infinite
 * A function (n -> a) that evaluates the active at each point in time, and for that specified value it creates a diagram.

----------------
-- Introduction to finite and infinite actives

To understand how to use Active.hs one needs to understand what is an active, and how it is built in this library. In this library we generalized the term "animation" to "active." Actives are split into two kinds, a finite active, and an infinite active.

Finite actives, as the name suggests, have specified durations. Infinite actives, on the other hand, run forever. 

data Duration f n where
  Duration :: n -> Duration F n
  Forever  ::      Duration I n

Try running the following Active and observe the outcome on your .mov player:

example1 :: A.Animation Double F Rasterific V2 Double
example1 = (\t -> circle 0.5 # fc blue # translateX t) <:$> interval 0 2

You will see a stationary blue circle. The reason for that is how your computer runs the animation. For every frame in which a new diagram is produced, the monitor is adjusting the size ratio to fit each diagram. Since we're running a single diagram, the computer zooms in/out with every frame to make the display better.

Now try running the following Active:

movingCircle :: A.Animation Double F Rasterific V2 Double
movingCircle = (<>) <:$> example <:*> ipure (rect 4 2 # translateX 1 # fc white)

You will observe that the same circle made in "example" is now moving. When we added "ipure (rect 4 2 # translateX 1 # fc white)" to our Active, we ran two actives simultaneously. The presence of the rectangle forces the computer to accomodate having two Actives, especially that the recentagle takes up more space than the circle. Therefore, the movement of the circle is now visible.

Infinite actives are particularly important when they're combined with finite actives. As you will see for the rest of this tutorial we will always be calling an infinite Active called backgound. As a recommendation for all your future work, always have background in your source file.

background :: A.Animation Double I Rasterific V2 Double
background = (ipure (rect 6 6 # fc white))

Tip: Read the type signature carefully. Understanding which functions are useful for finite actives, and which are useful for infinite actives is crucial.

----------------

-- Essential Actives

----------------
The following are actives that we will be using to explain functions, as well as build example. 

background :: A.Animation Double I Rasterific V2 Double
background = (ipure (rect 6 6 # fc white))

circleCircle :: A.Animation Double F Rasterific V2 Double
circleCircle =
  (<>) <:$> (translateX <:$> (sin <:$> interval 0 (2*pi)) <:*> (translateY <:$> (cos <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc red))))
  <:*> background

movingCircleH :: A.Animation Double F Rasterific V2 Double
movingCircleH = (\t -> circle 0.25 # fc green # translateX t) <:$> interval 0 1 

movingCircleV ::A.Animation Double F Rasterific V2 Double
movingCircleV = (\t -> circle 0.25 # fc blue # translateY t) <:$> interval 0 1 

----------------

-- Active.hs functions with examples

----------------
-- Stretch
stretch :: (Num n, Ord n) => n -> Active n F a -> Active n F a
stretch t (Active (Duration n1) f1)
    | t > 0     = Active (Duration n2) f
    | otherwise = error "Can only stretch by rational numbers > 0"
    where
      n2   = t * n1
      f n  = f1 (n*t)

stretch is a function that stretches the duration of an active to a fixed value. The function calculates the factor needed to extent the duration of the actives.

Example:

stretchEx :: A.Animation Double F Rasterific V2 Double
stretchEx = stretch (0.5) movingCircleH

stretchEx will shrink the duration of the active by a factor of (1/2). This function can be used to lengthen the duration of an active by using factors > 1.

----------------
-- (->>)
(->>) is a very important function in building actives. Understanding how it works is key to using it succefully. (Check addDuration in the library)

(->>) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
(Active d1@(Duration n1) f1) ->> (Active d2 f2) = Active (addDuration d1 d2) f
  where
    f n | n <  n1   = f1 n
        | n == n1   = f1 n <> f2 0
        | otherwise = f2 (n - n1)

(->>) allows the user to stitch one active with another active. It is a useful tool when you know the exact order in which the actives should run. Additionally, (->>) has infix 4, that means it has low precedence when compared with other operators (like . and #).

I want to make an animation that has a circle moving in the x direction, then in the y direction (at right angles).

movingCircleRight :: A.Animation Double F Rasterific V2 Double
movingCircleRight = (<>) <:$> movingCircleH <:*> ipure (rect 4 4 # fc white)

movingCircleUP :: A.Animation Double F Rasterific V2 Double
movingCircleUP = (<>) <:$> movingCircleV <:*> ipure (rect 4 4 # fc white)

arrowsEx :: A.Animation Double F Rasterific V2 Double
arrowsEx = movingCircleRight ->> movingCircleUP

This is not the best (or most effecient way) to produce this active, but for now let's try running arrowsEx. You will notice that the circle translated in the x direction, but then it went back to original position before the vertical transformation. The line movingCircleRight ->> movingCircleUP means execute movingCircleRight, then combine it with the movingCircleUP. The computer only understands the order in which the actives are passed. 

When you want an active to combine actives in a certain sequence, you have to tell the program to do the correct translation to the correct location (cooridnate) BEFORE running the second active. Let's fix arrowsEx. 

There are two ways to fixing arrowsEx:
 1) Instead of making two separate actives, use either movingCircleH or movingCircleV with the correct transformations as follows:

arrowsEx2 :: A.Animation Double F Rasterific V2 Double
arrowsEx2 = (<>) <:$> (movingCircleH ->> 
          (movingCircleH # rotate (90 @@ deg) # translateX  1)) <:*> background

 2) TODO: two separate ones combined (not working)

Exercise1: Make a circle move in a trianglular path.

----------------
-- backwards
backwards :: Num n => Active n F a -> Active n F a
backwards (Active (Duration n1) f1) =  Active (Duration n1) f
      where
        f n =  f1 (n1 - n)

backwards runs an active backwards (as the name suggests). To further understand backwards do the following:
 * Run circleCircle and watch the active produced
 * Run backwardsEx and watch the active (notice the difference in duration)

backwardsEx :: A.Animation Double F Rasterific V2 Double
backwardsEx =  circleCircle ->> backwards circleCircle 

----------------
-- simulate
simulate :: (Eq n, Num n, Fractional n, Enum n) => n -> Active n f a -> [a]
simulate 0 _ = error "Frame rate can't equal zero"
simulate n (Active (Duration t1) a1) = map a1 [0, 1/n .. t1]

simulate takes in two arguments, number of frames and an active, 
it produces a list of diagrams that can be linked together to make an animation, or movie. 
Simulate only works for finite actives, as it produces a list of known length. 
We use simulate in every animation.

----------------
-- stretchTo
stretchTo :: (Ord n,  Fractional n) => n -> Active n F a -> Active n F a
stretchTo n (Active (Duration t1) a1) = stretch x (Active (Duration t1) a1)
     where
       x = n/t1

stretchTo is a function that allows you to stretch the duration of an active to a certain length. It is different from stretch (stretch is used in the implementation). 
It is a simple function to read and understand. It takes two arguments, 
the length to which you want to stretch to, and the active to be stretched. Check how it is used in a simple example below.

stretchToEx :: A.Animation Double F Rasterific V2 Double
stretchToEx = stretchTo (pi) circleCircle

stretchToEx takes the active circleCircle and changed its duration to pi, instead of the original duration.

----------------
-- matchDuration
matchDuration :: (Ord n, Fractional n, Num n) => Active n f a -> Active n f a -> Active n f a
matchDuration (Active (Duration t1) a1) (Active (Duration t2) a2) =
    stretch x (Active (Duration t1) a1)
        where
          x = t2 / t1

matchDuration is a function that allows you to match the duration of any two actives. It is important to know the order in which actives are passed. It is very simple and convenient to use. Example below.

matchDurationEx :: A.Animation Double F Rasterific V2 Double
matchDurationEx = matchDuration stretchToEx stretchEx

----------------
-- stack
stack :: (Semigroup a, Num n, Ord n) => [Active n f a] -> Active n f a
stack = sconcat . NE.fromList

stack is a simple function that lets you pass on a list of actives and it will run them all simultaneously. This is particularly useful when you want you don't want to write a single complicated active. You can break it down into pieces and stack them all together. Example below.

movingCircleY :: A.Animation Double F Rasterific V2 Double
movingCircleY = (translateY <:$> (sin <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc yellow)))

movingCircleYO :: A.Animation Double F Rasterific V2 Double
movingCircleYO = (translateY <:$> ((\x -> - sin x) <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc purple)))

movingCircleX :: A.Animation Double F Rasterific V2 Double
movingCircleX = (translateX <:$> (sin <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc blue)))

movingCircleXO :: A.Animation Double F Rasterific V2 Double
movingCircleXO = (translateX <:$> ((\x -> - sin x) <:$> interval 0 (2*pi)) <:*> (ipure (circle 0.25 # fc green)))

collision :: A.Animation Double F Rasterific V2 Double
collision = (<>) <:$> stack [movingCircleY, movingCircleYO, movingCircleX, movingCircleXO] <:*> background

----------------
-- movie
movie :: (Num n, Ord n, Semigroup a) => [Active n F a] -> Active n F a
movie []     = error "Can't make empty movie!"
movie (a:as) = sequenceNE (a :| as)

movie is another simple function that takes in a list of two actives (TODO: only two?) and plays them in order. It is useful when you are making actives that are similar to each other and want to see specific differences. 

movieEx :: A.Animation Double F Rasterific V2 Double
movieEx = movie [arrowsEx, arrowsEx2]

----------------
-- IApplicative instances
instance (Num n, Ord n) => IApplicative (Active n) where
  type Id = I
  type (:*:) i j = Isect i j
  ipure a = Active Forever f
    where
      f _ = a
  (<:*>) (Active t1 f1) (Active t2 f2) = Active (minDuration t1 t2) f3
    where
      f3 t = (f1 t) (f2 t)

These instance of IApplicative are two of the most essential parts of making actives. 
ipure is used to produce infinite actives of constant value, like we saw in background. This can be useful when you need a non-constant actives combined with a cosntant active.

(<:*>) is parallel composition of actives, i.e. it is a function that lets you run actives in parallel at the same time. (<:*>) uses the shorter duration of the two actives. circleCircle in Essential Actives is an example of how to use (<:*>).

----------------
-- (<:>)
(<:>) :: (Semigroup a, Num n, Ord n) => Active n f1 a -> Active n f2 a -> Active n (f1 :*: f2) a
a1 <:> a2 = (<>) <:$> a1 <:*> a2

(<:>) lets you combine actives with different duration types (finite and infinite) together.
It is useful when you're using ipure to run infinite actives, while building some other finite
active. To further understand how to use (<:>), let's use it in an example.

In this example we're going to use actives we are familiar with.

example3 :: A.Animation Double F Rasterific V2 Double
example3 = circleCircle <:> collision

What did you see when you ran example3? You probably saw circleCircle, and that's what you should expect.
Each of the two actives we used already has <:*> background, therefore when we say circleCircle <:> collision
the computer is running both with TWO backgrounds. In order to make the active we're looking for, we're going have to eliminate <:*> background
in either circleCircle, collision, or both. If you choose to eliminate it from both you will have to add it to example3 (or its equivalent).
I am going to introduce a new version of circleCircle, compare it with the older version and think about the difference.

circleCircle2 :: A.Animation Double F Rasterific V2 Double
circleCircle2 =
  (translateX <:$> (cos <:$> interval 0 (4*pi)) <:*> (translateY <:$> (sin <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.25 # fc orange))))

It was easier to eliminate <:*> background from circleCircle and keep it in collision.
Now all I have to do is write this simple active.

combine :: A.Animation Double F Rasterific V2 Double
combine = circleCircle2 <:> collision

Now we can see how the two actives can coexsit with one background.

----------------
-- Other Instances
instance (Semigroup a, Num n, Ord n) => Semigroup (Active n f a) where
  a1 <> a2 = (<>) <:$> a1 <:*> a2

(<>) is a slightly different from (<:>). (<>) runs actives of the same duration type together, 
that is it runs only finite or infinite actives.



----------------
TODO: talk about (<>) and <:$>


----------------

-- How to build complicated actives?

----------------
-- Combining actives
-- overlapping circle

circleCircleBig =
 (translateX <:$> ((\x -> - cos x/2) <:$> interval 0 (4*pi)) <:*> (translateY <:$> ((\x -> - sin x/2) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.5 # fc purple))))

overlapping :: A.Animation Double F Rasterific V2 Double
overlapping = (<>) <:$> stack [circleCircle2, circleCircleBig] <:*> background








-}


