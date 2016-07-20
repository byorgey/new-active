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
  let frames = simulate 25 activeTitle
  forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
    renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame

Replace activeTitle by the active you want to run, and you can always change the number of frames for simulate (25 is standard for rasterific in diagrams). It is set to 30 now, but feel free to change it to any non-zero value.

----------------
-- What are Actives and how do they work?

data Active :: * -> Finitude -> * -> * where
  Active   :: Duration f n -> (n -> a) -> Active n f a

Actives, or animations, are dynamic values that change over time. A simple example of an active can be 
a set of transformations for an object. Imagine having a point at the origin, we want to transform it
to the coordinates (1,1). First we make a translation along the horizontal axis by value of 1. If I didn't 
specify the direction, all you know is we're transforming the dot by a value of 1. Now I can alter my transformation
by 90 degrees, and repeat the process. Since my translation changed over time, we can call it an active.

Since the purpuse of this embedded domain specific language is animations, we can look at a more specific definition
of actives. In this tutorial actives are dynamic diagrams that change over time.


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

stretch is a function that stretches an active by the given factor.

Example:

stretchEx :: A.Animation Double F Rasterific V2 Double
stretchEx = stretch (0.5) movingCircleH

stretchEx will shrink the duration of the active by a factor of (1/2). This function can be used to lengthen the duration of an active by using factors > 1.

----------------
-- (->>)
(->>) is a very important function in building actives. Understanding how it works is key to using it succefully. 
(->>) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a


(->>) allows the user to stitch one active with another active. 
It is a useful tool when you know the exact order in which the actives should run. 
Additionally, (->>) has infix 4, that means it has low precedence when compared with other operators 
(like . and #). If you think of actives as continuous timelines, like |---|, then when you say
Active 1 ->> Active 2, ->> operator connects the end point of the first active with the starting point
of the second active ONLY. It does not connect any other points on the timelines.

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

arrowsEx1 :: A.Animation Double F Rasterific V2 Double
arrowsEx1 = (<>) <:$> (movingCircleH ->> 
          (movingCircleH # rotate (90 @@ deg) # translateX  1)) <:*> background

 2) The second method relies on building the horizontal and vertical transformations separately. This method,
however, relies on another operator (->-) that we will introduce in the next section

Exercise1: Make a circle move in a trianglular path.

----------------
-- (->-)
(->-) :: (Semigroup a, Num n, Ord n) => Active n F a -> Active n f a -> Active n f a
To understand how (->-) operates, we need to look at Actives as timelines, just like we did in (->>),
the difference is (->-) produces an active from combining values of the two existing actives. That means
it evaluates both actives at all points (unlike ->>).

Let's solve the problem we had in the last section, we already highlighted one way of solving it. This approach
is a little different. The first step is to setup the transformations:

xTrans :: Active Double F (T2 Double)
xTrans = translationX <:$> interval 0 1

yTrans :: Active Double F (T2 Double)
yTrans = translationY <:$> interval 0 1

Note the type signatures, as well as use of translationX instead of translateX. translateX is a function,
while translationX is of type transformation (for more details go to: http://hackage.haskell.org/package/diagrams-lib-1.3.1.3/docs/Diagrams-TwoD-Transform.html#v:translationX)
xTrans and yTrans produce pure transformations of interval 1. Our next step is to combine them, this is when
(->-) is useful. You can write (xTrans ->- yTrans) to connect the transformations in order.

Lastly, we need to apply the combined transformation to the object we're transforming,
in this case a blue circle of radius 0.25. To do so, we need to use the Haskell function transform.

arrowsEx2 :: A.Animation Double F Rasterific V2 Double
arrowsEx2 = transform <:$> (xTrans ->- yTrans) <:*> ipure(circle 0.25 # fc blue) <:> background

arrowsEx applies transformations xTrans and yTrans to a circle on top of background. This active will produce
the same animation as arrowsEx1.

----------------
-- backwards
backwards :: Num n => Active n F a -> Active n F a

backwards runs an active backwards (as the name suggests). To further understand backwards do the following:
 * Run circleCircle and watch the active produced
 * Run backwardsEx and watch the active (notice the difference in duration)

backwardsEx :: A.Animation Double F Rasterific V2 Double
backwardsEx =  circleCircle ->> backwards circleCircle 

----------------
-- simulate
simulate :: (Eq n, Num n, Fractional n, Enum n) => n -> Active n f a -> [a]

simulate takes in two arguments, number of frames and an active, 
it produces a list of diagrams that can be linked together to make an animation, or movie. 
Simulate only works for finite actives, as it produces a list of known length. 
We use simulate in every animation.

----------------
-- stretchTo
stretchTo :: (Ord n,  Fractional n) => n -> Active n F a -> Active n F a

stretchTo is a function that allows you to stretch the duration of an active to a certain length. 
It is different from stretch (stretch is used in the implementation). 
It is a simple function to read and understand. It takes two arguments, 
the length to which you want to stretch to, and the active to be stretched. 
Check how it is used in a simple example below.

stretchToEx :: A.Animation Double F Rasterific V2 Double
stretchToEx = stretchTo (pi) circleCircle

stretchToEx takes the active circleCircle and changed its duration to pi, instead of the original duration.

----------------
-- matchDuration
matchDuration :: (Ord n, Fractional n, Num n) => Active n f a -> Active n f a -> Active n f a

matchDuration is a function that allows you to match the duration of any two actives. 
It is important to know the order in which actives are passed. 
It is very simple and convenient to use. Example below.

matchDurationEx :: A.Animation Double F Rasterific V2 Double
matchDurationEx = matchDuration stretchToEx stretchEx

----------------
-- stack
stack :: (Semigroup a, Num n, Ord n) => [Active n f a] -> Active n f a

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

movie is another simple function that takes in a list of actives and plays them in order. It is useful when you are making actives that are similar to each other and want to see specific differences. 

movieEx :: A.Animation Double F Rasterific V2 Double
movieEx = movie [arrowsEx, arrowsEx2]

----------------
-- (<:>)
(<:>) :: (Semigroup a, Num n, Ord n) => Active n f1 a -> Active n f2 a -> Active n (f1 :*: f2) a

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
that is it runs only finite or infinite actives. We used (<>) in circlecCircle and collision. 
(<>) is usually accompnied by another operator <:$>, this operator maps a function onto the active.

Note: remember that prefix operators take parenthesis, while infix operators do not.

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

-- How to build complicated actives?

----------------
-- Type signature of actives
exActive :: A.Animation Double F Rasterific V2 Double

So far we've seen a type signature similar to that of exActive. Before we dive into more complicated
actives, we need to understand the meaning of each term (in order). 

A.Animation: to identify animations

Double: Is for the duration of the active

F/I: This is the part where you specify if your active is finite of infinite. Infinite actives are linked
to ipure. If you want to make a nonchanging rectangle you can say something like: ipure(rect 2 1 # fc blue)

Rasterific: This has to do with the backend rendering process. Rasterific produces a .png for each frame

V2: Two deimensional diagrams and actives

Double: displacement within the diagram, change of coordinates from initial to final position for each frame.

----------------
-- Combining actives
-- overlapping circles

When you start thinking about building more intricate and complicated actives, I advise you to think about
building pieces of the active seprately, then putting them together using tools at your disposal.
By the end of this tutorial, we want to show you how to build a model of a solar system, with moons orbiting planets
and planets orbiting the sun.

Let's start by making a simple version of circles orbiting one another. I am going to use actives that
we have used before. To make two circles orbit one another, they both need to move in a elliptical path.
Standard parametric equations of an ellpise are: x = A cos(t), y = B sin(t).

My first step will be using circleCircle2 and modifying it in a way that fits what I am trying to make.
Guess and check works for finding values that yield simple orbital paths. 

circleCircle2M :: A.Animation Double F Rasterific V2 Double
circleCircle2M =
  (translateX <:$> ((\x ->  2 * cos x) <:$> interval 0 (4*pi)) <:*> (translateY <:$> 
  ((\x -> 1 * sin x) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.15 # fc green))))  

My second step is to modify equations of an ellipse to make my central circle (the sun) also move in an
elliptical path, except I want its displacement to be small compared to circlecircle2M.

circleCircleSun :: A.Animation Double F Rasterific V2 Double
circleCircleSun =
 (translateX <:$> ((\x -> - cos x/2) <:$> interval 0 (4*pi)) <:*> 
 (translateY <:$> ((\x -> - sin x/2) <:$> interval 0 (4*pi)) <:*> (ipure (circle 0.5 # fc orange))))

Lastly, I want to combine all three using functions we've introduced. Keep in mind that none of the pieces
should be connected with background, we want to connect ALL of them to the same background.

rotating :: A.Animation Double F Rasterific V2 Double
rotating = (<>) <:$> stack [circleCircle2, circleCircleSun, circleCircle2M] <:*> background

While building a solar system is our goal from this tutorial, 
the example we just developed could be improved. Just hang tight.

----------------
-- dur
Our last example took too much work, now we want to make life easier. Even though splitting actives into
smaller pieces then combining them is the best appraoch to more complicated actives, it is still inconvenient 
to have to specify the interval for each piece. Giving the pieces infinite intervals simplifies this process.

dur :: Active n I n

dur is a very simple function that gives actives infinite intervals. A simple example is having a circle
moving in some path. The following is a perfectly good example that you SHOULD NOT attempt.

durEx :: A.Animation Double I Rasterific V2 Double
durEx = (translateX <:$> ((\t -> cos (t/2)) <:$> dur) <:*> (translateY <:$> 
  ((\t -> cos (3*t/2))<:$> dur) <:*> (ipure (circle 0.25 # fc black))) <:> background)

durEx will compile, but it will also produce an infinite number of frames (don't believe me check your out directory).
The reason I have this example is that I want you to realize the importance of stiching pieces together. <:$> dur can be
a very useful tool if you know how to use it. We still want to make
a solar system, we will be using <:$> dur in doing so.

----------------
-- cut
cut :: (Num n, Ord n) => n -> Active n f a -> Active n F a

cut is a simple function that allows you to cut (or stop) an animation at any point shorter than the duration of an active.
This is useful when you are trying to look at specific instances of an active. Follow the example below:

blackRect :: A.Animation Double F Rasterific V2 Double
blackRect = (\t -> rect 2 2 # fc black # translateX t) <:$> interval 0 2

movingRect :: A.Animation Double F Rasterific V2 Double
movingRect = (<>) <:$> blackRect <:*> ipure (rect 4 2 # translateX 1 # fc white)

Run movingRect. What did you see? Now change your main to: let frames = simulate 30 (cut 1 movingRect)

Notice the difference?

----------------
-- Building a solar system

In this section of the tutorial we are going to focus on using all of the pieces we have introduced, in making
a solar system. As we progress, we are going to be using actives called solar1, solar2, ... 
The idea is every time we add a planet we changeto the system, we make a new active that accomodates it.

The best approach is to make each planet using ipure, then applying any transformations and translations. 
Let's start by making all of our planets and sun:

earth :: A.Animation Double I Rasterific V2 Double
earth = ipure(circle 0.5 # fc blue)

mercury :: A.Animation Double I Rasterific V2 Double
mercury = ipure(circle 0.25 # fc pink)

venus :: A.Animation Double I Rasterific V2 Double
venus = ipure(circle 0.45 # fc purple)

mars :: A.Animation Double I Rasterific V2 Double
mars = ipure(circle 0.35 # fc deeppink)

jupiter :: A.Animation Double I Rasterific V2 Double
jupiter = ipure(circle 1 # fc peachpuff)

saturn :: A.Animation Double I Rasterific V2 Double
saturn = ipure(circle 0.85 # fc green)--fc navajowhite)

uranus :: A.Animation Double I Rasterific V2 Double
uranus = ipure(circle 0.7 # fc deepskyblue)

neptune :: A.Animation Double I Rasterific V2 Double
neptune = ipure(circle 0.65 # fc dodgerblue)

pluto :: A.Animation Double I Rasterific V2 Double
pluto = ipure(circle 0.15 # fc yellow)
 
sun :: A.Animation Double I Rasterific V2 Double
sun = ipure (circle 1.4 # fc orange) 

For all the colors available go to http://hackage.haskell.org/package/colour-2.3.3/docs/Data-Colour-Names.html

Our second step is to make and add a moon to earth (and other planets). You might think you can use <:> in solar1
to apply translations to the moon and earth, but a better approach is to make earth with moon as one active, 
then apply transformations to that active.

moon :: A.Animation Double I Rasterific V2 Double
moon =    (translateX <:$> ((\x -> 0.7 * cos (3*x/2)) <:$> dur) <:*> (translateY <:$> 
  ((\x -> 0.7 * sin (3*x/2))<:$> dur) <:*> (ipure (circle 0.1 # fc green # lc red))))

earthwM :: A.Animation Double I Rasterific V2 Double
earthwM = (<>) <:$> earth <:*> moon

marswM :: A.Animation Double I Rasterific V2 Double
marswM =(<>) <:$> mars <:*> moon2


You can calculate the numbers in front of earthwM (or use guess and check). Now let's make solar1

solar1 :: A.Animation Double I Rasterific V2 Double
solar1 = (translateX <:$> ((\x -> 2 * cos x) <:$> dur) <:*> 
         (translateY <:$> ((\x -> 2 * sin x) <:$> dur) <:*> 
         (earthwM)) <:> sun <:> background)

Notice that this is an infinite active. It's easier to deal with actives of same type. To avoid infinite frames
and compiling time, I recommend you use cut in your main. solar1 looks good, let's add more planets!

solar2 :: A.Animation Double I Rasterific V2 Double
solar2 = (translateX <:$> ((\x -> (6/2) * cos x) <:$> dur) <:*> 
         (translateY <:$> ((\x -> (6/2) * sin x) <:$> dur) <:*> 
         (earthwM)) <:>
         (translateX <:$> ((\x -> (4/2) * cos x) <:$> dur) <:*>
         (translateY <:$> ((\x -> (4/2) * sin x) <:$> dur) <:*>
         (mercury))  )  <:> sun2 <:> background )

In solar2 we added mercury to earthwM. The first two lines of solar2 apply transfromations to an infinite active
(<:$> dur makes it infinite). Then we combined these transfromations with another set of transformations using
<:> operator. Now you can see a pattern for adding planets. I am going to leave adding other planets as an exercise.

Note: the colors do not match actual colors of planets. We picked colors that make all planets visible.
----------------
-- Adding background image

The animation you have has a white still background. You can change the background to an image through a few simple steps:
1) download the image and save it in same directory you're working in (src usually).

2) change your main function:
main :: IO ()
main = do
  pic <- loadImageEmb "fileName.jpg"
  case pic of
    Left st -> putStrLn st
    Right img -> do
        let background2 ::  Diagram B
            background2 =  image img # sized (mkWidth 55) 
            frames = simulate 25 (cut 20  activeName <:> ipure(background2)) 
        forM_ (zip [0 :: Int ..] frames) $ \(i,frame) -> do
          renderRasterific (printf "out/frame%03d.png" i) (mkWidth 400) frame


-}


