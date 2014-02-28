import Keyboard
import Window

-- Important game properties
shipStartY = -200
startPieceH = 100
startWidth = 200
minWidth = 50
startSpeed = 500
speedDelta = 0.5


-- Inputs
type Input = { dir:Int, delta:Float, space:Bool}
delta = inSeconds <~ fps 35
input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ delta
                               ~ Keyboard.space)
-- Models
type Ship = { x:Float, y:Float, vx:Float, vy:Float }
type Tunnel = { width:Float, speed:Float, x:Float, y:Float, h:Float, ampl:Float }
type Piece = { x: Float, y: Float, vx:Float, vy:Float, width:Float, height:Float }
type Debri = { x: Float, y: Float, vx:Float, vy:Float, size:Float }
type Game = { cnt:Int,
              ship:Ship,
              t:Tunnel,
              debri:[Debri],
              pieces:[Piece],
              score:Int,
              state:State}
data State = Waiting | Playing | Dead

-- initial state
defaultGame =
  { cnt    = 0,
    ship   = { x=0, y=shipStartY, vx=0, vy=0 },
    t = { width=startWidth, speed=startSpeed, x=curve 0 (toFloat 20), y=300, h=startPieceH, ampl=20 },
    debri = [],
    pieces = map (\n -> { x=curve n (toFloat 20), y=(toFloat (n + 30))*10, vx=0, vy=-startSpeed, width=startWidth, height=startPieceH }) [-60..0],
    score  = 0,
    state  = Waiting }

-- Updates
curve : Int -> Float -> Float
curve cnt ampl =
  let fcnt = toFloat cnt
      degree = (degrees fcnt) * 2
      segment = (floor (fcnt / 200)) `mod` 7 -- one more than # of segs
  in case segment of
      0 -> (sin (degree * 4)) * ampl
      1 -> (cos (degree * 6) + sin (2*degree)) * ampl
      2 -> (cos (degree * 3) + sin (2*degree)) * ampl
      3 -> 200
      4 -> (cos (degree * 3) + sin (2*degree)) * ampl
      5 -> (cos (degree) + cos (degree*3)) * ampl
      6 -> 0

towards target x =
    let xdelta = (target - x)
        neg = (xdelta < 0)
    in x + (xdelta / 30)

updateAmpl cur max = if (cur < max) then cur + 1 else cur

updateTunnel : Game -> Tunnel
updateTunnel game =
  let t = game.t
      state = game.state
      speed = if game.state == Playing then t.speed + speedDelta else t.speed
      ampl = if game.state == Playing then updateAmpl t.ampl 180 else t.ampl
      next = curve game.cnt ampl
      nx = if (withinN 2 next t.x) then next else towards next t.x
      nwidth = if (t.width < minWidth || state == Waiting) then t.width else t.width - 0.1
  in
    { t | x <- nx
             , width <- nwidth
             , speed <- speed
             , ampl <- ampl
             , h <- t.h + speedDelta }

stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
  let ship1 = stepObj t { ship | vx <- toFloat dir * 360 }
  in ship1

filterPiece piece = piece.y > -400

stepPiece : Time -> Game -> [Piece]
stepPiece t game =
  addPiece game
   |> map (stepObj t)
   |> filter (filterPiece)

stepDebri t cnt ship debri = map (stepObj t) (addDebri ship cnt debri)

addD sx sy n =
    let vx = sin (degrees (toFloat n)) * 150
        vy = cos (degrees (toFloat n)) * 150
    in {x=sx, y=sy, vx=vx, vy=vy, size=10}

addDebri ship cnt debri =
  let l = length debri
  in if (l == 0)
     then (map (addD ship.x ship.y) (foldl (\n a -> if n `mod` 8 == 0 then [n] ++ a else a ) [] [0..360])) ++ debri
     else debri

addPiece : Game -> [Piece]
addPiece game =
  let nx = game.t.x
      ny = game.t.y
      speed = game.t.speed
      nwidth = game.t.width
      h = (800 / (toFloat (length game.pieces))) + 50
  in
      if game.cnt `mod` 1 == 0
      then { x=nx, y=ny, vx=0, vy=-speed, width=nwidth, height=h } :: game.pieces
      else game.pieces

withinN offset px sx = (sx > px - offset) && (sx < px + offset)

inside : Ship -> Piece -> Bool
inside ship piece =
  let halfW = piece.width / 2
  in
  withinN halfW piece.x ship.x

updateState : Game -> State
updateState game =
  let pieces = game.pieces |> filter (\p -> withinN 60 p.y game.ship.y)
  in if any (inside game.ship) pieces
     then Playing
     else Dead

hideShip s = { s | x <- -30
                 , y <- 400 }

autoShip : Tunnel -> Ship -> Ship
autoShip t s = { s | x <- towards t.x s.x }

stepDead : Input -> Game -> Game
stepDead {dir,delta,space} game = if space
                          then { defaultGame | state <- Playing }
                          else { game | debri <- stepDebri delta game.cnt game.ship game.debri
                                      , ship <- hideShip game.ship
                                      , cnt <- (\n -> n + 1) game.cnt }

stepWaiting : Input -> Game -> Game
stepWaiting {dir,delta,space} game = if space
                          then { defaultGame | state <- Playing }
                          else { game | pieces <- stepPiece delta game
                                      , cnt <- (\n -> n + 1) game.cnt
                                      , ship <- autoShip game.t game.ship
                                      , t <- updateTunnel game}

stepGame : Input -> Game -> Game
stepGame {dir,delta,space} game =
  { game | pieces <- stepPiece delta game
         , cnt <- (\n -> n + 1) game.cnt
         , t <- updateTunnel game
         , ship <- stepShip delta dir game.ship
         , score <- (\n -> n + 1) game.score
         , state <- updateState game }

stepStart input game = case game.state of
          Playing -> stepGame input game
          Dead -> stepDead input game
          Waiting -> stepWaiting input game

gameState = foldp stepStart defaultGame input

-- DISPLAY
drawPiece piece = [rect piece.width piece.height |> filled lightRed
                                                 |> move (piece.x, piece.y)]

drawDebri d = [square d.size |> filled white |> move (d.x, d.y)]

drawShip ship = [ ngon 3 10 |> filled white
                            |> rotate (degrees 90)
                            |> move (ship.x, ship.y) ]

txt f = text . f . monospace . Text.color white . toText
displayText game =
    let text = case game.state of
        Playing -> ""
        Dead    -> "Failure."
        _       -> "Space to start then arrows"
    in text

display (w,h) game =
  collage w h <|
    [ rect 500 500 |> filled darkRed ] ++
    concatMap drawPiece game.pieces ++
    drawShip game.ship ++
    concatMap drawDebri game.debri ++
    [ rect 900 200 |> filled white |> move (0, 350)
    , rect 900 200 |> filled white |> move (0, -350)
    , rect 200 700 |> filled white |> move (-350, 100)
    , rect 200 700 |> filled white |> move (350, 100)
    , toForm (txt (Text.height 15) (displayText game))
    , toForm (asText game.score) |> move (220, 260) ]

main = lift2 display Window.dimensions gameState

