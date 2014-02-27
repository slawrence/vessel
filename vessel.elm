import Mouse
import Keyboard
import Window
import Random

shipStartY = -200
startPieceH = 100
startWidth = 200
minWidth = 50
startSpeed = 500
speedDelta = 0.5

data State = Waiting | Playing | Dead

-- Inputs
type Input = { dir:Int, delta:Float, rand:Int, space:Bool}
delta = inSeconds <~ fps 35

input = sampleOn delta (Input <~ lift .x Keyboard.arrows
                               ~ delta
                               ~ (Random.range 0 10 (every second))
                               ~ Keyboard.space)
-- Models
type Ship = { x:Float, y:Float, vx:Float, vy:Float }
type Tunnel = { width:Float, speed:Float, x:Float, y:Float, h:Float, ampl:Float }
type Piece = { x: Float, y: Float, vx:Float, vy:Float, width:Float, height:Float }
type Game = { cnt:Int,
              ship:Ship,
              tunnel:Tunnel,
              pieces:[Piece],
              score:Int,
              state:State}
curve : Int -> Float -> Float
curve x ampl =
  let angle = (degrees (toFloat x)) * 2
  in (cos (3*angle) + sin (2*angle)) * ampl

defaultGame : Game
defaultGame =
  { cnt    = 0,
    ship   = { x=0, y=shipStartY, vx=0, vy=0 },
    tunnel = { width=startWidth, speed=startSpeed, x=(curve 0 (toFloat 20)), y=300, h=startPieceH, ampl=20 },
    pieces = map (\n -> { x=(curve n (toFloat 20)), y=(toFloat (n + 30))*10, vx=0, vy=-startSpeed, width=startWidth, height=startPieceH }) [-60..0],
    score  = 0,
    state  = Waiting }

updateAmpl cur max = if (cur < max) then cur + 1 else cur

updateTunnel : Int -> Game -> Tunnel
updateTunnel rand game =
  let tunnel = game.tunnel
      state = game.state
      speed = if game.state == Playing then tunnel.speed + speedDelta else tunnel.speed
      ampl = if game.state == Playing then updateAmpl tunnel.ampl 80 else tunnel.ampl
      nx = curve game.cnt ampl
      nwidth = if (tunnel.width < minWidth || state == Waiting) then tunnel.width else tunnel.width - 0.1
  in
    { tunnel | x <- nx
             , width <- nwidth
             , speed <- speed
             , ampl <- ampl
             , h <- tunnel.h + speedDelta }

stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

stepShip : Time -> Int -> Ship -> Ship
stepShip t dir ship =
  let ship1 = stepObj t { ship | vx <- toFloat dir * 300 }
  in ship1

filterPiece piece = piece.y > -400

stepPiece : Time -> Int -> Game -> [Piece]
stepPiece t rand game =
  addPiece rand game
   |> map (stepObj t)
   |> filter (filterPiece)


addPiece : Int -> Game -> [Piece]
addPiece rand game =
  let nx = game.tunnel.x
      ny = game.tunnel.y
      speed = game.tunnel.speed
      nwidth = game.tunnel.width
  in
      if game.cnt `mod` 1 == 0
      then { x=nx, y=ny, vx=0, vy=-speed, width=nwidth, height=startPieceH } :: game.pieces
      else game.pieces

--withinN : Int -> Float -> Float -> Bool
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

stepDead : Input -> Game -> Game
stepDead {dir,delta,rand,space} game = if space
                          then { defaultGame | state <- Playing }
                          else game

stepWaiting : Input -> Game -> Game
stepWaiting {dir,delta,rand,space} game = if space
                          then { game | state <- Playing }
                          else { game | pieces <- stepPiece delta rand game
                                      , cnt <- (\n -> n + 1) game.cnt
                                      , tunnel <- updateTunnel rand game}

stepGame : Input -> Game -> Game
stepGame {dir,delta,rand,space} game =
  { game | pieces <- stepPiece delta rand game
         , cnt <- (\n -> n + 1) game.cnt
         , tunnel <- updateTunnel rand game
         , ship <- stepShip delta dir game.ship
         , score <- (\n -> n + 1) game.score
         , state <- updateState game }

stepStart input game = case game.state of
          Playing -> stepGame input game
          Dead -> stepDead input game
          Waiting -> stepWaiting input game

gameState = foldp stepStart defaultGame input

-- DISPLAY
drawPiece : Piece -> [Form]
drawPiece piece = [rect piece.width piece.height
    |> filled lightRed
    |> move (piece.x, piece.y)]

drawShip ship =
  let sx = ship.x
      sy = ship.y
  in [ ngon 3 10 |> filled white -- draw ship
            |> rotate (degrees 90)
            |> move (sx, sy) ]

displayTextLeft game =
    let text = case game.state of
        Playing -> (plainText "Vessel")
        Waiting -> (plainText "Press Space")
        Dead -> (plainText "Press Space")
    in toForm text |> move (-200, 260)

display (w,h) game =
  collage w h <|
    [ rect 500 500 |> filled darkRed ] ++
    concatMap drawPiece game.pieces ++
    drawShip game.ship ++
    [ rect 500 200 |> filled white |> move (0, 350)
    , rect 500 200 |> filled white |> move (0, -350)
    , displayTextLeft game
    , toForm (asText game.score) |> move (220, 260) ]

main = lift2 display Window.dimensions gameState


