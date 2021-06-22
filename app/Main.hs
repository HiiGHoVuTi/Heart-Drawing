import Graphics.Proc

width     = 400
height    = 400
duration  = 5
step_size = 0.05

tick_to_screen :: Float -> Float
tick_to_screen tick = rmap tick
  where
    max_tick = 60 * duration
    rmap     = rangemap 0 (duration*60) 0 width

rangemap :: Float -> Float -> Float -> Float -> Float -> Float
rangemap omin omax nmin nmax x = nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

screen_to_graph :: Float -> Float
screen_to_graph = rangemap 0 width (-1.3) 1.3

graph_to_screen :: Float -> Float
graph_to_screen = rangemap (-1.3) 1.3 0 width

setup :: Pio Int
setup = do
  size (width, height)
  strokeFill (Col 1 0 0.4 1)
  background (grey 0)
  return 0

draw t = do
  let
    proper_time = ( cos (2 * pi * float t / (2 * duration * 60) + pi) / 2 + 0.5 ) * duration * 60
    looper      = if t `mod` (int duration * 60 * 2) < int duration * 60
      then [0, step_size..proper_time]
      else [duration*60, duration*60 - step_size .. proper_time]
  background (grey 0)
  forM_ looper draw_vertex

get_vertex t = let
      screen_x = tick_to_screen t
      x        = screen_to_graph screen_x
      y        = sqrt (cos $ 4/3 * x) * (sin $ 1000 * x) + sqrt (abs $ 3 / 5 * x) - 0.4 * ((4 - x * x) *^ 0.01)
      screen_y = graph_to_screen y
    in (screen_x, height - (screen_y) + offset)
  where
    offset = rangemap 0 400 0 height 30

draw_vertex t = line (get_vertex t) (get_vertex $ t + step_size)

update :: Int -> Pio Int
-- update t = return $ (t + 1) `mod` (int duration * 60)
update t = return $ t + 1

main = runProc $ def
    { procSetup  = setup
    , procDraw   = draw
    , procUpdate = update }
