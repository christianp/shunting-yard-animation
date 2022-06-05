module Parameters exposing (..)

{-
Constants to do with track pieces.

For each kind of piece, we need to know:
* its length
* the straight-line vector from its start to its end (recorded as _width and _height)
-}

-- The base length of track pieces. The ShortHorizontal piece is this long, and it's the radius of any turns.
track_length = 75

-- A tiny bit of horizontal track.
tiny_track_length = track_length - turn_width

-- A medium-sized bit of horiontal track, the same width as a long up/down junction.

mid_track_length = 2*turn_width

-- A sufficiently long piece of horizontal track to go off the screen.
incredibly_long_track_length = 15 * track_length

cos45 = cos (degrees 45)

-- The x- and y-distance between the ends of a one-eighth turn piece.
turn_width = track_length * cos45
turn_height = track_length * (1 - cos45)

-- The length of a one-eighth turn
curve_length = 2 * pi * track_length / 8

-- The length of a diagonal piece
diagonal_length = (sqrt 2)*turn_width

-- An upward/downward junction part: turn upwards one-eighth of a turn, then a straight diagonal, then turn downwards back to horizontal.
long_junction_width = 3*turn_width
long_junction_height = 2*turn_height + turn_width

-- A shorter junction part: omit the diagonal.
short_junction_height = 2*turn_height

-- A U-turn: half a turn.
uturn_length = pi*track_length
uturn_height = 2*track_length

-- A sufficiently long vertical piece of track to go off the screen.
vertical_length = 3*track_length


-- The distance between the rails
track_gauge = 20

-- The lengths of train and carriage pieces are defined in terms of the track gauge
train_length = track_gauge * 3
carriage_length = track_gauge * 2

-- Parameters for the train's movement.
max_speed = 1
accelerate_distance = track_length
accelerate_time = 2*max_speed*accelerate_distance
acceleration = 0.5 / accelerate_distance

-- The distance between the fronts of each of the carriages in a queue
carriage_gap = carriage_length * 1.2

