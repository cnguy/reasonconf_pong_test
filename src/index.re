open Reprocessing;

type point = (float, float);

type ballState = {
  pos: point,
  dx: float,
  dy: float,
};

type pointsState = {
  player1: int,
  player2: int,
};

type state = {
  ball: ballState,
  points: pointsState,
  keyboard: int,
};

let initial_state = {
  ball: {
    pos: (30., 100.),
    dx: 1.,
    dy: 1.,
  },
  points: {
    player1: 0,
    player2: 0,
  },
  keyboard: 75,
};

let width = 200;

let height = 200;

let setup = env => {
  Env.size(~width, ~height, env);
  initial_state;
};

let rect_height = 10;

let rect_width = 50;

let size = 10.;

let rect_y = 20.;

let position_player1 = (state, env) => {
  let x = state.keyboard;
  let max_x = Env.width(env) - rect_width;
  let y = rect_height;
  (min(max_x, x), y);
};

let keyboard_state = (state, env) =>
  switch (Env.key(Events.Left, env), Env.key(Events.Right, env)) {
  | (true, false) => {...state, keyboard: state.keyboard - 1}
  | (false, true) => {...state, keyboard: state.keyboard + 1}
  | _ => state
  };

let position_player2 = env => {
  let (x, _) = Env.mouse(env);
  let max_x = Env.width(env) - rect_width;
  let y = Env.height(env) - rect_height;
  (min(max_x, x), y - 10);
};

let bounce = state => {
  let {ball: {pos: (x, y), dx, dy}} = state;
  let (x, dx) =
    if (x < size || x > float_of_int(width) -. size) {
      (max(size, min(float_of_int(width), x)), -. dx);
    } else {
      (x +. dx, dx);
    };
  let (y, dy) =
    if (y < size +. rect_y || y > float_of_int(height) -. size -. rect_y) {
      (
        max(size +. rect_y, min(float_of_int(height) -. size -. rect_y, y)),
        -. dy,
      );
    } else {
      (y +. dy, dy);
    };
  let point = (x, y);
  let ball = {pos: point, dx, dy};
  {...state, ball};
};

let player1_collision = (state, _env) => {
  let (x, y) = state.ball.pos;
  let keyboard_x = state.keyboard;
  if (y
      -. size == rect_y
      && (
        x < float_of_int(keyboard_x)
        || x > float_of_int(keyboard_x + rect_width)
      )) {
    let points = {
      player1: state.points.player1 + 1,
      player2: state.points.player2,
    };
    {...initial_state, points};
  } else {
    state;
  };
};

let player2_collision = (state, env) => {
  let (x, y) = state.ball.pos;
  let (mouse_x, _) = Env.mouse(env);
  if (y
      +. size == 200.
      -. rect_y
      && (
        x < float_of_int(mouse_x) || x > float_of_int(mouse_x + rect_width)
      )) {
    let points = {
      player1: state.points.player1,
      player2: state.points.player2 + 1,
    };
    {...initial_state, points};
  } else {
    state;
  };
};

let draw = (state, env) => {
  Draw.background(Constants.white, env);
  Draw.fill(Constants.red, env);
  Draw.ellipsef(~center=state.ball.pos, ~radx=size, ~rady=size, env);
  Draw.fill(Constants.blue, env);
  let state = keyboard_state(state, env);
  Draw.rect(
    ~pos=position_player1(state, env),
    ~width=rect_width,
    ~height=rect_height,
    env,
  );
  Draw.rect(
    ~pos=position_player2(env),
    ~width=rect_width,
    ~height=rect_height,
    env,
  );
  let state = bounce(state);
  let state = player1_collision(state, env);
  let state = player2_collision(state, env);
  Draw.text(
    ~body=
      string_of_int(state.points.player1)
      ++ " | "
      ++ string_of_int(state.points.player2),
    ~pos=(5, 90),
    env,
  );
  state;
};

run(~screen="canvas", ~setup, ~draw, ());