import Html exposing (..)
import Html.Events exposing (..)
import Random
import Random.List exposing (shuffle)
import Svg exposing (svg, circle, line, text_)
import Svg.Attributes exposing (..)
import Time


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type Tree
  = Empty
  | BlackEmpty
  | Node Color Tree Int Tree

type Color
  = Red
  | Black
  | BB
  | NB

type alias Model =
  { rbt : Tree
  , insert : { value : Int, node : Tree }
  , delete : { value : Int, node : Tree }
  }


init : (Model, Cmd Msg)
init =
  (Model Empty { value = 0, node = Empty } { value = 0, node = Empty }, randomTree 1 100 18 34)

randomTree min max lenmin lenmax =
  Random.map2 List.take (Random.int lenmin lenmax) (shuffle <| List.range min max)
  |> Random.generate NewTree

-- UPDATE


type Msg
  = Roll
  | NewTree (List Int)
  | Solve


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({rbt, insert, delete} as model) =
  case msg of
    Roll ->
      (model, randomTree 1 100 18 34)

    NewTree xs ->
      case xs of
        ins::del::xs ->
          { rbt = del::xs |> grow
          , insert = { value = ins, node = Empty }
          , delete = { value = del, node = Empty }
          } ! []
        _ -> model ! []

    Solve ->
      Model rbt { insert | node = (tree_ins insert.value rbt) } { delete | node = (tree_del delete.value rbt) } ! []

grow xs =
  case xs of
    x::xs ->
      tree_ins x (grow xs)
    _ -> Empty


tree_ins x t = black_root <| ins x t

black_root r =
  case r of
    Node _ left i right ->
      Node Black left i right
    _ ->
      Empty

ins x t =
  case t of
    Node color left i right ->
      if x < i then
        balance color (ins x left) i right
      else
        balance color left i (ins x right)
    _ ->
      Node Red Empty x Empty

balance color a v b =
  case (color, a, v, b) of
    (Black, (Node Red (Node Red a x b) y c), z, d) -> Node Red (Node Black a x b) y (Node Black c z d)
    (Black, (Node Red a x (Node Red b y c)), z, d) -> Node Red (Node Black a x b) y (Node Black c z d)
    (Black, a, x, (Node Red (Node Red b y c) z d)) -> Node Red (Node Black a x b) y (Node Black c z d)
    (Black, a, x, (Node Red b y (Node Red c z d))) -> Node Red (Node Black a x b) y (Node Black c z d)
    -- for delete
    (BB, (Node Red (Node Red a x b) y c), z, d) -> Node Black (Node Black a x b) y (Node Black c z d)
    (BB, (Node Red a x (Node Red b y c)), z, d) -> Node Black (Node Black a x b) y (Node Black c z d)
    (BB, a, x, (Node Red (Node Red b y c) z d)) -> Node Black (Node Black a x b) y (Node Black c z d)
    (BB, a, x, (Node Red b y (Node Red c z d))) -> Node Black (Node Black a x b) y (Node Black c z d)
    (BB, a, x, (Node NB (Node Black b y c) z (Node Black dl d dr))) ->
      Node Black (Node Black a x b) y (balance Black c z (Node Red dl d dr))
    (BB, (Node NB (Node Black al a ar) x (Node Black b y c)), z, d) ->
      Node Black (balance Black (Node Red al a ar) x b) y (Node Black c z d)
    _ ->
      Node color a v b

isBB t = case t of
  BlackEmpty -> True
  Node BB _ _ _ -> True
  _ -> False

redder color = case color of
  Red -> NB
  Black -> Red
  _ -> Black
redder_tree t = case t of
  Node color left i right -> Node (redder color) left i right
  _ -> Empty

blacker color = case color of
  NB -> Red
  Red -> Black
  _ -> BB
blacker_tree t = case t of
  Node color left i right -> Node (blacker color) left i right
  _ -> BlackEmpty

tree_del x t = black_root <| del x t

del x t = case t of
  Node color left i right ->
    if x < i then
      bubble color (del x left) i right
    else if i < x then
      bubble color left i (del x right)
    else
      remove t
  _ ->
    Empty

bubble color left i right =
  if isBB left || isBB right then
    balance (blacker color) (redder_tree left) i (redder_tree right)
  else
    balance color left i right

remove t = case t of
  Node Black Empty i Empty -> BlackEmpty
  Node Black Empty _ (Node Red left i right) -> Node Black left i right
  Node Black (Node Red left i right) _ Empty -> Node Black left i right
  Node color left i right ->
    let m = max left in
    if m == 0 then
      Empty
    else
      bubble color (removeMax left) m right
  _ -> Empty

max t = case t of
  Node _ _ i Empty -> i
  Node _ _ i right -> max right
  _ -> 0


removeMax t = case t of
  Node _ _ _ Empty -> remove t
  Node color left i right -> bubble color left i (removeMax t)
  _ -> Empty

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

circle_helper : Int -> Color -> Float -> Float -> List (Html Msg)
circle_helper v color row col =
  let px = 50 / 2 ^ row * col |> toString
      py = 8 * row + 4 |> toString
  in
  [ circle [ cx px, cy py, r "3", stroke <| toString color, fill "white", strokeWidth "0.5" ] []
  , text_ [ x px, y py, dy ".35em", textAnchor "middle", fontSize "3" ] [ toString v |> text ]
  ]

line_helper child r1 c1 r2 c2 =
  case child of
    [] -> []
    _ ->
      let
        cx1 = 50 / 2 ^ r1 * c1
        cx2 = 50 / 2 ^ r2 * c2
        cy1 = 8 * r1 + 4
        cy2 = 8 * r2 + 4
        angle = atan2 (cy1 - cy2) (cx1 - cx2)
        px1 = cx1 - 3 * cos angle |> toString
        px2 = cx2 + 3 * cos angle |> toString
        py1 = cy1 - 3 * sin angle |> toString
        py2 = cy2 + 3 * sin angle |> toString
      in
      [line [ x1 px1, y1 py1, x2 px2, y2 py2, strokeWidth "0.5", stroke "black" ] []]

tree_to_svg : Tree -> Float -> Float -> List (Html Msg)
tree_to_svg node row col =
  case node of
    Node color left v right ->
      let left_node = tree_to_svg left (row + 1) (col * 2 - 1)
          right_node = tree_to_svg right (row + 1) (col * 2 + 1)
      in
      List.concat
        [ circle_helper v color row col
        , line_helper left_node row col (row + 1) (col * 2 - 1)
        , line_helper right_node row col (row + 1) (col * 2 + 1)
        , left_node
        , right_node
        ]
    _ -> []

view : Model -> Html Msg
view model =
  div [] <|
    [ svg [ viewBox "0 0 100 75", width "1000px", height "500px" ] (tree_to_svg model.rbt 0 1)
    , div [] [ "請新增 " ++ toString model.insert.value |> text ]
    , div [] [ "請刪除 " ++ toString model.delete.value |> text ]
    , button [ onClick Solve ] [ text "看解答" ]
    , button [ onClick Roll ] [ text "重新出題" ], div [] []
    , svg [ viewBox "0 0 100 100", width "1000px", height "500px" ] (tree_to_svg model.insert.node 0 1)
    , svg [ viewBox "0 0 100 100", width "1000px", height "500px" ] (tree_to_svg model.delete.node 0 1)
    ]
