def hello := "world"
def world := "hello"

infix:arg "s+" => String.append

def together := world s+ hello
#eval together

def joinStringsWith (between : String) (first : String) (second : String) : String :=
  String.append first (String.append between second)

#check joinStringsWith ": "

def volume (height : Nat) (width : Nat) (depth : Nat) : Nat :=
  height * width * depth

#eval volume 1 1 1
#eval volume 2 4 5

def S : Type := String

def thing : String := "hi"

def NaturalNumber : Type := Nat
abbrev N : Type := Nat

def a : NaturalNumber := (56 : Nat)
def b : N := 56


structure RectangularPrism where
  h : Float
  w : Float
  d : Float

def RectangularPrism.volume (prism : RectangularPrism) : Float :=
  prism.h * prism.w * prism.d

def somePrism : RectangularPrism := { h := 1, w := 4, d := 2.5 }
#eval somePrism.volume

inductive Firewood where
  | birch
  | pine
  | beech
deriving Repr

#eval [Firewood.beech, Firewood.beech]


def last {α : Type} (l : List α) : Option α :=
  match l with
    | [] => Option.none
    | s :: ls => (last ls).or s

#eval last [57, 23, 56]
#eval last ([] : List Empty)

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => Option.none
  | s :: ls => if predicate s then Option.some s else ls.findFirst? predicate

def take {α : Type} (xs : List α) (n : N) :=
  if n = 0 then [] else
  if xs.length <= n then xs else
  match xs with
  | [] => [] -- this is provable unreachable, but don't know the syntax for that
  | List.cons a ls => List.cons a (take ls (n - 1))

#eval take [1, 2, 3, 4, 5, 6] 2
#eval take [1, 2, 3, 4, 5, 6] 7
#eval take [1, 2, 3, 4, 5, 6] 6
#eval take [1, 2, 3, 4, 5, 6] 0


#eval s!"Some string {volume 3 5 7}"
