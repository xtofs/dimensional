package io.xtof.dimensional

package object integers {


  // //////////
  // Boolean
  // https://apocalisp.wordpress.com/2010/06/13/type-level-programming-in-scala-part-3-boolean/

  sealed trait Bool {
    type If[T <: Up, F <: Up, Up] <: Up
  }

  sealed trait True extends Bool {
    type If[T <: Up, F <: Up, Up] = T
  }

  sealed trait False extends Bool {
    type If[T <: Up, F <: Up, Up] = F
  }

  object Bool {
    type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
    type ||[A <: Bool, B <: Bool] = A#If[True, B, Bool]
    type Not[A <: Bool] = A#If[False, True, Bool]
  }


  // balanced ternary digits for +1, 0, -1 (aka Trits)


  sealed trait Digit {
    type Match[Upper, IfPos <: Upper, IfZero <: Upper, IfNeg <: Upper] <: Upper
  }

  object Digit {

    sealed trait _Pos1 extends Digit {
      type Match[Up, IfPos <: Up, IfZero <: Up, IfNeg <: Up] = IfPos
    }

    sealed trait _Zero extends Digit {
      type Match[Up, IfPos <: Up, IfZero <: Up, IfNeg <: Up] = IfZero
    }

    sealed trait _Neg1 extends Digit {
      type Match[Up, IfPos <: Up, IfZero <: Up, IfNeg <: Up] = IfNeg
    }

  }


  // balanced ternary numbers
  // represented as lists of digits (+1, 0, -1) with least significant digit first and no trailing zeros
  // inspired by https://apocalisp.wordpress.com/2010/06/24/type-level-programming-in-scala-part-5a-binary-numbers/

  import Ternary._

  sealed trait Ternary {
    // "data" members
    type digit <: Digit
    type tail <: Ternary

    // operations
    type Match[Upper, Empty <: Upper, Some <: Upper] <: Upper

    type Inc <: Ternary
    type Dec <: Ternary
    type Add[B <: Ternary] <: Ternary
  }

  sealed trait TNil extends Ternary {
    // "data" members

    // operations
    type Match[Upper, Zero <: Upper, NonZero <: Upper] = Zero

    import Digit._

    type Inc = _Pos1 :: TNil
    type Dec = _Neg1 :: TNil

    type Add[B <: Ternary] = B
  }

  sealed trait TCons[HD <: Digit, TL <: Ternary] extends Ternary {
    // "data" members
    type digit = HD
    type tail = TL

    // operations
    type Match[Upper, Zero <: Upper, NonZero <: Upper] = NonZero

    import Digit._

    type Inc = HD#Match[Ternary, _Neg1 :: TL#Inc, _Pos1 :: TL, Times3[TL]]
    type Dec = HD#Match[Ternary, Times3[TL], _Neg1 :: TL, _Pos1 :: TL#Dec]

    type This = TCons[HD,TL]
    
    type Add[B <: Ternary] = B#Match[Ternary, This, AddNE[This, B]]
  }


  object Ternary {

    import Digit._

    type ::[H <: Digit, T <: Ternary] = TCons[H, T]

    //    type :+:[A <: Ternary, B <: Ternary] = A#Add[B]
    //    type +[A <: Ternary, B <: Ternary] = A#Add[B]
    type +[A <: Ternary, B <: Ternary] = A#Add[B]


    //  patterns for Add method:
    //    []   ,   bs    =>  bs
    //    as   ,   []    =>  as
    //
    //    1 :: as, 1 :: bs =>  T :: (as + bs) ++
    //    1 :: as, 0 :: bs =>  1 :: (as + bs)
    //    1 :: as, T :: bs =>  `*3`(as + bs)
    //
    //    0 :: as, 1 :: bs =>  1 :: (as + bs)
    //    0 :: as, 0 :: bs =>  `*3`(as + bs)
    //    0 :: as, T :: bs =>  T :: (as + bs)
    //
    //    T :: as, 1 :: bs =>  `*3`(as + bs)
    //    T :: as, 0 :: bs =>  T :: (as + bs)
    //    T :: as, T :: bs =>  1 :: (as + bs) --

    //    neither A nor B are TNil
    type AddNE[A <: Ternary, B <: Ternary] = A#digit#Match[Ternary,
      B#digit#Match[Ternary,
        _Neg1 :: (A#tail + B#tail)#Inc,
        _Pos1 :: A#tail#Add[B#tail],
        Times3[A#tail#Add[B#tail]]],
      B#digit#Match[Ternary,
        _Pos1 :: A#tail#Add[B#tail],
        Times3[A#tail#Add[B#tail]],
        _Neg1 :: A#tail#Add[B#tail]],
      B#digit#Match[Ternary,
        Times3[A#tail#Add[B#tail]],
        _Neg1 :: A#tail#Add[B#tail],
        _Pos1 :: A#tail#Add[B#tail]#Dec]]


    type Times3[A <: Ternary] = A#Match[Ternary, Zero, _Zero :: A]



    type Zero = TNil
    type Pos1 = _Pos1 :: TNil
    type Pos2 = _Neg1 :: _Pos1 :: TNil
    type Pos3 = _Zero :: _Pos1 :: TNil
    type Pos4 = _Pos1 :: _Pos1 :: TNil
    type Pos5 = _Neg1 :: _Neg1 :: _Pos1 :: TNil
    type Pos6 = _Zero :: _Neg1 :: _Pos1 :: TNil
    type Pos7 = _Pos1 :: _Neg1 :: _Pos1 :: TNil
    type Pos8 = _Neg1 :: _Zero :: _Pos1 :: TNil
    type Pos9 = _Zero :: _Zero :: _Pos1 :: TNil
    type Pos10 = _Pos1 :: _Zero :: _Pos1 :: TNil
    type Pos11 = _Neg1 :: _Pos1 :: _Pos1 :: TNil
    type Pos12 = _Zero :: _Pos1 :: _Pos1 :: TNil
    type Pos13 = _Pos1 :: _Pos1 :: _Pos1 :: TNil
    type Pos14 = _Neg1 :: _Neg1 :: _Neg1 :: _Pos1 :: TNil
    type Pos15 = _Zero :: _Neg1 :: _Neg1 :: _Pos1 :: TNil
    type Pos16 = _Pos1 :: _Neg1 :: _Neg1 :: _Pos1 :: TNil
    type Neg1 = _Neg1 :: TNil
    type Neg2 = _Pos1 :: _Neg1 :: TNil
    type Neg3 = _Zero :: _Neg1 :: TNil
    type Neg4 = _Neg1 :: _Neg1 :: TNil
    type Neg5 = _Pos1 :: _Pos1 :: _Neg1 :: TNil
    type Neg6 = _Zero :: _Pos1 :: _Neg1 :: TNil
    type Neg7 = _Neg1 :: _Pos1 :: _Neg1 :: TNil
    type Neg8 = _Pos1 :: _Zero :: _Neg1 :: TNil
    type Neg9 = _Zero :: _Zero :: _Neg1 :: TNil
    type Neg10 = _Neg1 :: _Zero :: _Neg1 :: TNil
    type Neg11 = _Pos1 :: _Neg1 :: _Neg1 :: TNil
    type Neg12 = _Zero :: _Neg1 :: _Neg1 :: TNil
    type Neg13 = _Neg1 :: _Neg1 :: _Neg1 :: TNil
    type Neg14 = _Pos1 :: _Pos1 :: _Pos1 :: _Neg1 :: TNil
    type Neg15 = _Zero :: _Pos1 :: _Pos1 :: _Neg1 :: TNil
    type Neg16 = _Neg1 :: _Pos1 :: _Pos1 :: _Neg1 :: TNil

    // value conversion

    final class TRep[D <: Ternary](val value: Int)

    implicit def tNilToRep = new TRep[TNil](0)

    implicit def tConsPos1ToRep[T <: Ternary](implicit tail: TRep[T]): TRep[TCons[_Pos1, T]] = new TRep(tail.value * 3 + 1)

    implicit def tConsZeroToRep[T <: Ternary](implicit tail: TRep[T]): TRep[TCons[_Zero, T]] = new TRep(tail.value * 3)

    implicit def tConsNeg1ToRep[T <: Ternary](implicit tail: TRep[T]): TRep[TCons[_Neg1, T]] = new TRep(tail.value * 3 - 1)

    def toInt[T <: Ternary](implicit rep: TRep[T]): Int = rep.value
  }

}
