package io.xtof.dimensional

import io.xtof.dimensional.integers.{Bool, False, True}
import org.scalatest.FunSuite


class BoolSpecs extends FunSuite  {


  test("all") {
    import Bool._

    implicitly[ (False && False) =:= False ]
    implicitly[ (False && False) =:= False ]
    implicitly[ (False && True ) =:= False ]
    //    implicitly[ (True# && [False]) =:= False ]
    //    implicitly[ (True# && [True ]) =:= True ]
    //
    //    implicitly[ (False# || [False]) =:= False ]
    //    implicitly[ (False# || [True ]) =:= True ]
    //    implicitly[ (True# || [False]) =:= True ]
    //    implicitly[ (True# || [True ]) =:= True ]
    //
    //    implicitly[ False# IfElse[Any, Int, String] =:= String ]
    //    implicitly[ True# IfElse[Any, Int, String] =:= Int ]

  }
}
