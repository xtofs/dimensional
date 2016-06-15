package io.xtof.dimensional

import io.xtof.dimensional.integers.Ternary
import org.scalatest.FunSuite

class IntegerSpecs extends FunSuite  {


  def gen() = {
    def d(int:Int) = int.compare(0) match {
      case 1 => s"Pos$int"
      case 0 => s"Zero"
      case -1 => s"Neg${- int}"
    }

    for{
      a <- -9 to 9
      b <- -9 to 9
      c = a + b
      if -10 < c && c < 10
    } {
      println(f"""test(" $a%2d + $b%2d == $c%2d") { implicitly[ ${d(a)} + ${d(b)} =:= ${d(b)} ] } """)
    }
  }


  import Ternary._

  test(" 1") { assert(toInt[Pos1] == 1) }
  test(" 2") { assert(toInt[Pos2] == 2) }
  test(" 3") { assert(toInt[Pos3] == 3) }
  test(" 4") { assert(toInt[Pos4] == 4) }
  test(" 5") { assert(toInt[Pos5] == 5) }
  test(" 6") { assert(toInt[Pos6] == 6) }
  test(" 7") { assert(toInt[Pos7] == 7) }
  test(" 8") { assert(toInt[Pos8] == 8) }
  test(" 9") { assert(toInt[Pos9] == 9) }


  test("-1") { assert(toInt[Neg1] == -1) }
  test("-2") { assert(toInt[Neg2] == -2) }
  test("-3") { assert(toInt[Neg3] == -3) }
  test("-4") { assert(toInt[Neg4] == -4) }
  test("-5") { assert(toInt[Neg5] == -5) }
  test("-6") { assert(toInt[Neg6] == -6) }
  test("-7") { assert(toInt[Neg7] == -7) }
  test("-8") { assert(toInt[Neg8] == -8) }
  test("-9") { assert(toInt[Neg9] == -9) }


  test("0++ ==  1") { implicitly[ Zero#Inc =:= Pos1] }
  test("1++ ==  2") { implicitly[ Pos1#Inc =:= Pos2] }
  test("2++ ==  3") { implicitly[ Pos2#Inc =:= Pos3] }
  test("3++ ==  4") { implicitly[ Pos3#Inc =:= Pos4] }
  test("4++ ==  5") { implicitly[ Pos4#Inc =:= Pos5] }
  test("5++ ==  6") { implicitly[ Pos5#Inc =:= Pos6] }
  test("6++ ==  7") { implicitly[ Pos6#Inc =:= Pos7] }
  test("7++ ==  8") { implicitly[ Pos7#Inc =:= Pos8] }
  test("8++ ==  9") { implicitly[ Pos8#Inc =:= Pos9] }

  test("0-- == -1") { implicitly[ Zero#Dec =:= Neg1 ] }
  test("1-- ==  0") { implicitly[ Pos1#Dec =:= Zero ] }
  test("2-- ==  1") { implicitly[ Pos2#Dec =:= Pos1 ] }
  test("3-- ==  2") { implicitly[ Pos3#Dec =:= Pos2 ] }
  test("4-- ==  3") { implicitly[ Pos4#Dec =:= Pos3 ] }
  test("5-- ==  4") { implicitly[ Pos5#Dec =:= Pos4 ] }
  test("6-- ==  5") { implicitly[ Pos6#Dec =:= Pos5 ] }
  test("7-- ==  6") { implicitly[ Pos7#Dec =:= Pos6 ] }
  test("8-- ==  7") { implicitly[ Pos8#Dec =:= Pos7 ] }
  test("9-- ==  8") { implicitly[ Pos9#Dec =:= Pos8 ] }


  test(" -9 +  0 == -9") { implicitly[ Neg9 + Zero =:= Neg9 ] }
  test(" -9 +  1 == -8") { implicitly[ Neg9 + Pos1 =:= Neg8 ] }
  test(" -9 +  2 == -7") { implicitly[ Neg9 + Pos2 =:= Neg7 ] }
  test(" -9 +  3 == -6") { implicitly[ Neg9 + Pos3 =:= Neg6 ] }
  test(" -9 +  4 == -5") { implicitly[ Neg9 + Pos4 =:= Neg5 ] }
  test(" -9 +  5 == -4") { implicitly[ Neg9 + Pos5 =:= Neg4 ] }
  test(" -9 +  6 == -3") { implicitly[ Neg9 + Pos6 =:= Neg3 ] }
  test(" -9 +  7 == -2") { implicitly[ Neg9 + Pos7 =:= Neg2 ] }
  test(" -9 +  8 == -1") { implicitly[ Neg9 + Pos8 =:= Neg1 ] }
  test(" -9 +  9 ==  0") { implicitly[ Neg9 + Pos9 =:= Zero ] }
  test(" -8 + -1 == -9") { implicitly[ Neg8 + Neg1 =:= Neg9 ] }
  test(" -8 +  0 == -8") { implicitly[ Neg8 + Zero =:= Neg8 ] }
  test(" -8 +  1 == -7") { implicitly[ Neg8 + Pos1 =:= Neg7 ] }
  test(" -8 +  2 == -6") { implicitly[ Neg8 + Pos2 =:= Neg6 ] }
  test(" -8 +  3 == -5") { implicitly[ Neg8 + Pos3 =:= Neg5 ] }
  test(" -8 +  4 == -4") { implicitly[ Neg8 + Pos4 =:= Neg4 ] }
  test(" -8 +  5 == -3") { implicitly[ Neg8 + Pos5 =:= Neg3 ] }
  test(" -8 +  6 == -2") { implicitly[ Neg8 + Pos6 =:= Neg2 ] }
  test(" -8 +  7 == -1") { implicitly[ Neg8 + Pos7 =:= Neg1 ] }
  test(" -8 +  8 ==  0") { implicitly[ Neg8 + Pos8 =:= Zero ] }
  test(" -8 +  9 ==  1") { implicitly[ Neg8 + Pos9 =:= Pos1 ] }
  test(" -7 + -2 == -9") { implicitly[ Neg7 + Neg2 =:= Neg9 ] }
  test(" -7 + -1 == -8") { implicitly[ Neg7 + Neg1 =:= Neg8 ] }
  test(" -7 +  0 == -7") { implicitly[ Neg7 + Zero =:= Neg7 ] }
  test(" -7 +  1 == -6") { implicitly[ Neg7 + Pos1 =:= Neg6 ] }
  test(" -7 +  2 == -5") { implicitly[ Neg7 + Pos2 =:= Neg5 ] }
  test(" -7 +  3 == -4") { implicitly[ Neg7 + Pos3 =:= Neg4 ] }
  test(" -7 +  4 == -3") { implicitly[ Neg7 + Pos4 =:= Neg3 ] }
  test(" -7 +  5 == -2") { implicitly[ Neg7 + Pos5 =:= Neg2 ] }
  test(" -7 +  6 == -1") { implicitly[ Neg7 + Pos6 =:= Neg1 ] }
  test(" -7 +  7 ==  0") { implicitly[ Neg7 + Pos7 =:= Zero ] }
  test(" -7 +  8 ==  1") { implicitly[ Neg7 + Pos8 =:= Pos1 ] }
  test(" -7 +  9 ==  2") { implicitly[ Neg7 + Pos9 =:= Pos2 ] }
  test(" -6 + -3 == -9") { implicitly[ Neg6 + Neg3 =:= Neg9 ] }
  test(" -6 + -2 == -8") { implicitly[ Neg6 + Neg2 =:= Neg8 ] }
  test(" -6 + -1 == -7") { implicitly[ Neg6 + Neg1 =:= Neg7 ] }
  test(" -6 +  0 == -6") { implicitly[ Neg6 + Zero =:= Neg6 ] }
  test(" -6 +  1 == -5") { implicitly[ Neg6 + Pos1 =:= Neg5 ] }
  test(" -6 +  2 == -4") { implicitly[ Neg6 + Pos2 =:= Neg4 ] }
  test(" -6 +  3 == -3") { implicitly[ Neg6 + Pos3 =:= Neg3 ] }
  test(" -6 +  4 == -2") { implicitly[ Neg6 + Pos4 =:= Neg2 ] }
  test(" -6 +  5 == -1") { implicitly[ Neg6 + Pos5 =:= Neg1 ] }
  test(" -6 +  6 ==  0") { implicitly[ Neg6 + Pos6 =:= Zero ] }
  test(" -6 +  7 ==  1") { implicitly[ Neg6 + Pos7 =:= Pos1 ] }
  test(" -6 +  8 ==  2") { implicitly[ Neg6 + Pos8 =:= Pos2 ] }
  test(" -6 +  9 ==  3") { implicitly[ Neg6 + Pos9 =:= Pos3 ] }
  test(" -5 + -4 == -9") { implicitly[ Neg5 + Neg4 =:= Neg9 ] }
  test(" -5 + -3 == -8") { implicitly[ Neg5 + Neg3 =:= Neg8 ] }
  test(" -5 + -2 == -7") { implicitly[ Neg5 + Neg2 =:= Neg7 ] }
  test(" -5 + -1 == -6") { implicitly[ Neg5 + Neg1 =:= Neg6 ] }
  test(" -5 +  0 == -5") { implicitly[ Neg5 + Zero =:= Neg5 ] }
  test(" -5 +  1 == -4") { implicitly[ Neg5 + Pos1 =:= Neg4 ] }
  test(" -5 +  2 == -3") { implicitly[ Neg5 + Pos2 =:= Neg3 ] }
  test(" -5 +  3 == -2") { implicitly[ Neg5 + Pos3 =:= Neg2 ] }
  test(" -5 +  4 == -1") { implicitly[ Neg5 + Pos4 =:= Neg1 ] }
  test(" -5 +  5 ==  0") { implicitly[ Neg5 + Pos5 =:= Zero ] }
  test(" -5 +  6 ==  1") { implicitly[ Neg5 + Pos6 =:= Pos1 ] }
  test(" -5 +  7 ==  2") { implicitly[ Neg5 + Pos7 =:= Pos2 ] }
  test(" -5 +  8 ==  3") { implicitly[ Neg5 + Pos8 =:= Pos3 ] }
  test(" -5 +  9 ==  4") { implicitly[ Neg5 + Pos9 =:= Pos4 ] }
  test(" -4 + -5 == -9") { implicitly[ Neg4 + Neg5 =:= Neg9 ] }
  test(" -4 + -4 == -8") { implicitly[ Neg4 + Neg4 =:= Neg8 ] }
  test(" -4 + -3 == -7") { implicitly[ Neg4 + Neg3 =:= Neg7 ] }
  test(" -4 + -2 == -6") { implicitly[ Neg4 + Neg2 =:= Neg6 ] }
  test(" -4 + -1 == -5") { implicitly[ Neg4 + Neg1 =:= Neg5 ] }
  test(" -4 +  0 == -4") { implicitly[ Neg4 + Zero =:= Neg4 ] }
  test(" -4 +  1 == -3") { implicitly[ Neg4 + Pos1 =:= Neg3 ] }
  test(" -4 +  2 == -2") { implicitly[ Neg4 + Pos2 =:= Neg2 ] }
  test(" -4 +  3 == -1") { implicitly[ Neg4 + Pos3 =:= Neg1 ] }
  test(" -4 +  4 ==  0") { implicitly[ Neg4 + Pos4 =:= Zero ] }
  test(" -4 +  5 ==  1") { implicitly[ Neg4 + Pos5 =:= Pos1 ] }
  test(" -4 +  6 ==  2") { implicitly[ Neg4 + Pos6 =:= Pos2 ] }
  test(" -4 +  7 ==  3") { implicitly[ Neg4 + Pos7 =:= Pos3 ] }
  test(" -4 +  8 ==  4") { implicitly[ Neg4 + Pos8 =:= Pos4 ] }
  test(" -4 +  9 ==  5") { implicitly[ Neg4 + Pos9 =:= Pos5 ] }
  test(" -3 + -6 == -9") { implicitly[ Neg3 + Neg6 =:= Neg9 ] }
  test(" -3 + -5 == -8") { implicitly[ Neg3 + Neg5 =:= Neg8 ] }
  test(" -3 + -4 == -7") { implicitly[ Neg3 + Neg4 =:= Neg7 ] }
  test(" -3 + -3 == -6") { implicitly[ Neg3 + Neg3 =:= Neg6 ] }
  test(" -3 + -2 == -5") { implicitly[ Neg3 + Neg2 =:= Neg5 ] }
  test(" -3 + -1 == -4") { implicitly[ Neg3 + Neg1 =:= Neg4 ] }
  test(" -3 +  0 == -3") { implicitly[ Neg3 + Zero =:= Neg3 ] }
  test(" -3 +  1 == -2") { implicitly[ Neg3 + Pos1 =:= Neg2 ] }
  test(" -3 +  2 == -1") { implicitly[ Neg3 + Pos2 =:= Neg1 ] }
  test(" -3 +  3 ==  0") { implicitly[ Neg3 + Pos3 =:= Zero ] }
  test(" -3 +  4 ==  1") { implicitly[ Neg3 + Pos4 =:= Pos1 ] }
  test(" -3 +  5 ==  2") { implicitly[ Neg3 + Pos5 =:= Pos2 ] }
  test(" -3 +  6 ==  3") { implicitly[ Neg3 + Pos6 =:= Pos3 ] }
  test(" -3 +  7 ==  4") { implicitly[ Neg3 + Pos7 =:= Pos4 ] }
  test(" -3 +  8 ==  5") { implicitly[ Neg3 + Pos8 =:= Pos5 ] }
  test(" -3 +  9 ==  6") { implicitly[ Neg3 + Pos9 =:= Pos6 ] }
  test(" -2 + -7 == -9") { implicitly[ Neg2 + Neg7 =:= Neg9 ] }
  test(" -2 + -6 == -8") { implicitly[ Neg2 + Neg6 =:= Neg8 ] }
  test(" -2 + -5 == -7") { implicitly[ Neg2 + Neg5 =:= Neg7 ] }
  test(" -2 + -4 == -6") { implicitly[ Neg2 + Neg4 =:= Neg6 ] }
  test(" -2 + -3 == -5") { implicitly[ Neg2 + Neg3 =:= Neg5 ] }
  test(" -2 + -2 == -4") { implicitly[ Neg2 + Neg2 =:= Neg4 ] }
  test(" -2 + -1 == -3") { implicitly[ Neg2 + Neg1 =:= Neg3 ] }
  test(" -2 +  0 == -2") { implicitly[ Neg2 + Zero =:= Neg2 ] }
  test(" -2 +  1 == -1") { implicitly[ Neg2 + Pos1 =:= Neg1 ] }
  test(" -2 +  2 ==  0") { implicitly[ Neg2 + Pos2 =:= Zero ] }
  test(" -2 +  3 ==  1") { implicitly[ Neg2 + Pos3 =:= Pos1 ] }
  test(" -2 +  4 ==  2") { implicitly[ Neg2 + Pos4 =:= Pos2 ] }
  test(" -2 +  5 ==  3") { implicitly[ Neg2 + Pos5 =:= Pos3 ] }
  test(" -2 +  6 ==  4") { implicitly[ Neg2 + Pos6 =:= Pos4 ] }
  test(" -2 +  7 ==  5") { implicitly[ Neg2 + Pos7 =:= Pos5 ] }
  test(" -2 +  8 ==  6") { implicitly[ Neg2 + Pos8 =:= Pos6 ] }
  test(" -2 +  9 ==  7") { implicitly[ Neg2 + Pos9 =:= Pos7 ] }
  test(" -1 + -8 == -9") { implicitly[ Neg1 + Neg8 =:= Neg9 ] }
  test(" -1 + -7 == -8") { implicitly[ Neg1 + Neg7 =:= Neg8 ] }
  test(" -1 + -6 == -7") { implicitly[ Neg1 + Neg6 =:= Neg7 ] }
  test(" -1 + -5 == -6") { implicitly[ Neg1 + Neg5 =:= Neg6 ] }
  test(" -1 + -4 == -5") { implicitly[ Neg1 + Neg4 =:= Neg5 ] }
  test(" -1 + -3 == -4") { implicitly[ Neg1 + Neg3 =:= Neg4 ] }
  test(" -1 + -2 == -3") { implicitly[ Neg1 + Neg2 =:= Neg3 ] }
  test(" -1 + -1 == -2") { implicitly[ Neg1 + Neg1 =:= Neg2 ] }
  test(" -1 +  0 == -1") { implicitly[ Neg1 + Zero =:= Neg1 ] }
  test(" -1 +  1 ==  0") { implicitly[ Neg1 + Pos1 =:= Zero ] }
  test(" -1 +  2 ==  1") { implicitly[ Neg1 + Pos2 =:= Pos1 ] }
  test(" -1 +  3 ==  2") { implicitly[ Neg1 + Pos3 =:= Pos2 ] }
  test(" -1 +  4 ==  3") { implicitly[ Neg1 + Pos4 =:= Pos3 ] }
  test(" -1 +  5 ==  4") { implicitly[ Neg1 + Pos5 =:= Pos4 ] }
  test(" -1 +  6 ==  5") { implicitly[ Neg1 + Pos6 =:= Pos5 ] }
  test(" -1 +  7 ==  6") { implicitly[ Neg1 + Pos7 =:= Pos6 ] }
  test(" -1 +  8 ==  7") { implicitly[ Neg1 + Pos8 =:= Pos7 ] }
  test(" -1 +  9 ==  8") { implicitly[ Neg1 + Pos9 =:= Pos8 ] }
  test("  0 + -9 == -9") { implicitly[ Zero + Neg9 =:= Neg9 ] }
  test("  0 + -8 == -8") { implicitly[ Zero + Neg8 =:= Neg8 ] }
  test("  0 + -7 == -7") { implicitly[ Zero + Neg7 =:= Neg7 ] }
  test("  0 + -6 == -6") { implicitly[ Zero + Neg6 =:= Neg6 ] }
  test("  0 + -5 == -5") { implicitly[ Zero + Neg5 =:= Neg5 ] }
  test("  0 + -4 == -4") { implicitly[ Zero + Neg4 =:= Neg4 ] }
  test("  0 + -3 == -3") { implicitly[ Zero + Neg3 =:= Neg3 ] }
  test("  0 + -2 == -2") { implicitly[ Zero + Neg2 =:= Neg2 ] }
  test("  0 + -1 == -1") { implicitly[ Zero + Neg1 =:= Neg1 ] }
  test("  0 +  0 ==  0") { implicitly[ Zero + Zero =:= Zero ] }
  test("  0 +  1 ==  1") { implicitly[ Zero + Pos1 =:= Pos1 ] }
  test("  0 +  2 ==  2") { implicitly[ Zero + Pos2 =:= Pos2 ] }
  test("  0 +  3 ==  3") { implicitly[ Zero + Pos3 =:= Pos3 ] }
  test("  0 +  4 ==  4") { implicitly[ Zero + Pos4 =:= Pos4 ] }
  test("  0 +  5 ==  5") { implicitly[ Zero + Pos5 =:= Pos5 ] }
  test("  0 +  6 ==  6") { implicitly[ Zero + Pos6 =:= Pos6 ] }
  test("  0 +  7 ==  7") { implicitly[ Zero + Pos7 =:= Pos7 ] }
  test("  0 +  8 ==  8") { implicitly[ Zero + Pos8 =:= Pos8 ] }
  test("  0 +  9 ==  9") { implicitly[ Zero + Pos9 =:= Pos9 ] }
  test("  1 + -9 == -8") { implicitly[ Pos1 + Neg9 =:= Neg8 ] }
  test("  1 + -8 == -7") { implicitly[ Pos1 + Neg8 =:= Neg7 ] }
  test("  1 + -7 == -6") { implicitly[ Pos1 + Neg7 =:= Neg6 ] }
  test("  1 + -6 == -5") { implicitly[ Pos1 + Neg6 =:= Neg5 ] }
  test("  1 + -5 == -4") { implicitly[ Pos1 + Neg5 =:= Neg4 ] }
  test("  1 + -4 == -3") { implicitly[ Pos1 + Neg4 =:= Neg3 ] }
  test("  1 + -3 == -2") { implicitly[ Pos1 + Neg3 =:= Neg2 ] }
  test("  1 + -2 == -1") { implicitly[ Pos1 + Neg2 =:= Neg1 ] }
  test("  1 + -1 ==  0") { implicitly[ Pos1 + Neg1 =:= Zero ] }
  test("  1 +  0 ==  1") { implicitly[ Pos1 + Zero =:= Pos1 ] }
  test("  1 +  1 ==  2") { implicitly[ Pos1 + Pos1 =:= Pos2 ] }
  test("  1 +  2 ==  3") { implicitly[ Pos1 + Pos2 =:= Pos3 ] }
  test("  1 +  3 ==  4") { implicitly[ Pos1 + Pos3 =:= Pos4 ] }
  test("  1 +  4 ==  5") { implicitly[ Pos1 + Pos4 =:= Pos5 ] }
  test("  1 +  5 ==  6") { implicitly[ Pos1 + Pos5 =:= Pos6 ] }
  test("  1 +  6 ==  7") { implicitly[ Pos1 + Pos6 =:= Pos7 ] }
  test("  1 +  7 ==  8") { implicitly[ Pos1 + Pos7 =:= Pos8 ] }
  test("  1 +  8 ==  9") { implicitly[ Pos1 + Pos8 =:= Pos9 ] }
  test("  2 + -9 == -7") { implicitly[ Pos2 + Neg9 =:= Neg7 ] }
  test("  2 + -8 == -6") { implicitly[ Pos2 + Neg8 =:= Neg6 ] }
  test("  2 + -7 == -5") { implicitly[ Pos2 + Neg7 =:= Neg5 ] }
  test("  2 + -6 == -4") { implicitly[ Pos2 + Neg6 =:= Neg4 ] }
  test("  2 + -5 == -3") { implicitly[ Pos2 + Neg5 =:= Neg3 ] }
  test("  2 + -4 == -2") { implicitly[ Pos2 + Neg4 =:= Neg2 ] }
  test("  2 + -3 == -1") { implicitly[ Pos2 + Neg3 =:= Neg1 ] }
  test("  2 + -2 ==  0") { implicitly[ Pos2 + Neg2 =:= Zero ] }
  test("  2 + -1 ==  1") { implicitly[ Pos2 + Neg1 =:= Pos1 ] }
  test("  2 +  0 ==  2") { implicitly[ Pos2 + Zero =:= Pos2 ] }
  test("  2 +  1 ==  3") { implicitly[ Pos2 + Pos1 =:= Pos3 ] }
  test("  2 +  2 ==  4") { implicitly[ Pos2 + Pos2 =:= Pos4 ] }
  test("  2 +  3 ==  5") { implicitly[ Pos2 + Pos3 =:= Pos5 ] }
  test("  2 +  4 ==  6") { implicitly[ Pos2 + Pos4 =:= Pos6 ] }
  test("  2 +  5 ==  7") { implicitly[ Pos2 + Pos5 =:= Pos7 ] }
  test("  2 +  6 ==  8") { implicitly[ Pos2 + Pos6 =:= Pos8 ] }
  test("  2 +  7 ==  9") { implicitly[ Pos2 + Pos7 =:= Pos9 ] }
  test("  3 + -9 == -6") { implicitly[ Pos3 + Neg9 =:= Neg6 ] }
  test("  3 + -8 == -5") { implicitly[ Pos3 + Neg8 =:= Neg5 ] }
  test("  3 + -7 == -4") { implicitly[ Pos3 + Neg7 =:= Neg4 ] }
  test("  3 + -6 == -3") { implicitly[ Pos3 + Neg6 =:= Neg3 ] }
  test("  3 + -5 == -2") { implicitly[ Pos3 + Neg5 =:= Neg2 ] }
  test("  3 + -4 == -1") { implicitly[ Pos3 + Neg4 =:= Neg1 ] }
  test("  3 + -3 ==  0") { implicitly[ Pos3 + Neg3 =:= Zero ] }
  test("  3 + -2 ==  1") { implicitly[ Pos3 + Neg2 =:= Pos1 ] }
  test("  3 + -1 ==  2") { implicitly[ Pos3 + Neg1 =:= Pos2 ] }
  test("  3 +  0 ==  3") { implicitly[ Pos3 + Zero =:= Pos3 ] }
  test("  3 +  1 ==  4") { implicitly[ Pos3 + Pos1 =:= Pos4 ] }
  test("  3 +  2 ==  5") { implicitly[ Pos3 + Pos2 =:= Pos5 ] }
  test("  3 +  3 ==  6") { implicitly[ Pos3 + Pos3 =:= Pos6 ] }
  test("  3 +  4 ==  7") { implicitly[ Pos3 + Pos4 =:= Pos7 ] }
  test("  3 +  5 ==  8") { implicitly[ Pos3 + Pos5 =:= Pos8 ] }
  test("  3 +  6 ==  9") { implicitly[ Pos3 + Pos6 =:= Pos9 ] }
  test("  4 + -9 == -5") { implicitly[ Pos4 + Neg9 =:= Neg5 ] }
  test("  4 + -8 == -4") { implicitly[ Pos4 + Neg8 =:= Neg4 ] }
  test("  4 + -7 == -3") { implicitly[ Pos4 + Neg7 =:= Neg3 ] }
  test("  4 + -6 == -2") { implicitly[ Pos4 + Neg6 =:= Neg2 ] }
  test("  4 + -5 == -1") { implicitly[ Pos4 + Neg5 =:= Neg1 ] }
  test("  4 + -4 ==  0") { implicitly[ Pos4 + Neg4 =:= Zero ] }
  test("  4 + -3 ==  1") { implicitly[ Pos4 + Neg3 =:= Pos1 ] }
  test("  4 + -2 ==  2") { implicitly[ Pos4 + Neg2 =:= Pos2 ] }
  test("  4 + -1 ==  3") { implicitly[ Pos4 + Neg1 =:= Pos3 ] }
  test("  4 +  0 ==  4") { implicitly[ Pos4 + Zero =:= Pos4 ] }
  test("  4 +  1 ==  5") { implicitly[ Pos4 + Pos1 =:= Pos5 ] }
  test("  4 +  2 ==  6") { implicitly[ Pos4 + Pos2 =:= Pos6 ] }
  test("  4 +  3 ==  7") { implicitly[ Pos4 + Pos3 =:= Pos7 ] }
  test("  4 +  4 ==  8") { implicitly[ Pos4 + Pos4 =:= Pos8 ] }
  test("  4 +  5 ==  9") { implicitly[ Pos4 + Pos5 =:= Pos9 ] }
  test("  5 + -9 == -4") { implicitly[ Pos5 + Neg9 =:= Neg4 ] }
  test("  5 + -8 == -3") { implicitly[ Pos5 + Neg8 =:= Neg3 ] }
  test("  5 + -7 == -2") { implicitly[ Pos5 + Neg7 =:= Neg2 ] }
  test("  5 + -6 == -1") { implicitly[ Pos5 + Neg6 =:= Neg1 ] }
  test("  5 + -5 ==  0") { implicitly[ Pos5 + Neg5 =:= Zero ] }
  test("  5 + -4 ==  1") { implicitly[ Pos5 + Neg4 =:= Pos1 ] }
  test("  5 + -3 ==  2") { implicitly[ Pos5 + Neg3 =:= Pos2 ] }
  test("  5 + -2 ==  3") { implicitly[ Pos5 + Neg2 =:= Pos3 ] }
  test("  5 + -1 ==  4") { implicitly[ Pos5 + Neg1 =:= Pos4 ] }
  test("  5 +  0 ==  5") { implicitly[ Pos5 + Zero =:= Pos5 ] }
  test("  5 +  1 ==  6") { implicitly[ Pos5 + Pos1 =:= Pos6 ] }
  test("  5 +  2 ==  7") { implicitly[ Pos5 + Pos2 =:= Pos7 ] }
  test("  5 +  3 ==  8") { implicitly[ Pos5 + Pos3 =:= Pos8 ] }
  test("  5 +  4 ==  9") { implicitly[ Pos5 + Pos4 =:= Pos9 ] }
  test("  6 + -9 == -3") { implicitly[ Pos6 + Neg9 =:= Neg3 ] }
  test("  6 + -8 == -2") { implicitly[ Pos6 + Neg8 =:= Neg2 ] }
  test("  6 + -7 == -1") { implicitly[ Pos6 + Neg7 =:= Neg1 ] }
  test("  6 + -6 ==  0") { implicitly[ Pos6 + Neg6 =:= Zero ] }
  test("  6 + -5 ==  1") { implicitly[ Pos6 + Neg5 =:= Pos1 ] }
  test("  6 + -4 ==  2") { implicitly[ Pos6 + Neg4 =:= Pos2 ] }
  test("  6 + -3 ==  3") { implicitly[ Pos6 + Neg3 =:= Pos3 ] }
  test("  6 + -2 ==  4") { implicitly[ Pos6 + Neg2 =:= Pos4 ] }
  test("  6 + -1 ==  5") { implicitly[ Pos6 + Neg1 =:= Pos5 ] }
  test("  6 +  0 ==  6") { implicitly[ Pos6 + Zero =:= Pos6 ] }
  test("  6 +  1 ==  7") { implicitly[ Pos6 + Pos1 =:= Pos7 ] }
  test("  6 +  2 ==  8") { implicitly[ Pos6 + Pos2 =:= Pos8 ] }
  test("  6 +  3 ==  9") { implicitly[ Pos6 + Pos3 =:= Pos9 ] }
  test("  7 + -9 == -2") { implicitly[ Pos7 + Neg9 =:= Neg2 ] }
  test("  7 + -8 == -1") { implicitly[ Pos7 + Neg8 =:= Neg1 ] }
  test("  7 + -7 ==  0") { implicitly[ Pos7 + Neg7 =:= Zero ] }
  test("  7 + -6 ==  1") { implicitly[ Pos7 + Neg6 =:= Pos1 ] }
  test("  7 + -5 ==  2") { implicitly[ Pos7 + Neg5 =:= Pos2 ] }
  test("  7 + -4 ==  3") { implicitly[ Pos7 + Neg4 =:= Pos3 ] }
  test("  7 + -3 ==  4") { implicitly[ Pos7 + Neg3 =:= Pos4 ] }
  test("  7 + -2 ==  5") { implicitly[ Pos7 + Neg2 =:= Pos5 ] }
  test("  7 + -1 ==  6") { implicitly[ Pos7 + Neg1 =:= Pos6 ] }
  test("  7 +  0 ==  7") { implicitly[ Pos7 + Zero =:= Pos7 ] }
  test("  7 +  1 ==  8") { implicitly[ Pos7 + Pos1 =:= Pos8 ] }
  test("  7 +  2 ==  9") { implicitly[ Pos7 + Pos2 =:= Pos9 ] }
  test("  8 + -9 == -1") { implicitly[ Pos8 + Neg9 =:= Neg1 ] }
  test("  8 + -8 ==  0") { implicitly[ Pos8 + Neg8 =:= Zero ] }
  test("  8 + -7 ==  1") { implicitly[ Pos8 + Neg7 =:= Pos1 ] }
  test("  8 + -6 ==  2") { implicitly[ Pos8 + Neg6 =:= Pos2 ] }
  test("  8 + -5 ==  3") { implicitly[ Pos8 + Neg5 =:= Pos3 ] }
  test("  8 + -4 ==  4") { implicitly[ Pos8 + Neg4 =:= Pos4 ] }
  test("  8 + -3 ==  5") { implicitly[ Pos8 + Neg3 =:= Pos5 ] }
  test("  8 + -2 ==  6") { implicitly[ Pos8 + Neg2 =:= Pos6 ] }
  test("  8 + -1 ==  7") { implicitly[ Pos8 + Neg1 =:= Pos7 ] }
  test("  8 +  0 ==  8") { implicitly[ Pos8 + Zero =:= Pos8 ] }
  test("  8 +  1 ==  9") { implicitly[ Pos8 + Pos1 =:= Pos9 ] }
  test("  9 + -9 ==  0") { implicitly[ Pos9 + Neg9 =:= Zero ] }
  test("  9 + -8 ==  1") { implicitly[ Pos9 + Neg8 =:= Pos1 ] }
  test("  9 + -7 ==  2") { implicitly[ Pos9 + Neg7 =:= Pos2 ] }
  test("  9 + -6 ==  3") { implicitly[ Pos9 + Neg6 =:= Pos3 ] }
  test("  9 + -5 ==  4") { implicitly[ Pos9 + Neg5 =:= Pos4 ] }
  test("  9 + -4 ==  5") { implicitly[ Pos9 + Neg4 =:= Pos5 ] }
  test("  9 + -3 ==  6") { implicitly[ Pos9 + Neg3 =:= Pos6 ] }
  test("  9 + -2 ==  7") { implicitly[ Pos9 + Neg2 =:= Pos7 ] }
  test("  9 + -1 ==  8") { implicitly[ Pos9 + Neg1 =:= Pos8 ] }
  test("  9 +  0 ==  9") { implicitly[ Pos9 + Zero =:= Pos9 ] }

}
