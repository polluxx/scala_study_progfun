package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("------")
    println("Balance List")
    println(" ())( : ", balance("())(".toList))
    println(" ()() : ", balance("()()".toList))
    println(" :-) : ", balance(":-)".toList))
    println(" (if (zero? x) max (/ 1 x)) : ", balance("(if (zero? x) max (/ 1 x))".toList))
    println(" I told him (that it’s not (yet) done). (But he wasn’t listening): ",
      balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(" Some string without parentheses : ", balance("Some string without parentheses".toList))


    println("------")
    println("Counting Change")
    println("0 CHF, 1 coin, denominations:", countChange(0, List(1)))
    println("2 CHF, no coins, denominations:", countChange(2, List()))
    println("3 CHF, 1 coin, denominations:", countChange(3, List(1)))
    println("4 CHF, 1,2 coin, denominations:", countChange(4, List(1,2)))
    println("10 CHF, 1,2,4,5 coin, denominations:", countChange(10, List(1,2,4,5)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c == 0 || r == c) 1
      else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
      {
        var opened: List[Char] = List()
        val charsFixed: List[Char] = chars.filter(p => p.toString.equals("(") || p.toString.equals(")"))
        def looper(charsTail: List[Char]): List[Char] = {
          if (charsTail.isEmpty) return opened

          val head = charsTail.head
          if(head.toString().equals(")") && opened.isEmpty) return head :: opened

          def checkFunc(headChar: Char): List[Char] = {
              if(headChar.toString().equals("(")) return headChar :: opened
              else {
                if(headChar.toString().equals(")") && !opened.isEmpty) return opened.dropRight(1)
              }
              return opened
          }
          opened = checkFunc(head)
          looper(charsTail.tail)
        }

        looper(charsFixed).isEmpty
      }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money <= 0 || coins.isEmpty) return 0

      def changePicker(coinsTail: List[Int], variants: List[List[Int]]): Int = {
        if(coinsTail.isEmpty) {
          return variants.distinct.length
        }
        val coin = coinsTail.head
        val tail = coinsTail.tail

        def summer(args: List[Int], founded: List[List[Int]]): List[List[Int]] = {
          if (args.isEmpty) founded
          else {
            val variant = adder(args, List(coin), 0)

//            println("variant", variant)
//            println("variantLEN", variant.length)
            val newFound = if (variant.length > 0) (variant :: founded) else founded
            summer(args.tail, newFound)
          }
        }

        def adder(args: List[Int], added: List[Int], index: Int): List[Int] = {
          if(added.sum == money) return added

          if(added.sum > money) return adder(args, added.dropRight(1), index-1)

          if(index < 0) return List()

          val newadd = args(index) :: added

          adder(args, newadd, index)
        }
        val vars = summer(coins, List())
        val variantsLen = variants ::: vars

        changePicker(tail, variantsLen)
      }

      changePicker(coins, List())
    }
  }
