package scalafp.state

object StateTest {
  import State._

  simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Turn))(Machine(false, 5, 10))
                                                  //> res0: ((Int, Int), ua.scalafp.state.Machine) = ((14,1),Machine(false,1,14))
                                                  //| 
}