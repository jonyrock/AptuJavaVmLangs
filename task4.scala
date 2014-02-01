object main extends App {
  lazy val x : Int = {
    println("gonna get synchronized x")
    777
  }
  lazy val y  : Int = {
    println("gonna get synchronized y")
    val threadLock =  new Thread(new Runnable {
      def run() {
        println("threadLock got locked x"  + x) // it will never happen
      }
    })
    threadLock.start()
    threadLock.join()
    x // just return x
  }
  
  println(y)
  
}
