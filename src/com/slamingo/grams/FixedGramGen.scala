package com.slamingo.grams


class FixedGramGen( val q : Int ) extends GramGen {
	def gramLength : Int = q
	def gramsOf( str : String ) = {
	  val chars = FixedGramGen.charsOf( str, q )
	  val ret = FixedGramGen.asGrams( chars, q )
	  ret
	}
}

object FixedGramGen {
  private[ FixedGramGen ] def charsOf( str : String, q : Int ) = {
    val res = ( ( q - 1 ) *  '\\') + ( str ) + ( ( q - 1 ) * '/' )
    res
  }
  private[ FixedGramGen ] def asGrams_( s : Stream[ Char ], q : Int  )  : Stream[ Int ] = {
    val (h, t) = s.splitAt( q )
    val h1 = h.hashCode()
    if( t.isEmpty )
      Stream.make( 1, h1 )
    else{
      new Stream[ Int ]{
        override def isEmpty = false
        override def tailDefined = true
        override def head = h1
        override def tail = asGrams_( s.tail, q )
      }
    }
  }
  private[ FixedGramGen ] def asGrams( s : String, q : Int  ) = {
    val ret = for( i <-( 0 to s.length() - q + 1) ) yield {
      val x = s.subSequence( i, i + q - 1 ).hashCode 
      x
    }
    ret
  }
  val FixedGramGen2 = new FixedGramGen( 2 )
  val FixedGramGen3 = new FixedGramGen( 3 )
}


object Test{
  //make FixedGramGen2 visible
  import FixedGramGen.{ FixedGramGen2, FixedGramGen3 }  
  import scala.runtime.ScalaRunTime

  def main( argv : Array[ String ] ){
      val g = FixedGramGen2.gramsOf( "eyal" )
      
      println( ScalaRunTime.stringOf( g ) )
      println( ScalaRunTime.stringOf( FixedGramGen3.gramsOf( "eyal" ) ) )
      
      perfTest()
    }
    def perfTest() = {
      println( "reading file..." )
      val fname = "\\\\ilpchilap\\crap\\names3.csv"
      val ilines = scala.io.Source.fromFile( fname)( "utf-8" ).getLines.map( _.trim() )
      val lines = ilines.toArray
      val n = lines.length
      
      val f = ()=>{ cranch( lines ) }
      println( "working..." )
      val times = ( 1 to 10 ).map( (_) =>{ time( f ) } )
      val avgTimes = times.map( _.toDouble / n )
      val avgTime = (avgTimes.sum) / avgTimes.length
      
      println( ScalaRunTime.stringOf( times ) )
      println( ScalaRunTime.stringOf( avgTimes ) )
      println( avgTime )
      
    }
    def cranch( strs : Array[ String ] ) : Unit = {
      strs.foreach( FixedGramGen2.gramsOf( _ ) )
    }
    
    def time(f: ()=> Unit)={
		val s = System.currentTimeMillis
		f()
		System.currentTimeMillis - s
	}
  }

