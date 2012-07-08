package com.slamingo.filters
import com.slamingo.grams.GramGen

trait Filter {
	def keyOf( str : String ) : Int
	def boundsForSearch( str : String, gramGen : GramGen, threshold : Int ) : ( Int, Int )
}

object CharSumFilter extends Filter {
  def keyOf( str : String ) : Int = str.map( _.toInt ).sum
  def boundsForSearch( str : String, gramGen : GramGen, threshold : Int ) : ( Int, Int ) = (0,0)   
}

object LengthSumFilter extends Filter {
  def keyOf( str : String ) : Int = str.length
  def boundsForSearch( str : String, gramGen : GramGen, threshold : Int ) : ( Int, Int ) = (0,0)   
}

