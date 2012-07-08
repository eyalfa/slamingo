package com.slamingo.grams

trait GramGen {
	def numGrams( str :String ) : Int = str.length + this.gramLength - 1 
	def gramLength : Int
	def gramsOf( str : String ) : Seq[ Int ]
}