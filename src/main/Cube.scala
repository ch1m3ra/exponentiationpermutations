package main

import scala.collection.mutable.HashMap
import scala.math.BigDecimal

/** A class to help find N permutations of exponentiation.
 * 
 * @constructor : create a new object to find roots for a specific exponent.
 * @param : exponent is the power the values are raised to.
 * @param : rootFunction is the function used to calculate the root.
 */
class Root(val exponent: Int, val rootFunction: Double => Double) {
  /** finds all of the permutations of the base to the exponent power value that result in whole number roots.
   * 
   * @param : base is the number used to calculate all permutations.
   * @return : List[Int] of whole number roots.
   * @exceptions : None
   * @sideEffects : None
   */
  def findAllRootPermutations(base: Int) = {
    val result = HashMap[Int, Int]()
    val cubedValue = BigDecimal(base).pow(exponent).toBigInt.toString

	for(permutation <- cubedValue.permutations) {
	  // Ignore all strings that start with '0'
	  if(permutation.head != '0') {
		val testResult = rootFunction(BigInt(permutation).toDouble)
		val testInt = testResult.intValue

		if(testResult - testInt == 0.00) {
		  result.get(testInt) match {
		    case Some(count) => result.put(testInt, count + 1)
		    case None => result.put(testInt, 1)
	}}}}

	result.keys.toArray.sorted
  }

  /** finds the smallest root which has N permutations of the exponentiation whose root 
   *  	results in whole numbers.
   * 
   * @param : permutationsNeeded is how many permutations are needed.
   * @param	 : exactMatch if true then the function will only return if permutations
   * 	found length is exactly permtuationsNeed, otherwise it is treated as a minimum lower bound.
   * @param	 : startingValue to start checking from.
   * @param : maxIterations to check.
   * @return : Option[List[Int]] if conditions were satisfied before maxIterations were exceeded
   * 	then a List[Int] is returned or else None
   * @exceptions : None
   * @sideEffects : None
   */
  def findSmallestRootWithNPermutations(permutationsNeeded: Int, exactMatch: Boolean, startingValue: Int = 1, maxIterations: Int = 10000): Option[Array[Int]] = {
	def compareEquals(testValue: Int) = permutationsNeeded == testValue
	def compareAtLeast(testValue: Int) = permutationsNeeded <= testValue
	  
	/*
	 * Partial application to allow testing for an exact number of matches or at least n matches without having to perform 
	 * the decision every loop
	 */
	val test = exactMatch match {
	  case true => compareEquals _
	  case false => compareAtLeast _
	}
    
    for(offset <- 0 to maxIterations) {
	  val resultReturned = findAllRootPermutations(startingValue + offset)
	  if(test(resultReturned.length)) {
		return Some(resultReturned)
	}}

    None
  }
  
  /** finds the smallest exponentiation which has N permutations of the exponentiation whose root 
   *  	results in whole numbers.
   * 
   * @param : permutationsNeeded is how many permutations are needed.
   * @param	 : exactMatch if true then the function will only return if permutations
   * 	found length is exactly permtuationsNeed, otherwise it is treated as a minimum lower bound.
   * @param	 : startingValue to start checking from.
   * @param	 : maxIterations to check.
   * @return : Option[BigInt] if conditions were satisfied before maxIterations were exceeded
   * 	then a BigInt is returned or else None
   * @exceptions : None
   * @sideEffects : None
   */
  def findSmallestValueWithNPermutations(permutationsNeeded: Int, exactMatch: Boolean, startingValue: Int = 1, maxIterations: Int = 10000): Option[BigInt] = {
    findSmallestRootWithNPermutations(permutationsNeeded, exactMatch, startingValue, maxIterations) match {
      case Some(result) => Some(BigDecimal(result.head).pow(exponent).toBigInt)
      case None => None
    }
  }
}

/** Creates a singleton of the Root class configured for cubes. */
object Cube extends Root(3, math.cbrt)

/** Inferences:
 *  	+ Duplicate exponentiations do not count towards the goal.
 *   	+ Exponentiations with leading zeros are discarded.
 *    
 *  Possible speed improvements:
 * 		+ See if it is possible to leverage the fact that we need whole number roots. Maybe using Binary Search.
 *  	+ If there is a lot of RAM we can store the permutations of cubed value then check to see if it is 
 *   		faster to generate all the permutations first in a HashMap.
 *   	+ Utilize parallelization
 */
object Run extends App {
  println("---- 3 permutations ----")
  println(Cube.findSmallestValueWithNPermutations(3, true))

  println("---- 5 permutations ----")
  println(Cube.findSmallestValueWithNPermutations(5, true))
}
