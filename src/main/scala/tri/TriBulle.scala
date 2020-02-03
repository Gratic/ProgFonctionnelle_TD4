package tri

import common._


/**
 * Tri Bulle
 *
 * Le tri a bulles ou tri par propagation est un algorithme de tri qui consiste a faire remonter progressivement les plus
 * grands elements d'un tableau, comme les bulles d'air remontent a la surface d'un liquide.
 */


object TriBulle {

  /**
   *  cette fonction compare deux a deux les elements e1 et e2 consecutifs d'une liste et d'effecteur une permutation si
   *  e1 > e2
   *
   *  ( 5 1 4 2 8 ) ->  ( 1 5 4 2 8 )  les elements 5 et 1 sont compares, et comme 5 > 1, l'algorithme les intervertit.
   *  ( 1 5 4 2 8 ) ->  ( 1 4 5 2 8 )  interversion car 5 > 4.
   *  ( 1 4 5 2 8 ) ->  ( 1 4 2 5 8 )  interversion car 5 > 2.
   *  ( 1 4 2 5 8 ) ->  ( 1 4 2 5 8 )  comme 5 < 8, les elements ne sont pas echanges.
   */

  
  def bubble(myList: List[Int]): List[Int] = {
    myList match {
      case first::second::reste if first > second => second::bubble(first::reste)
      case first::second::reste => first::bubble(second::reste)
      case _ => myList
    }
  }

  /**
   * Tri une liste
   * On continue de trier (fonction bulle) jusqu'a ce qu'il n'y ait plus de permutation ( la liste de initial est la meme
   * de la final)
   */

  def bubbleTri(myList: List[Int]): List[Int] = {
    myList match {
      case myList if myList == bubble(myList) => myList
      case myList => bubbleTri(bubble(myList))
    }
  }

  /**
   * Notre fonction trie une liste en ordre croissant. Tres bien.
   * Mais si on voulait trier en ordre decroissant, il faudrait la recoder ? non
   * Refefinissez la fonction bulle avec en parametre de plus une fonction
   */

  def bubble(myList: List[Int], plusGrandQue: (Int, Int) => Boolean): List[Int] = {
    myList match {
      case first::second::reste if plusGrandQue(second, first) => second::bubble(first::reste, plusGrandQue)
      case first::second::reste => first::bubble(second::reste, plusGrandQue)
      case _ => myList
    }
  }

  /**
   * tri croissant avec la nouvelle fonction bubble
   */
  def triDecroissant(myList: List[Int]): List[Int] = {
    myList match {
      case myList if myList == bubble(myList, (x, y) => x > y) => myList
      case myList => triDecroissant(bubble(myList, (x, y) => x > y))
    }
  }

  /**
   * tri decroissant avec la nouvelle fonction bubble
   */
  def triCroissant(myList: List[Int]): List[Int] = {
    myList match {
      case myList if myList == bubble(myList, (x, y) => x < y) => myList
      case myList => triCroissant(bubble(myList, (x, y) => x < y))
    }
  }


  /* Le Polymorphisme est atteint en precisant les variables de type. 
 
	scala> def drop1 [A] (l: List[A]) = l.tail 
	drop1: [A] (l: List[A]) List[A] 

	scala> drop1 (Liste (1,2,3)) 
	res1: Liste [Int] = List (2, 3)
	
	On voi ici que l'on peut utilise drop1 avec une liste de plusieur type*/

  /**
   *  Reecrivez les fonctions precedents avec des type generique
   */
  def bullePoly[A](myList: List[A], plusGrandQue: (A, A) => Boolean): List[A] = {
    myList match {
      case first::second::reste if plusGrandQue(second, first) => second::bullePoly(first::reste, plusGrandQue)
      case first::second::reste => first::bullePoly(second::reste, plusGrandQue)
      case _ => myList
    }
  }

  def triBullePoly[A](myList: List[A], plusGrandQue: (A, A) => Boolean): List[A] = {
    myList match {
      case myList if myList == bullePoly(myList, plusGrandQue) => myList
      case myList => triBullePoly(bullePoly(myList, plusGrandQue), plusGrandQue)
    }
  }

  /**
   * Tri d'une liste de carateres
   */
  def triCroissantChar(myList: List[Char]): List[Char] = {
    triBullePoly(myList, (x :Char, y :Char) => x < y)
  }
}
