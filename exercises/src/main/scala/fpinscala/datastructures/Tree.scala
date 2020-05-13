package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

def size[A](tree: Tree[A]): Int = {
    tree match {
        case Leaf(_) => 1
        case Branch(l,r) => size(l) + size(r)
    }
}

def maximum(tree: Tree[Int]): Int = {
    tree match {
        case Leaf(value) => value
        case Branch(left, right) => maximum(left).max(maximum(right))
        // val maxL = maximum(left)
        // val maxR = maximum(right)
        // if(maxL > maxR) maxL
        // else maxR
    }
}

def depth[A](tree: Tree[A]): Int = {
    tree match {
        case Leaf(value) => 1
        case Branch(left, right) => depth(left).max(depth(right)) + 1 //one level deeeper though each iteration
    }
}

def map[A,B](tree: Tree[A])(f:A=>B): Tree[B] = {
    tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f),map(right)(f))
    }
}

def fold[A,B](tree: Tree[A])(f:A=>B)(h:(Tree[A], Tree[A])=>B): B = {
    tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => h(left,right)
    }
}

def fold2[A,B](tree: Tree[A])(f:A=>B)(h:(B,B)=>B): B = {
    tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => h(fold2(left)(f)(h), fold2(right)(f)(h))
    }
}

def foldSize[A](tree:Tree[A]): Int = fold(tree)(_ => 1)(foldSize(_) + foldSize(_))

def foldSize2[A](tree: Tree[A]): Int = fold2(tree)(_ => 1)(_ + _)

def foldMap[A,B](tree: Tree[A])(f:A=>B):Tree[B] = {
    fold2[A, Tree[B]](tree)(value => Leaf(f(value)))(Branch(_,_))
}

}

object TreeMain {
  def main(args: Array[String]): Unit = {
    
    val smallTree = Leaf(123)
    val tree = 
    Branch(
      Branch(
        Leaf(1),
        Leaf(2)
      ),
      Leaf(3)
    )
    
    println("size")
    println(Tree.size(smallTree))
    println(Tree.size(tree))

    println("maximum")
    println(Tree.maximum(smallTree))
    println(Tree.maximum(tree))

    println("depth")
    println(Tree.depth(smallTree))
    println(Tree.depth(tree))

    println("map")
    println(Tree.map(smallTree)(_ + 1))
    println(Tree.map(tree)(_ * 2))

    println("foldsize")
    println(Tree.foldSize(smallTree))
    println(Tree.foldSize2(smallTree))
    println(Tree.foldSize(tree))
    println(Tree.foldSize2(tree))

    println("foldmap")
    println(Tree.foldMap(smallTree)(_ + 1))
    println(Tree.foldMap(tree)(_ * 2))
  }
}