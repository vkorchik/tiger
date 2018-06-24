type key = string
datatype 'a tree = LEAF | TREE of 'a tree * key * 'a * 'a tree
val empty = LEAF

fun insert (LEAF, key, a) = TREE(empty, key, a, empty)
  | insert (TREE(l, k, ak, r), key, a) =
                 if key < k
                   then TREE(insert(l, key, a), k, ak, r)
                 else if key > k
                   then TREE(l, k, ak, insert(r, key, a))
                 else TREE(l, k, a, r)

fun member (key, LEAF) = false
  | member (key, TREE(l,k,_,r)) =
                 if key < k
                   then member(key, l)
                 else if key > k
                   then member(key, r)
                 else true
 
fun lookup (LEAF, key) = NONE
  | lookup (TREE(l,k,a,r), key) =
                 if key < k
                   then lookup(l, key)
                 else if key > k
                   then lookup(r, key)
                 else SOME(a)
