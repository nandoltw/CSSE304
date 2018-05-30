(load "chez-init.ss") ; not needed if you are using DrScheme

(define-datatype bintree bintree? 
  [leaf-node 
    (datum number?)]
  [interior-node
    (key symbol?) 
    (left bintree?)
    (right bintree?)])

(define leaf-sum  ; of a bintree
  (lambda (tree)
    (cases bintree tree
      [leaf-node (datum) datum]
      [interior-node (key left right)
        (+ (leaf-sum left) 
           (leaf-sum right))])))

(define inorder ; lisinting of interior nodes in a bintree,
  (lambda (tree)
    (cases bintree tree 
      [leaf-node (datum) '()]
      [interior-node (key left right)
        (append (inorder left)  (list key)  (inorder right))])))

(define list-to-bintree ; construct a bintree from a list representation.
  (lambda (t)
   (cond
    [(number? t)
     (leaf-node t)]
    [(symbol? (car t))
     (interior-node
      (car t)
      (list-to-bintree (cadr t))
      (list-to-bintree (caddr t)))])))

(define t2 (list-to-bintree
               '(a (b 3 4) 5)))

