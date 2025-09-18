mexpr

lam a.
  lam b.
    (lam n.
       match
         lti n 1
       with
         true
       then
         1
       else
         muli a (recur (subi n 1))) 10
