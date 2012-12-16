(examples:clear-add-example "alt_eigen" '(
  :code-text (
   :text ("The eigenvectors of " :codecomma "matrix([1,2],[4,5])" " are ")
   :n 1
   :ex ("alt_eigen(matrix([1,2],[4,5]),'var=z);"
         "[z^2=6*z+3,[matrix([2],[z-1])]]")
   :text ("Substituting the two roots of " :code "z^2=6*z+3" " in to the column vector "
          :code "matrix([2],[z-1])" " gives the two eigenvectors. To find explicit "
          " expressions for these eigenvectors, set " :var "maxdegree" " to " :math "2"
          "; thus")
   :ex ("alt_eigen(matrix([1,2],[4,5]),'var=z, 'maxdegree=2);"
        "[z=2*sqrt(3)+3,[matrix([2],[2*sqrt(3)+2])],z=3-2*sqrt(3),
    [matrix([2],[2-2*sqrt(3)])]]")
   :text "Here is a matrix with a degenerate eigenvalue:"
   :ex (("m : matrix([5,6,5,6,5,6],[6,5,6,5,6,5],[5,6,5,6,5,6],[6,5,6,5,6,5],
   [5,6,5,6,5,6],[6,5,6,5,6,5])$"
         nil)
        ("alt_eigen(m,'var=z);"
         "[z=-3,[matrix([-1],[1],[-1],[1],[-1],[1])],z=0,[matrix([0],[-1],[0],[1],[0],[0]),
     matrix([0],[0],[0],[-1],[0],[1]), matrix([0],[0],[1],[0],[-1],[0]),
     matrix([1],[0],[-1],[0],[0],[0])],z=33,[matrix([-1],[-1],[-1],[-1],[-1],[-1])]]"))
   :text ("There are four eigenvectors with eigenvalue " :math "0" ". To find an orthogonal basis for "
          "this eigenspace, set the optional variable " :var "orthogonal" " to true; thus")
   :ex ("alt_eigen(m,'var=z, 'orthogonal=true);"
        "[z=-3,[matrix([-1],[1],[-1],[1],[-1],[1])],z=0,[matrix([1],[0],[-1/2],[0],
    [-1/2],[0]), matrix([0],[0],[1],[0],[-1],[0]),matrix([0],[-1/2],[0],[-1/2],[0],[1]),
    matrix([0],[-1],[0],[1],[0],[0])],z=33,[matrix([-1],[-1],[-1],[-1],[-1],[-1])]]"))))
