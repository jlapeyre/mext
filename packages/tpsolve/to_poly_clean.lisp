(defmfun1:set-file-and-package "to_poly_clean.lisp" "tpsolve")

(defmfun1 ($to_poly_clean :doc) ()
  :desc 
  ("Removes the temporary variables created by "
   :emrefdot "to_poly" " These are also created by " :emrefdot "to_poly_solve"
   " They can be created from maxima with " :emrefdot "new_variable"
   " These variables can be found in the infolist " :emrefdot "props" " "
   :mref "to_poly_clean" " returns the number of variables removed.")
  (delete-gentemps))
