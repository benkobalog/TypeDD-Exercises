import Data.Vect

createEmpty : Vect n (Vect 0 elem)
createEmpty = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpty
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                        zipWith (::) x xsTrans


addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys



multMatrix: Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix xs ys = transposeMat (map (multColumn xs) (transposeMat ys))
    where
        multColumn: Num a => Vect n (Vect m a) -> Vect m a -> Vect n a
        multColumn [] ys = []
        multColumn xs ys = map (\x => sum (zipWith (*) x ys)) xs
