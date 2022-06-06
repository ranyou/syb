
{-# LANGUAGE RankNTypes, DeriveAnyClass #-}

module Gmap where

import Unsafe.Coerce (unsafeCoerce)

-- =============================================================================
-- Data Structures
-- =============================================================================

data Company    = C [Dept]
data Dept       = D Name Manager [SubUnit]
data SubUnit    = PU Employee | DU Dept
data Employee   = E Person Salary
data Person     = P Name Address
data Salary     = S Float
type Manager    = Employee
type Name       = String
type Address    = String

instance Show Company where
    show (C departments) = "Company:\n" ++ concat (map show departments)
instance Show Dept where
    show (D name manager subunits) = 
        "Department: " ++ name ++ "\n\tManager: " ++ show manager ++ 
        concat (map show subunits)
instance Show SubUnit where
    show (PU employee) = "\t\t" ++ show employee
    show (DU department) = "\t\t" ++ show department
instance Show Employee where
    show (E p s) =  "Employee: " ++ (show p) ++ " " ++ (show s) ++ "\n"
instance Show Person where
    show (P name address) = name ++ " " ++ address
instance Show Salary where
    show (S salary) = show salary

genCom :: Company
genCom = C [
    D "Research" ralf [PU joost, PU marlow, DU (D "Science" joost [])], 
    D "Strategy" blair []
    ] 

ralf, joost, marlow, blair :: Employee
ralf = E (P "Ralf" "Amsterdam") (S 8000)
joost = E (P "Joost" "Amsterdam") (S 1000)
marlow = E (P "Marlow" "Cambridge") (S 2000)
blair = E (P "Blair" "London") (S 100000)

-- an example of increase without using generic programming
increase :: Float -> Company -> Company
increase k (C ds) = C (map (incD k) ds)

incD :: Float -> Dept -> Dept
incD k (D nm mgr us) = D nm (incE k mgr) (map (incU k) us)
incU :: Float -> SubUnit -> SubUnit
incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)
incE :: Float -> Employee -> Employee
incE k (E p s) = E p (incS k s)
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))

-- =============================================================================
-- Typeable Class
-- =============================================================================

class Typeable a where
    typeOf :: a -> TypeRep

data TypeRep = TR String [TypeRep]
    deriving (Eq, Show)

instance Typeable Company where
    typeOf x = TR "Prelude.Company" []
instance Typeable Dept where
    typeOf x = TR "Prelude.Dept" []
instance Typeable SubUnit where
    typeOf x = TR "Prelude.SubUnit" []
instance Typeable Employee where
    typeOf x = TR "Prelude.Employee" []
instance Typeable Person where
    typeOf x = TR "Prelude.Person" []
instance Typeable Salary where
    typeOf x = TR "Prelude.Salary" []

instance Typeable Char where
    typeOf x = TR "Prelude.Char" []
instance Typeable Int where
    typeOf x = TR "Prelude.Int" []
instance Typeable Float where
    typeOf x = TR "Prelude.Float" []
instance Typeable Bool where
    typeOf x = TR "Prelude.Bool" []
instance Typeable a => Typeable [a] where
    typeOf x = TR "Prelude.List" [typeOf (get x)]
        where 
            get :: [a] -> a
            get = undefined
instance (Typeable a, Typeable b) => Typeable (a -> b) where
    typeOf f = TR "Prelude.->" [typeOf (getArg f), typeOf (getRes f)]
        where 
            getArg :: (a->b) -> a
            getArg = undefined
            getRes :: (a->b) -> b
            getRes = undefined

-- type casting function
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
    where
        r = if typeOf x == typeOf (get r)
            then Just (unsafeCoerce x)
            else Nothing
        get :: Maybe a -> a
        get x = undefined

-- =============================================================================
-- Term Class
-- =============================================================================

class Typeable a => Term a where
    gmapT :: (forall b. Term b => b -> b) -> a -> a
    gmapQ :: (forall b. Term b => b -> r) -> a -> [r]
    gmapM :: Monad m => (forall b. Term b => b -> m b) -> a -> m a

instance Term Char where
    gmapT f x = x
    gmapQ f x = []
    gmapM f x = return x
instance Term Bool where
    gmapT f x = x
    gmapQ f x = []
    gmapM f x = return x
instance Term Float where
    gmapT f x = x
    gmapQ f x = []
    gmapM f x = return x

instance Term Company where
    gmapT f (C depts) = C (map f depts)
    gmapQ f (C depts) = map f depts
    gmapM f (C depts) = 
        do 
            depts' <- f depts
            return (C depts)
instance Term Dept where
    gmapT f (D n m us) = D (f n) (f m) (map f us)
    gmapQ f (D n m us) = [f n, f m, f us]
    gmapM f (D n m us) = 
        do 
            n' <- f n
            m' <- f m
            us' <- f us
            return (D n' m' us')
instance Term SubUnit where
    gmapT f (PU e) = PU (f e)
    gmapT f (DU d) = DU (f d)
    gmapQ f (PU e) = [f e]
    gmapQ f (DU d) = [f d]
    gmapM f (PU e) = 
        do 
            e' <- f e
            return (PU e')
    gmapM f (DU d) = 
        do 
            d' <- f d
            return (DU d')
instance Term Employee where
    gmapT f (E p s) = E (f p) (f s)
    gmapQ f (E p s) = [f p, f s]
    gmapM f (E p s) = 
        do
            p' <- f p
            s' <- f s
            return (E p' s')
instance Term Person where
    gmapT f (P name addr) = P (f name) (f addr)
    gmapQ f (P name addr) = [f name, f addr]
    gmapM f (P name addr) = 
        do 
            name' <- f name
            addr' <- f addr
            return (P name' addr')
instance Term Salary where
    gmapT f (S s) = S (f s)
    gmapQ f (S s) = [f s]
    gmapM f (S s) = 
        do 
            s' <- f s 
            return (S s')

instance Term a => Term [a] where
    gmapT f [] = []
    gmapT f (x:xs) = f x : f xs
    gmapQ f [] = []
    gmapQ f (x:xs) = [f x, f xs]
    gmapM f [] = return []
    gmapM f (x:xs) = 
        do 
            x' <- f x
            xs' <- f xs
            return (x':xs')

-- =============================================================================
-- Generic Transformations
-- =============================================================================

increase' :: Float -> Company -> Company
increase' k = everywhere (mkT (incS k))

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f = case cast f of 
            Just g -> g
            Nothing -> id

-- Apply a transformation everywhere, bottom-up
everywhere :: Term a => (forall b. Term b => b -> b) -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

-- Apply a transformation everywhere, top-down
everywhere' :: Term a => (forall b. Term b => b -> b) -> a -> a
everywhere' f x = gmapT (everywhere' f) (f x)

-- Another Example

flatten :: Name -> Company -> Company
flatten d = everywhere (mkT (flatD d))

flatD :: Name -> Dept -> Dept
flatD d (D n m us) = D n m (concatMap unwrap us)
    where 
        unwrap :: SubUnit -> [SubUnit]
        unwrap (DU (D d' m us)) | d == d' = PU m : us
        unwrap u = [u]

-- =============================================================================
-- Generic Queries
-- =============================================================================

salaryBill :: Company -> Float
salaryBill = everything (+) (0 `mkQ` billS)

billS :: Salary -> Float
billS (S f) = f

mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` q) a = case cast a of
    Just b -> q b
    Nothing -> r

-- Summarise all nodes in top-down, left-to-right
everything :: Term a => (r -> r -> r)
    -> (forall a. Term a => a -> r)
    -> a -> r
everything k f x = 
    foldl k (f x) (gmapQ (everything k f) x)

-- Another Example

find :: Name -> Company -> Maybe Dept
find n = everything orElse (Nothing `mkQ` findD n)

findD :: String -> Dept -> Maybe Dept
findD n d@(D n' _ _) 
    | n == n' = Just d
    | otherwise = Nothing
orElse :: Maybe a -> Maybe a -> Maybe a
x `orElse` y = case x of
    Just _ -> x
    Nothing -> y

-- =============================================================================
-- Monadic Transformations
-- =============================================================================

mkM :: (Typeable a, Typeable b, Typeable (m a), Typeable (m b), Monad m)
    => (b -> m b) -> a -> m a
mkM f = case cast f of
    Just g -> g
    Nothing -> return

everywhereM :: (Monad m, Term a) => (forall b. Term b => b -> m b) -> a -> m a
everywhereM f x = do 
        x' <- gmapM (everywhereM f) x 
        f x'

lookupSalaries :: Company -> IO Company
lookupSalaries = everywhereM (mkM lookupE)

lookupE :: Employee -> IO Employee
lookupE (E p@(P n _) _) = 
    do { s <- dbLookup n; return (E p s) }

-- lookup the person in the external database
dbLookup :: Name -> IO Salary
dbLookup name = case name of
    "Ralf" -> return (S 100.0)
    "Joost" -> return (S 200.0)
    "Marlow" -> return (S 300.0)
    "Blair" -> return (S 400.0)
    _ -> return (S 0.0)
