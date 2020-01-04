--import System.IO
--import Text.Show.Unicode
import Data.Monoid   -- use Sum
import Data.Foldable
import Data.Traversable

menus :: String -> [(Int,String)]
menus "マクドナルド" = [(390,"ビッグマック"), (340,"ダブルチーズバーガー"), (200,"スパチキ"), (110,"チキンクリスプ")]
menus "サイゼリヤ" = [(299,"ミラノ風ドリア"), (399,"マルゲリータピザ"), (299,"辛味チキン")]
menus "Ippudo" = [(790,"Shiromaru Classic"), (850,"Akamaru Modern")]
menus "Matsuya" = [(380,"Regular Premium Gyumeshi"), (490,"Regular Matsuya Tender Beef Curry & Rice"), (600,"Barbecued Beef Set Meal")]
menus _ = []

main = do
     putStrLn $ id "== Applicative"
     -- class Functor f => Applicative f where
     --    pure :: a -> f a
     --    (<*>) :: f (a -> b) -> f a -> f b
     -- 
     -- instance Applicative [] where
     --    pure :: a -> [a]
     --    pure x = [x]
     --
     --    (<*>) :: [a -> b] -> [a] -> [b]
     --    gs <*> xs = [g x | g <- gs, x <- xs]
     --
     print $ pure (+1) <*> [100]
     print $      (+1) <$> [100]            -- same
     print $ pure (+)  <*> [100] <*> [200]
     print $      (+)  <$> [100] <*> [200]  -- same
     print $ pure (+1) <*> [10,20]
     print $ pure (+)  <*> [10,20] <*> [1,2]
     print $ pure (\x y -> x + y) <*> [10,20] <*> [1,2] -- same
     print $ pure (\x y -> if y then x else 0) <*> [10,20] <*> [True,False] -- [10,0,20,0]
     print $ pure (&&) <*> [True,False] <*> [True,False]

     putStrLn $ id "== Traaversable"
     -- class (Functor t, Foldable t) => Traversable t where
     --    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
     --
     -- instance Traversable [] where
     --    traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
     --    traverse g [] = pure []
     --    traverse g (x:xs) = pure (:) <*> g x <*> traverse g xs`
     --
     print $ traverse id [[1,2,3],[4,5]]
     print $ traverse id ([1,2,3],[4,5])
     print $ traverse (\x -> [1..x])  [1,2,3]
     -- print $ traverse id ((1,2,3),(4,5,6)) -- error
     print $ (traverse (\x -> menus x) ["Ippudo","Matsuya"])
     print $ (traverse (\x -> menus x) ["Ippudo","Matsuya","xxx"]) -- []
     print $ (traverse (\x -> menus x) (Just "Ippudo"))
     print $ (traverse (\x -> menus x) (Just "xxx")) -- []

