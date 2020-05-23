module QntSyn.DepAnal where

import QntSyn
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Functor
import Data.Function
import Data.List

mkDepGroups :: M.Map Identifier (S.Set Identifier) -> [S.Set Identifier]
mkDepGroups xs =
  let g = mkDepGraph xs
      l = visitAll g
      grouped = assignAll g l

      f name group gs =
        if group `M.member` gs
        then M.adjust (S.insert name) group gs
        else M.insert group (S.singleton name) gs

      groups = M.foldrWithKey f M.empty grouped
  in sortDepGroups xs S.empty $ M.elems groups

sortDepGroups :: M.Map Identifier (S.Set Identifier) -> S.Set Identifier -> [S.Set Identifier] -> [S.Set Identifier]

sortDepGroups _ _ [] = []

sortDepGroups refs done groups =
  let getGroupRefs g = S.unions ((refs M.!) <$> S.toList g)
      getExtGroupRefs g = getGroupRefs g S.\\ g S.\\ done
      groupRefs = (\g -> (g, getExtGroupRefs g)) <$> groups
      emptys = filter (\(_, x) -> S.null x) groupRefs
      toAdd = fst $ head emptys
  in toAdd : sortDepGroups refs (S.union done toAdd) (groups \\ [toAdd])

type Graph = M.Map Identifier GraphRefs

data GraphRefs = GraphRefs
  { getInRefs  :: S.Set Identifier
  , getOutRefs :: S.Set Identifier
  } deriving (Show)

mkDepGraph :: M.Map Identifier (S.Set Identifier) -> Graph
mkDepGraph xs =
  let f name outRefs =
        let inRefs = M.keysSet $ M.filter (name `S.member`) xs
        in GraphRefs inRefs outRefs
  in f `M.mapWithKey` xs

-- Implementation of Kosaraju's algorithm
-- Algorithm sourced from https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm

visitAll :: Graph
         -> [Identifier]

visitAll g = concat $ reverse $ evalState (visit g `mapM` M.keys g) S.empty

visit :: Graph
      -> Identifier
      -> State (S.Set Identifier) [Identifier]

visit g u =
  get >>= \visited ->
  if u `S.member` visited
  then pure []
  else
    let node = g M.! u
    in modify (S.insert u) *>
       visit g `mapM` S.toList (getOutRefs node) <&>
         (u:) . concat . reverse

assignAll :: Graph
          -> [Identifier]
          -> M.Map Identifier Identifier

assignAll g l = foldl (\c x -> assign g x x c) M.empty l

assign :: Graph
       -> Identifier -- root
       -> Identifier -- u
       -> M.Map Identifier Identifier
       -> M.Map Identifier Identifier

assign g root u compnts =
  if u `M.member` compnts
  then compnts
  else let node = g M.! u
           compnts' = M.insert u root compnts
       in S.foldr' (assign g root) compnts' (getInRefs node)
