import Debug.Trace

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

visualizeBoard:: Board-> String


showPieceB :: Piece -> String

showPieceB(P _) = "PB"
showPieceB (B _) = "BB"
showPieceB (N _) = "NB"
showPieceB(R _) = "RB"
showPieceB (Q _) = "QB"
showPieceB (K _) = "KB"

showPieceW :: Piece -> String

showPieceW (P _) = "PW"
showPieceW (B _) = "BW"
showPieceW (N _) = "NW"
showPieceW (R _) = "RW"
showPieceW (Q _) = "QW"
showPieceW (K _) = "KW"




assocW1 :: Location -> [Piece] -> [Piece] -> String

assocW1 (x,y) [][] = "    "


assocW1 (x,y) [] (hb:tb) | (hb== Q (x,y)) || (hb== K (x,y)) || (hb== B (x,y)) || (hb== N (x,y)) || (hb== R (x,y))  || (hb== P (x,y))= " " ++ showPieceB(hb) ++ " " 
                         | otherwise = assocW1 (x,y) [] (tb)


assocW1 (x,y) (hw:tw) (hb:tb) | (hw== Q (x,y)) || (hw== K (x,y)) || (hw== B (x,y)) || (hw== N (x,y)) || (hw== R (x,y))  || (hw== P (x,y))= " " ++ showPieceW(hw) ++ " " 
                              | otherwise = assocW1 (x,y) (tw) (hb:tb)

 
                      
                                                   
visualizeBoard (White , (hw:tw) , (hb:tb))  = unlines[    "     a    b    c    d    e    f    g    h   "
                                                     ,"8 |"++assocW1 ('a' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,8)(hw:tw)(hb:tb)++"|"
                                                     ,"7 |"++assocW1 ('a' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,7)(hw:tw)(hb:tb)++"|"
                                                     ,"6 |"++assocW1 ('a' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,6)(hw:tw)(hb:tb)++"|"
                                                     ,"5 |"++assocW1 ('a' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,5)(hw:tw)(hb:tb)++"|"
                                                     ,"4 |"++assocW1 ('a' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,4)(hw:tw)(hb:tb)++"|"
                                                     ,"3 |"++assocW1 ('a' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,3)(hw:tw)(hb:tb)++"|"
                                                     ,"2 |"++assocW1 ('a' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,2)(hw:tw)(hb:tb)++"|"
                                                     ,"1 |"++assocW1 ('a' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,1)(hw:tw)(hb:tb)++"|"
                                                     ,""
                                                     ,"Turn:White"] 

                                                   
visualizeBoard (Black , (hw:tw) , (hb:tb))  = unlines[    "     a    b    c    d    e    f    g    h   "
                                                     ,"8 |"++assocW1 ('a' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,8)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,8)(hw:tw)(hb:tb)++"|"
                                                     ,"7 |"++assocW1 ('a' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,7)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,7)(hw:tw)(hb:tb)++"|"
                                                     ,"6 |"++assocW1 ('a' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,6)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,6)(hw:tw)(hb:tb)++"|"
                                                     ,"5 |"++assocW1 ('a' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,5)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,5)(hw:tw)(hb:tb)++"|"
                                                     ,"4 |"++assocW1 ('a' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,4)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,4)(hw:tw)(hb:tb)++"|"
                                                     ,"3 |"++assocW1 ('a' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,3)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,3)(hw:tw)(hb:tb)++"|"
                                                     ,"2 |"++assocW1 ('a' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,2)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,2)(hw:tw)(hb:tb)++"|"
                                                     ,"1 |"++assocW1 ('a' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('b' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('c' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('d' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('e' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('f' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('g' ,1)(hw:tw)(hb:tb)++"|"++assocW1 ('h' ,1)(hw:tw)(hb:tb)++"|"
                                                     ,""
                                                     ,"Turn:Black"] 



notOutOfBounds (x,y) = x >= 'a' && x <= 'h' && y >= 1 && y <= 8

addToChar x y = toEnum ((fromEnum x) + y)::Char
subtractChar x y = (fromEnum x) - (fromEnum y)

pieceLocation (P location) = location
pieceLocation (N location) = location
pieceLocation (K location) = location
pieceLocation (Q location) = location
pieceLocation (R location) = location
pieceLocation (B location) = location

locationExists l [] = False
locationExists l (piece:t) = l == (pieceLocation piece) || locationExists l t

-- white playing
canMoveTo location (White, white, black) = not (locationExists location white) && not (elem (K location) black)

-- black playing
canMoveTo location (Black, white, black) = not (locationExists location black) && not (elem (K location) white)

-- x2 > x1, moving right, add 1
-- x2 < x1, moving left, add -1
-- therefore add signum(x2-x1)

isPathClearHorizontal (x1,y1) (colour, white, black) (x2,_) | x1 == addToChar x2 (signum (subtractChar x1 x2)) = True
                                                            | otherwise = not (locationExists (xNew,y1) (white ++ black)) && isPathClearHorizontal (xNew,y1) (colour, white, black) (x2,y1) where xNew = addToChar x1 (signum(subtractChar x2 x1))

isPathClearVertical (x1,y1) (colour, white, black) (_,y2) | y1 == y2+signum(y1-y2) = True
                                                          | otherwise = not (locationExists (x1,yNew) (white ++ black)) && isPathClearVertical (x1,yNew) (colour, white, black) (x1,y2) where yNew = y1 + signum(y2-y1)

isPathClearDiagonal (x1,y1) (colour, white, black) (x2,y2) | y1 == y2+signum(y1-y2) && x1 == addToChar x2 (signum (subtractChar x1 x2)) = True
                                                           | otherwise = not (locationExists (xNew,yNew) (white ++ black)) && isPathClearDiagonal (xNew,yNew) (colour, white, black) (x2,y2) where {xNew = addToChar x1 (signum(subtractChar x2 x1)) ; yNew = y1 + signum(y2-y1)}

isPawnAttacking (x1,y1) (_, white, black) (x2,y2) | (elem (P (x1,y1)) white) = y2 == y1+1 && (x2 == addToChar x1 (-1) || x2 == addToChar x1 1) && (locationExists (x2,y2) black)
                                                  | (elem (P (x1,y1)) black) = y2 == y1-1 && (x2 == addToChar x1 (-1) || x2 == addToChar x1 1) && (locationExists (x2,y2) white)


isDiagonalMove (x1,y1) (x2,y2) = abs (y2 - y1) == abs (subtractChar x2 x1)
isVerticalMove (x1,y1) (x2,y2) = x2 == x1 && y2 /= y1
isHorizontalMove (x1,y1) (x2,y2) = y2 == y1 && x2 /= x1

isLegal:: Piece -> Board -> Location -> Bool
isLegal piece (_, white, black) (x2,y2) = pieceLocation piece /= (x2,y2) && notOutOfBounds (x2,y2) && canMoveTo (x2,y2) (colour, white, black) && isLegalHelper piece (colour, white, black) (x2,y2) where colour | elem piece white = White
                                                                                                                                                                                                                  | otherwise = Black

isLegalHelper:: Piece -> Board -> Location -> Bool
isLegalHelper (N (x1,y1)) (colour, white, black) (x2,y2) = abs((subtractChar x2 x1) * (y2 - y1)) == 2

isLegalHelper (P (x,y1)) (White, white, black) (x2,y2) = (((y1 == 2 && y2 == 4 && x == x2) && isPathClearVertical (x,y1) (Black, white, black) (x2,y2)) || (y2 == y1+1 && x == x2)) && not(locationExists (x2,y2) (white ++ black)) || isPawnAttacking (x,y1) (Black, white, black) (x2,y2)
isLegalHelper (P (x,y1)) (Black, white, black) (x2,y2) = (((y1 == 7 && y2 == 5 && x == x2) && isPathClearVertical (x,y1) (Black, white, black) (x2,y2)) || (y2 == y1-1 && x == x2)) && not(locationExists (x2,y2) (white ++ black)) || isPawnAttacking (x,y1) (Black, white, black) (x2,y2)

isLegalHelper (K (x1,y1)) (colour, white, black) (x2,y2) = abs (subtractChar x2 x1) <= 1 && abs (y2 - y1) <= 1
isLegalHelper (R l1) (colour, white, black) l2 = isVerticalMove l1 l2 && isPathClearVertical l1 (colour, white, black) l2 || isHorizontalMove l1 l2 && isPathClearHorizontal l1 (colour, white, black) l2
isLegalHelper (B l1) (colour, white, black) l2 = isDiagonalMove l1 l2 && isPathClearDiagonal l1 (colour, white, black) l2
isLegalHelper (Q l1) (colour, white, black) l2 = isLegalHelper (R l1) (colour, white, black) l2 || isLegalHelper (B l1) (colour, white, black) l2


suggestMove:: Piece -> Board -> [Location]


suggestMove  piece  (colour,(hw:tw),(hb:tb))  | piece `elem` (hb:tb) =   filter (isLegal  piece  (Black,(hw:tw),(hb:tb)) ) ([(x, y) | x <- ['a'..'h'], y <- [1..8]])
                                              | piece `elem` (hw:tw)=    filter (isLegal  piece  (White,(hw:tw),(hb:tb)) ) ([(x, y) | x <- ['a'..'h'], y <- [1..8]])

move:: Piece -> Location -> Board -> Board


moveP ::  Piece -> Location -> Piece
 
moveP (P _) = P
moveP (N _) = N
moveP (K _) = K
moveP(Q _) = Q
moveP (R _) = R
moveP(B _) = B



update movingpiece newlocation = map (\x -> if x == movingpiece then moveP movingpiece newlocation else x)

removePiece pieces location = filter (\piece -> location /= pieceLocation piece) pieces

move piece l (White,white,black)  | not (isLegal piece (White,white,black) l) = error ("Illegal move for piece " ++ show piece)
                                  | not (piece `elem` (white ++ black)) = error "Piece not found on board"
                                  | piece `elem` black = error "This is White player's turn, Black can't move."
                                  | otherwise = (Black , update piece l white , removePiece black l) 


move piece l (Black,white,black)  | not (isLegal piece (Black,white,black) l) = error ("Illegal move for piece " ++ show piece)
                                  | not (piece `elem` (white ++ black)) = error "Piece not found on board"
                                  | piece `elem` white = error "This is Black player's turn, White can't move."
                                  | otherwise = (White , removePiece white l , update piece l black) 