
import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Data.Word (Word8,Word16)
import Data.Aeson (Value(Null))
import Data.List.Split


data Domain = 
    RootDom String |
    NodeDom String Domain


data Path = 
    RootPath String |
    NodePath String Path


data KeyVal = KeyVal String String

data Request = 
    RootReq KeyVal |
    NodeReq KeyVal Request


data URL = MkURL {
    protocole :: String,
    domain :: Domain,
    path :: Maybe Path,
    request :: Maybe Request,
    fragment :: Maybe String
}
    


isLetter::Char -> Bool
isLetter c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))
 
isLetterOrNumber::Char -> Bool
isLetterOrNumber c = (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9'))

onlyAlpha::String -> Bool
onlyAlpha [] = True
onlyAlpha (x:xs) | isLetter x  = onlyAlpha xs
onlyAlpha _ = False

onlyAlphaNum::String -> Bool
onlyAlphaNum [] = True
onlyAlphaNum (x:xs) | isLetterOrNumber x  = onlyAlphaNum xs
onlyAlphaNum _ = False

parseUrl:: String -> Either String URL
parseUrl str = case splitOn "://" str of
    [s1,_] | not (onlyAlpha s1) -> Left "Erreur domain"
    [s1,s2] -> parseUrl2 s1 s2

    _ -> Left "Erreur domain"

parseUrl2:: String -> String -> Either String URL
parseUrl2 prot str = case splitOn "#" str of
    [s1] -> parseUrl3 prot Nothing s1
    [_,s2] | not (onlyAlpha s2) -> Left "Fragment Invalide"
    [s1,s2] -> parseUrl3 prot (Just s2) s1
    _ -> Left "Fragment Invalide"

parseSplitedRequest:: [String] -> Either String Request
parseSplitedRequest tab = case tab of
    [s1]  -> case splitOn "=" s1 of
        [s1,s2] | not (onlyAlphaNum s1) || not (onlyAlphaNum s2)  -> Left "Erreur request"
        [s1,s2] -> Right (RootReq (KeyVal s1 s2))
        _ -> Left "Erreur request"
    (x:xs) -> case splitOn "=" x of
        [s1,s2] | not (onlyAlphaNum s1) || not (onlyAlphaNum s2)  -> Left "Erreur request"
        [s1,s2] -> 
            let next = parseSplitedRequest xs in
                case next of
                    Left err -> Left err
                    Right obj -> Right (NodeReq (KeyVal s1 s2) obj)
        _ -> Left "Erreur request"
    _ -> Left "Erreur request"

parseRequest:: String -> Either String Request
parseRequest str = parseSplitedRequest (splitOn "&" str)




parseUrl3:: String -> Maybe String -> String->Either String URL
parseUrl3 prot frag str = case splitOn "?" str of 
    [s1] -> parseUrl4 prot frag Nothing s1
    [s1,s2] -> 
        let next = parseRequest s2 in
            case next of
                Left err -> Left err
                Right req -> parseUrl4 prot frag (Just req) s1
    _ -> Left "Erreur request"


parseSplitedDomain:: [String] -> Either String Domain
parseSplitedDomain tab = case tab of
    [s1] | not (onlyAlpha s1) -> Left "Erreur Domaine"
    [s1] -> Right (RootDom s1)
    (x:_) | not (onlyAlpha x) -> Left "Erreur Domaine"
    (x:xs) ->
        let next = parseSplitedDomain xs in
            case next of
                Left err -> Left err
                Right obj -> Right (NodeDom x obj)
    _ -> Left "Erreur Domaine"


parseSplitedPath:: [String] -> Either String Path
parseSplitedPath tab = case tab of
    [s1] | not (onlyAlpha s1) -> Left "Erreur Path"
    [s1] -> Right (RootPath s1)
    [s1,""] -> Right (RootPath s1)
    (x:_) | not (onlyAlpha x) -> Left "Erreur Path"
    ("":xs)-> parseSplitedPath xs
    (x:xs) ->
        let next = parseSplitedPath xs in
            case next of
                Left err -> Left err
                Right obj -> Right (NodePath x obj)
    _ -> Left "Erreur Path"


assemble:: String -> Maybe String -> Maybe Request -> Domain -> Maybe Path -> URL
assemble prot frag req dom path = MkURL{protocole = prot, domain = dom, path = path,request = req,fragment = frag}


parseUrl4::String -> Maybe String -> Maybe Request -> String -> Either String URL
parseUrl4 prot frag req str = case splitOn "/" str of 
    [s1] -> 
        let next = parseSplitedDomain (splitOn "." s1) in
            case next of
                Left err -> Left err
                Right dom -> Right (assemble prot frag req dom Nothing)
    [s1,""] -> 
        let next = parseSplitedDomain (splitOn "." s1) in
            case next of
                Left err -> Left err
                Right dom -> Right (assemble prot frag req dom Nothing)
    (x:xs) ->
        let next = parseSplitedDomain (splitOn "." x) in
            let next2 = parseSplitedPath xs in
                case (next,next2) of
                    (Left err,_) -> Left err
                    (_,Left err) -> Left err
                    (Right dom,Right path) -> Right (assemble prot frag req dom (Just path))
    _ -> Left "Erreur Path + Domain "


instance Show Domain where
    show d = case d of
        RootDom s -> s
        NodeDom x xs -> x++"."++(show xs)

instance Show Path where
    show p = case p of
        RootPath s -> "/" ++ s
        NodePath s xs -> "/"++s++(show xs)

instance Show Request where
    show r = case r of 
        RootReq (KeyVal k v) -> k++"="++v
        NodeReq (KeyVal k v) xs -> k++"="++v++"&"++(show xs)



instance Show URL where
    show url = case (url.path,url.request,url.fragment) of
        (Nothing,Nothing,Nothing) -> ( url.protocole)++"://"++(show url.domain)
        (Nothing,Nothing,Just c) -> ( url.protocole)++"://"++(show url.domain)++"#"++c
        (Nothing,Just b,Nothing) -> ( url.protocole)++"://"++(show url.domain)++"?"++(show b)
        (Nothing,Just b,Just c) -> ( url.protocole)++"://"++(show url.domain)++"?"++(show b)++"#"++(c)
        (Just a,Nothing,Nothing) -> ( url.protocole)++"://"++(show url.domain)++(show a)
        (Just a,Nothing,Just c) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"#"++(c)
        (Just a,Just b,Nothing) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"?"++(show b)
        (Just a,Just b,Just c) -> ( url.protocole)++"://"++(show url.domain)++(show a)++"?"++(show b)++"#"++(c)
        

main :: IO ()
main = let tmp = parseUrl "http://www.google.com/hi/hello/?val1=val2&t1=v2#huiaudhaiuhdz" in
    putStrLn (show tmp)