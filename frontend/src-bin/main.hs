import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

main :: IO ()
main = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
