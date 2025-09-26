import Std.Internal.Http
import Std.Internal.Async.TCP
import Std.Time

open Std.Internal.IO.Async

def extractZoneId (p : Std.Http.URI.Path) : Option String :=
  let segs := p.segments.filter (!·.isEmpty)
  if segs.isEmpty then
    none
  else
    some (String.intercalate "/" segs.toList)

def getTime (req : Std.Http.Request Std.Http.Body) : Async (Std.Http.Response Std.Http.Body) := do
  if let some p := req.head.uri.path?.bind extractZoneId then
    try
      let t ← Std.Time.ZonedDateTime.nowAt p
      return .ok s!"<html><body>The current time in {p} is {t}.</body></html>"
    catch _ =>
      return .notFound s!"<html><body>Time zone {p} was not found.</body></html>"
  else
    return Std.Http.Response.new
      |>.status .movedPermanently
      |>.header "Location" (.new "/Europe/Berlin")
      |>.text s!"Please go to /Europe/Berlin"

def main : IO Unit := do
  (Std.Http.Server.serve (.v6 ⟨.ofParts 0 0 0 0 0 0 0 0, 8007⟩) getTime).block
