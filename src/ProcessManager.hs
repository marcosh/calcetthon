module ProcessManager where

-- eventful-core
import           Eventful.ProcessManager (ProcessManager (ProcessManager),
                                          ProcessManagerCommand)
import           Eventful.Projection     (Projection (..))
import           Eventful.Store.Class    (StreamEvent, VersionedStreamEvent)
import           Eventful.UUID           (UUID)

processManager :: ProcessManager () event command
processManager = ProcessManager
    processManagerProjection
    processManagerPendingCommands
    processManagerPendingEvents

processManagerProjection :: Projection () (VersionedStreamEvent event)
processManagerProjection = Projection () handleEvent

handleEvent :: () -> VersionedStreamEvent event -> ()
handleEvent = undefined

processManagerPendingCommands :: () -> [ProcessManagerCommand event command]
processManagerPendingCommands = undefined

processManagerPendingEvents :: () -> [StreamEvent UUID () event]
processManagerPendingEvents = undefined
