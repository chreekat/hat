import System.Posix.Resource
    (Resource (..), ResourceLimit (..), ResourceLimits (..), getResourceLimit,
     setResourceLimit)
import Test.Hspec

import Hat.Bench.LinearSpec qualified
import Hat.Bench.PerfStatSpec qualified
import Hat.Bench.ReportSpec qualified
import Hat.Bench.ResidencySpec qualified
import Hat.Bench.RtsStatsSpec qualified
import Hat.Client.DrawSpec qualified
import Hat.Client.TtySpec qualified
import Hat.Command.ParserSpec qualified
import Hat.DebugSpec qualified
import Hat.FuzzyMatchSpec qualified
import Hat.GlobSpec qualified
import Hat.InternSpec qualified
import Hat.IntegrationSpec qualified
import Hat.LogSpec qualified
import Hat.PathSpec qualified
import Hat.Server.PersistSpec qualified
import Hat.Server.FlashSpec qualified
import Hat.Term.PtySpec qualified
import Hat.Server.BufferSpec qualified
import Hat.Server.CaptureSpec qualified
import Hat.Server.ColorSchemeSpec qualified
import Hat.Server.ConfigSpec qualified
import Hat.Server.CopyModeSpec qualified
import Hat.Server.DisplayPanesSpec qualified
import Hat.Server.EnvironSpec qualified
import Hat.Server.FlagsSpec qualified
import Hat.Server.FormatSpec qualified
import Hat.Server.HooksSpec qualified
import Hat.Server.KeysSpec qualified
import Hat.Server.LayoutSpec qualified
import Hat.Server.LayoutStringSpec qualified
import Hat.Server.MruSpec qualified
import Hat.Server.OptionEffectSpec qualified
import Hat.Server.OptionsSpec qualified
import Hat.Server.PickerSpec qualified
import Hat.Server.PromptSpec qualified
import Hat.Server.ReloadSpec qualified
import Hat.Server.RenderSpec qualified
import Hat.Server.ResizeRenderSpec qualified
import Hat.Server.RestoreSpec qualified
import Hat.Server.SessionSpec qualified
import Hat.Server.SendSpec qualified
import Hat.Server.StyleSpec qualified
import Hat.Server.TargetSpec qualified
import Hat.Server.TitleSpec qualified
import Hat.Transport.SocketSpec qualified
import Hat.Term.EmulatorSpec qualified
import Hat.Term.GoldenSpec qualified
import Hat.Transport.WireSpec qualified

-- | Bound the fd table so each close_fds spawn scans a small range; the
-- hard limit stays untouched.
boundFds :: IO ()
boundFds = do
    ResourceLimits _ hard <- getResourceLimit ResourceOpenFiles
    setResourceLimit ResourceOpenFiles
        ResourceLimits { softLimit = ResourceLimit 2048, hardLimit = hard }

main :: IO ()
main = hspec $ do
    runIO boundFds
    -- Integration first: its items (each a real hat through a pty) are the
    -- suite's heaviest.
    describe "integration" Hat.IntegrationSpec.spec
    -- Specs over private state (own ServerState, mkdtemp dirs, emulators)
    -- run parallel. Sequential stays the default only where an item is
    -- wall-clock-sensitive (ResizeRender's frame pacing, Flash timers) or
    -- shares a process-wide fixture (Pty's throwaway HOME, Tty's terminal).
    describe "Hat.Path" (parallel Hat.PathSpec.spec)
    describe "Hat.Debug" Hat.DebugSpec.spec
    describe "Hat.Bench.Linear" (parallel Hat.Bench.LinearSpec.spec)
    describe "Hat.Bench.PerfStat" (parallel Hat.Bench.PerfStatSpec.spec)
    describe "Hat.Bench.Report" (parallel Hat.Bench.ReportSpec.spec)
    describe "Hat.Bench.Residency" (parallel Hat.Bench.ResidencySpec.spec)
    describe "Hat.Bench.RtsStats" (parallel Hat.Bench.RtsStatsSpec.spec)
    describe "Hat.Intern" (parallel Hat.InternSpec.spec)
    describe "Hat.Log" Hat.LogSpec.spec
    describe "Hat.Transport.Socket" Hat.Transport.SocketSpec.spec
    describe "Hat.Term.Pty" Hat.Term.PtySpec.spec
    describe "Hat.Server.Persist" (parallel Hat.Server.PersistSpec.spec)
    describe "Hat.Term.Emulator" (parallel Hat.Term.EmulatorSpec.spec)
    describe "Hat.Term golden" (parallel Hat.Term.GoldenSpec.spec)
    describe "Hat.Transport.Wire" (parallel Hat.Transport.WireSpec.spec)
    describe "Hat.Server.Render" (parallel Hat.Server.RenderSpec.spec)
    describe "Hat.Server.ResizeRender" Hat.Server.ResizeRenderSpec.spec
    describe "Hat.Server.Reload" (parallel Hat.Server.ReloadSpec.spec)
    describe "Hat.Client.Draw" (parallel Hat.Client.DrawSpec.spec)
    describe "Hat.Client.Tty" Hat.Client.TtySpec.spec
    describe "Hat.Server.Restore" (parallel Hat.Server.RestoreSpec.spec)
    describe "Hat.Server.Session" (parallel Hat.Server.SessionSpec.spec)
    describe "Hat.Server.Layout" (parallel Hat.Server.LayoutSpec.spec)
    describe "Hat.Server.LayoutString" (parallel Hat.Server.LayoutStringSpec.spec)
    describe "Hat.Server.Mru" (parallel Hat.Server.MruSpec.spec)
    describe "Hat.Server.Options" (parallel Hat.Server.OptionsSpec.spec)
    describe "Hat.Server.OptionEffect" (parallel Hat.Server.OptionEffectSpec.spec)
    describe "Hat.Server.Environ" (parallel Hat.Server.EnvironSpec.spec)
    describe "Hat.Server.Buffer" (parallel Hat.Server.BufferSpec.spec)
    describe "Hat.Server.Capture" (parallel Hat.Server.CaptureSpec.spec)
    describe "Hat.Server.Picker" (parallel Hat.Server.PickerSpec.spec)
    describe "Hat.Server.Style" (parallel Hat.Server.StyleSpec.spec)
    describe "Hat.Server.Target" (parallel Hat.Server.TargetSpec.spec)
    describe "Hat.Server.Title" (parallel Hat.Server.TitleSpec.spec)
    describe "Hat.Command.Parser" (parallel Hat.Command.ParserSpec.spec)
    describe "Hat.FuzzyMatch" (parallel Hat.FuzzyMatchSpec.spec)
    describe "Hat.Glob" (parallel Hat.GlobSpec.spec)
    describe "Hat.Server.ColorScheme" (parallel Hat.Server.ColorSchemeSpec.spec)
    describe "Hat.Server.Config" (parallel Hat.Server.ConfigSpec.spec)
    describe "Hat.Server.Keys" (parallel Hat.Server.KeysSpec.spec)
    describe "Hat.Server.CopyMode" (parallel Hat.Server.CopyModeSpec.spec)
    describe "Hat.Server.DisplayPanes" (parallel Hat.Server.DisplayPanesSpec.spec)
    describe "Hat.Server.Prompt" (parallel Hat.Server.PromptSpec.spec)
    describe "Hat.Server.send" (parallel Hat.Server.SendSpec.spec)
    describe "Hat.Server.Format" (parallel Hat.Server.FormatSpec.spec)
    describe "Hat.Server.Hooks" (parallel Hat.Server.HooksSpec.spec)
    describe "Hat.Server.Flags" (parallel Hat.Server.FlagsSpec.spec)
    describe "Hat.Server.Flash" Hat.Server.FlashSpec.spec
