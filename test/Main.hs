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
import Hat.Model.PasteBuffersSpec qualified
import Hat.Server.PersistSpec qualified
import Hat.Server.FlashSpec qualified
import Hat.Server.FloatingSpec qualified
import Hat.Term.PtySpec qualified
import Hat.Server.AlertsSpec qualified
import Hat.Server.BufferSpec qualified
import Hat.Server.CaptureSpec qualified
import Hat.Server.ClipboardSpec qualified
import Hat.Server.ClockSpec qualified
import Hat.Server.ColorSchemeSpec qualified
import Hat.Server.CommandTypesSpec qualified
import Hat.Server.ConfigSpec qualified
import Hat.Server.ControlSpec qualified
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
import Hat.Server.WinlinkSpec qualified
import Hat.Server.OptionEffectSpec qualified
import Hat.Server.OptionParseSpec qualified
import Hat.Server.OptionsSpec qualified
import Hat.Server.PaneCommandSpec qualified
import Hat.Server.PaneReaderSpec qualified
import Hat.Server.PickerSpec qualified
import Hat.Server.PromptSpec qualified
import Hat.Server.PromptHistorySpec qualified
import Hat.Server.ReloadSpec qualified
import Hat.Server.SceneSpec qualified
import Hat.Server.ThemeSpec qualified
import Hat.Server.RenderSpec qualified
import Hat.Server.ResizeRenderSpec qualified
import Hat.Server.RestoreSpec qualified
import Hat.Server.SessionSpec qualified
import Hat.Server.SendSpec qualified
import Hat.Server.StatusSpec qualified
import Hat.Server.MenuSpec qualified
import Hat.Server.MessageSpec qualified
import Hat.Server.MouseSpec qualified
import Hat.Server.StyleSpec qualified
import Hat.Server.FormatDrawSpec qualified
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
main = hspec $ parallel $ do
    runIO boundFds
    -- Integration first: its items (each a real hat through a pty) are the
    -- suite's heaviest.
    describe "integration" Hat.IntegrationSpec.spec
    describe "Hat.Path" Hat.PathSpec.spec
    describe "Hat.Model.PasteBuffers" Hat.Model.PasteBuffersSpec.spec
    describe "Hat.Debug" Hat.DebugSpec.spec
    describe "Hat.Bench.Linear" Hat.Bench.LinearSpec.spec
    describe "Hat.Bench.PerfStat" Hat.Bench.PerfStatSpec.spec
    describe "Hat.Bench.Report" Hat.Bench.ReportSpec.spec
    describe "Hat.Bench.Residency" Hat.Bench.ResidencySpec.spec
    describe "Hat.Bench.RtsStats" Hat.Bench.RtsStatsSpec.spec
    describe "Hat.Intern" Hat.InternSpec.spec
    describe "Hat.Log" Hat.LogSpec.spec
    describe "Hat.Transport.Socket" Hat.Transport.SocketSpec.spec
    describe "Hat.Term.Pty" Hat.Term.PtySpec.spec
    describe "Hat.Server.Persist" Hat.Server.PersistSpec.spec
    describe "Hat.Term.Emulator" Hat.Term.EmulatorSpec.spec
    describe "Hat.Term golden" Hat.Term.GoldenSpec.spec
    describe "Hat.Transport.Wire" Hat.Transport.WireSpec.spec
    describe "Hat.Server.Render" Hat.Server.RenderSpec.spec
    describe "Hat.Server.ResizeRender" Hat.Server.ResizeRenderSpec.spec
    describe "Hat.Server.Reload" Hat.Server.ReloadSpec.spec
    describe "Hat.Server.Scene" Hat.Server.SceneSpec.spec
    describe "Hat.Server.Theme" Hat.Server.ThemeSpec.spec
    describe "Hat.Client.Draw" Hat.Client.DrawSpec.spec
    describe "Hat.Client.Tty" Hat.Client.TtySpec.spec
    describe "Hat.Server.Restore" Hat.Server.RestoreSpec.spec
    describe "Hat.Server.Session" Hat.Server.SessionSpec.spec
    describe "Hat.Server.Layout" Hat.Server.LayoutSpec.spec
    describe "Hat.Server.LayoutString" Hat.Server.LayoutStringSpec.spec
    describe "Hat.Server.Mru" Hat.Server.MruSpec.spec
    describe "Hat.Server.OptionParse" Hat.Server.OptionParseSpec.spec
    describe "Hat.Server.Winlink" Hat.Server.WinlinkSpec.spec
    describe "Hat.Server.Options" Hat.Server.OptionsSpec.spec
    describe "Hat.Server.PaneCommand" Hat.Server.PaneCommandSpec.spec
    describe "Hat.Server.OptionEffect" Hat.Server.OptionEffectSpec.spec
    describe "Hat.Server.Environ" Hat.Server.EnvironSpec.spec
    describe "Hat.Server.Alerts" Hat.Server.AlertsSpec.spec
    describe "Hat.Server.Buffer" Hat.Server.BufferSpec.spec
    describe "Hat.Server.Capture" Hat.Server.CaptureSpec.spec
    describe "Hat.Server.Clipboard" Hat.Server.ClipboardSpec.spec
    describe "Hat.Server.Clock" Hat.Server.ClockSpec.spec
    describe "Hat.Server.PaneReader" Hat.Server.PaneReaderSpec.spec
    describe "Hat.Server.Picker" Hat.Server.PickerSpec.spec
    describe "Hat.Server.Status" Hat.Server.StatusSpec.spec
    describe "Hat.Server.Menu" Hat.Server.MenuSpec.spec
    describe "Hat.Server.Message" Hat.Server.MessageSpec.spec
    describe "Hat.Server.Mouse" Hat.Server.MouseSpec.spec
    describe "Hat.Server.Style" Hat.Server.StyleSpec.spec
    describe "Hat.Server.FormatDraw" Hat.Server.FormatDrawSpec.spec
    describe "Hat.Server.Target" Hat.Server.TargetSpec.spec
    describe "Hat.Server.Title" Hat.Server.TitleSpec.spec
    describe "Hat.Command.Parser" Hat.Command.ParserSpec.spec
    describe "Hat.FuzzyMatch" Hat.FuzzyMatchSpec.spec
    describe "Hat.Glob" Hat.GlobSpec.spec
    describe "Hat.Server.ColorScheme" Hat.Server.ColorSchemeSpec.spec
    describe "Hat.Server.Command.Types" Hat.Server.CommandTypesSpec.spec
    describe "Hat.Server.Config" Hat.Server.ConfigSpec.spec
    describe "Hat.Server.Keys" Hat.Server.KeysSpec.spec
    describe "Hat.Server.CopyMode" Hat.Server.CopyModeSpec.spec
    describe "Hat.Server.DisplayPanes" Hat.Server.DisplayPanesSpec.spec
    describe "Hat.Server.Prompt" Hat.Server.PromptSpec.spec
    describe "Hat.Server.PromptHistory" Hat.Server.PromptHistorySpec.spec
    describe "Hat.Server.send" Hat.Server.SendSpec.spec
    describe "Hat.Server.Format" Hat.Server.FormatSpec.spec
    describe "Hat.Server.Hooks" Hat.Server.HooksSpec.spec
    describe "Hat.Server.Control" Hat.Server.ControlSpec.spec
    describe "Hat.Server.Flags" Hat.Server.FlagsSpec.spec
    describe "Hat.Server.Flash" Hat.Server.FlashSpec.spec
    describe "Hat.Server.Floating" Hat.Server.FloatingSpec.spec
