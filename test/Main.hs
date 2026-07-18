import Test.Hspec

import qualified Hat.Client.DrawSpec
import qualified Hat.Command.ParserSpec
import qualified Hat.FuzzyMatchSpec
import qualified Hat.IntegrationSpec
import qualified Hat.PathSpec
import qualified Hat.Server.PersistSpec
import qualified Hat.Term.PtySpec
import qualified Hat.Server.ColorSchemeSpec
import qualified Hat.Server.ConfigSpec
import qualified Hat.Server.CopyModeSpec
import qualified Hat.Server.FlagsSpec
import qualified Hat.Server.FormatSpec
import qualified Hat.Server.KeysSpec
import qualified Hat.Server.LayoutSpec
import qualified Hat.Server.LayoutStringSpec
import qualified Hat.Server.OptionEffectSpec
import qualified Hat.Server.OptionsSpec
import qualified Hat.Server.PickerSpec
import qualified Hat.Server.PromptSpec
import qualified Hat.Server.ReloadSpec
import qualified Hat.Server.RenderSpec
import qualified Hat.Server.RestoreSpec
import qualified Hat.Server.SessionSpec
import qualified Hat.Server.SendSpec
import qualified Hat.Server.StyleSpec
import qualified Hat.Server.TargetSpec
import qualified Hat.Server.TitleSpec
import qualified Hat.Transport.SocketSpec
import qualified Hat.Term.EmulatorSpec
import qualified Hat.Term.GoldenSpec
import qualified Hat.Transport.WireSpec

main :: IO ()
main = hspec $ do
    describe "Hat.Path" Hat.PathSpec.spec
    describe "Hat.Transport.Socket" Hat.Transport.SocketSpec.spec
    describe "Hat.Term.Pty" Hat.Term.PtySpec.spec
    describe "Hat.Server.Persist" Hat.Server.PersistSpec.spec
    describe "Hat.Term.Emulator" Hat.Term.EmulatorSpec.spec
    describe "Hat.Term golden" Hat.Term.GoldenSpec.spec
    describe "Hat.Transport.Wire" Hat.Transport.WireSpec.spec
    describe "Hat.Server.Render" Hat.Server.RenderSpec.spec
    describe "Hat.Server.Reload" Hat.Server.ReloadSpec.spec
    describe "Hat.Client.Draw" Hat.Client.DrawSpec.spec
    describe "Hat.Server.Restore" Hat.Server.RestoreSpec.spec
    describe "Hat.Server.Session" Hat.Server.SessionSpec.spec
    describe "Hat.Server.Layout" Hat.Server.LayoutSpec.spec
    describe "Hat.Server.LayoutString" Hat.Server.LayoutStringSpec.spec
    describe "Hat.Server.Options" Hat.Server.OptionsSpec.spec
    describe "Hat.Server.OptionEffect" Hat.Server.OptionEffectSpec.spec
    describe "Hat.Server.Picker" Hat.Server.PickerSpec.spec
    describe "Hat.Server.Style" Hat.Server.StyleSpec.spec
    describe "Hat.Server.Target" Hat.Server.TargetSpec.spec
    describe "Hat.Server.Title" Hat.Server.TitleSpec.spec
    describe "Hat.Command.Parser" Hat.Command.ParserSpec.spec
    describe "Hat.FuzzyMatch" Hat.FuzzyMatchSpec.spec
    describe "Hat.Server.ColorScheme" Hat.Server.ColorSchemeSpec.spec
    describe "Hat.Server.Config" Hat.Server.ConfigSpec.spec
    describe "Hat.Server.Keys" Hat.Server.KeysSpec.spec
    describe "Hat.Server.CopyMode" Hat.Server.CopyModeSpec.spec
    describe "Hat.Server.Prompt" Hat.Server.PromptSpec.spec
    describe "Hat.Server.send" Hat.Server.SendSpec.spec
    describe "Hat.Server.Format" Hat.Server.FormatSpec.spec
    describe "Hat.Server.Flags" Hat.Server.FlagsSpec.spec
    describe "integration" Hat.IntegrationSpec.spec
