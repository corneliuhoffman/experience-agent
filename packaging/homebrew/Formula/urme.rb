# Canonical tap formula (template). The release workflow's bump-tap job
# copies this file into urme-dev/homebrew-urme as Formula/urme.rb,
# substituting @VERSION@ and the two @..._SHA256@ placeholders — so edit
# structure HERE, never in the tap.
class Urme < Formula
  desc "TUI + MCP server: git/Claude session history and an annotated call graph"
  homepage "https://github.com/urme-dev/urme"
  version "@VERSION@"
  license "MIT"

  # urme shells out to git at runtime; everything else is bundled into
  # the tarball (macos: dylibbundler, linux: bundled .so + $ORIGIN rpath).
  depends_on "git"

  on_macos do
    on_arm do
      url "https://github.com/urme-dev/urme/releases/download/v@VERSION@/urme-@VERSION@-arm64-darwin.tar.gz"
      sha256 "@ARM64_DARWIN_SHA256@"
    end
  end

  on_linux do
    on_intel do
      url "https://github.com/urme-dev/urme/releases/download/v@VERSION@/urme-@VERSION@-x86_64-linux.tar.gz"
      sha256 "@X86_64_LINUX_SHA256@"
    end
  end

  def install
    # Both tarballs ship `urme` + `lib/` side-by-side. Install into
    # libexec so the binary's RPATH / @executable_path resolves the
    # bundled libs, then symlink into bin.
    libexec.install "urme", "lib"
    bin.install_symlink libexec/"urme"
  end

  def caveats
    <<~EOS
      Session indexing and the annotation pipeline drive the `claude` CLI
      (Claude Code) as a subprocess — install and log it in separately.

      Code-navigation quickstart (needs a call-graph export; see the README
      section 'Call-graph JSON format'):
        urme graph-init
        urme annotate
    EOS
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/urme --version")
  end
end
