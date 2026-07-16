# Canonical copy of the tap formula. Lives in urme-dev/homebrew-urme as
# Formula/urme.rb; the release workflow's tap-bump job rewrites the
# version/sha256 lines there on every tagged release. Edit THIS file for
# structural changes and copy it over.
class Urme < Formula
  desc "TUI + MCP server: git/Claude session history and an annotated call graph"
  homepage "https://github.com/urme-dev/urme"
  version "0.2.0"
  license "MIT"

  on_macos do
    # Prebuilt binaries are arm64-only for now; Intel Macs build from source.
    depends_on arch: :arm64
    url "https://github.com/urme-dev/urme/releases/download/v#{version}/urme-#{version}-arm64-darwin.tar.gz"
    sha256 "REPLACE_ARM64_DARWIN_SHA256"
  end

  on_linux do
    depends_on arch: :x86_64
    url "https://github.com/urme-dev/urme/releases/download/v#{version}/urme-#{version}-x86_64-linux.tar.gz"
    sha256 "REPLACE_X86_64_LINUX_SHA256"
  end

  def install
    # The tarball ships `urme` + `lib/` with install names rewritten to
    # @executable_path/lib ($ORIGIN/lib on Linux), so binary and libs
    # must stay siblings: install both under libexec and symlink into bin.
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
