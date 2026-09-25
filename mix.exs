defmodule Mix.Tasks.Compile.CrcNif do
  @moduledoc false
  # Builds the NIF via c_src/Makefile. Kept inline (instead of depending on
  # elixir_make) so the Hex package has no requirements and can be consumed
  # by rebar3, which uses rebar.config and can't build Mix-only deps.
  use Mix.Task.Compiler

  @impl Mix.Task.Compiler
  def run(_args) do
    case make(make_args(["all"])) do
      {_, 0} -> {:ok, []}
      {_, code} -> Mix.raise("Could not compile crc NIF, make exited with status #{code}")
    end
  end

  @impl Mix.Task.Compiler
  def clean() do
    make(make_args(["clean"]))
    :ok
  end

  defp make(args) do
    System.cmd(make_cmd(), args,
      cd: "c_src",
      env: [
        {"MIX_APP_PATH", Mix.Project.app_path()},
        {"MIX_ENV", to_string(Mix.env())}
      ],
      into: IO.stream(:stdio, :line),
      stderr_to_stdout: true
    )
  end

  defp make_args(targets) do
    case :os.type() do
      {:win32, _} -> ["/F", "Makefile.win" | targets]
      _ -> targets
    end
  end

  defp make_cmd() do
    case :os.type() do
      {:win32, _} -> "nmake"
      {:unix, os} when os in [:freebsd, :openbsd, :netbsd, :dragonfly] -> "gmake"
      _ -> "make"
    end
  end
end

defmodule CRC.Mixfile do
  use Mix.Project

  @source_url "https://github.com/TattdCodeMonkey/crc"
  @version "0.10.7"

  def project() do
    [
      app: :crc,
      version: @version,
      elixir: "~> 1.14",
      elixirc_paths: elixirc_paths(Mix.env()),
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      compilers: [:crc_nif] ++ Mix.compilers(),
      name: "crc",
      package: package(),
      deps: deps(),
      docs: docs()
    ]
  end

  def application() do
    [
      extra_applications: []
    ]
  end

  defp deps() do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:propcheck, "~> 1.0", only: :test}
    ]
  end

  # Specifies which paths to compile per environment
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp package() do
    [
      name: :crc,
      description: "A library used to calculate CRC checksums for binary data.",
      files: [
        "c_src/nif/*.c",
        "c_src/nif/*.h",
        "c_src/Makefile",
        "c_src/Makefile.win",
        "lib",
        "LICENSE*",
        "mix.exs",
        "README*",
        "rebar.config",
        "src"
      ],
      maintainers: ["Rodney Norris"],
      licenses: ["MIT"],
      links: %{
        "Changelog" => "https://hexdocs.pm/crc/changelog.html",
        "GitHub" => @source_url
      }
    ]
  end

  defp docs do
    [
      extras: [
        "CHANGELOG.md",
        "LICENSE.md": [title: "License"],
        "README.md": [title: "Overview"]
      ],
      main: "readme",
      source_url: @source_url,
      source_ref: "v#{@version}",
      api_reference: false,
      formatters: ["html"]
    ]
  end
end
