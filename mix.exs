defmodule CRC.Mixfile do
  use Mix.Project

  def project() do
    [
      app: :crc,
      version: "0.6.0",
      elixir: ">= 1.0.0 and < 2.0.0",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:elixir_make] ++ Mix.compilers(),
      make_env: %{"MIX_ENV" => to_string(Mix.env())},
      make_clean: ["clean"],
      make_cwd: "c_src",
      description: description(),
      name: "crc",
      package: package(),
      source_url: "https://github.com/TattdCodeMonkey/crc"
    ]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application() do
    # Specify extra applications you'll use from Erlang/Elixir
    [
      extra_applications: []
    ]
  end

  # Dependencies can be Hex packages:
  #
  #   {:my_dep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:my_dep, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps() do
    [
      {:credo, "~> 0.8", only: [:dev, :test], runtime: false},
      {:elixir_make, "~> 0.4", runtime: false},
      {:ex_doc, "~> 0.18", only: :dev},
      {:propcheck, "~> 1.0", only: :test}
    ]
  end

  defp description() do
    """
    A library used to calculate CRC checksums for binary data
    """
  end

  defp package() do
    [
      name: :crc,
      files: [
        "c_src",
        "CHANGELOG*",
        "include",
        "lib",
        "LICENSE*",
        "mix.exs",
        "priv",
        "README*",
        "rebar.config",
        "src"
      ],
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/TattdCodeMonkey/crc"
      },
      maintainers: ["Rodney Norris"]
    ]
  end
end
