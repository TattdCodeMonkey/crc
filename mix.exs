defmodule CRC.Mixfile do use Mix.Project 
  def project() do
    [
      app: :crc,
      version: "0.8.1",
      elixir: ">= 1.4.2 and < 2.0.0",
      elixirc_paths: elixirc_paths(Mix.env()),
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

  def application() do
    [
      extra_applications: []
    ]
  end

  defp deps() do
    [
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

  # Specifies which paths to compile per environment
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_),     do: ["lib"]

  defp package() do
    [
      name: :crc,
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
      licenses: ["MIT"],
      links: %{
        "GitHub" => "https://github.com/TattdCodeMonkey/crc"
      },
      maintainers: ["Rodney Norris"]
    ]
  end
end
