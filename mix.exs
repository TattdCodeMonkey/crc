defmodule Mix.Tasks.Compile.Crc do
  @shortdoc "Compiles CRC"

  def run(_) do
    if match? {:win32, _}, :os.type do
      #{result, _error_code} = System.cmd("nmake", ["/F", "Makefile.win", "priv\\crc.dll"], stderr_to_stdout: true)
      #IO.binwrite result
    else
      {result, _error_code} = System.cmd("make", ["priv/crc.so"], stderr_to_stdout: true)
      IO.binwrite result
    end

    :ok
  end
end

defmodule Crc.Mixfile do
  use Mix.Project

  def project do
    [app: :crc,
     version: "0.0.1",
     elixir: "~> 1.1",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     compilers: [:Crc, :elixir, :app],
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end
end
