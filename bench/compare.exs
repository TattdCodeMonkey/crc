# Compares the NIF (crc_fast) with the pure engine (crc_pure) and prints
# Markdown tables of throughput. Needs the NIF, so run it on v0.12.x:
#
#     MIX_ENV=prod mix run bench/compare.exs
#
# Throughput is in MB/s (10^6 bytes per second, higher is faster). Diff is
# the pure engine's throughput as a multiple of the NIF's.

models = [:crc_8, :crc_16, :crc_32, :crc_64]

groups = [
  {"Small inputs", [16, 256, 4_096]},
  {"Large inputs", [65_536, 1_048_576]}
]

impls = [nif: &:crc_fast.calc/2, pure: &:crc_pure.calc/2]

# Microseconds per call: the fastest of 3 runs, each repeating the call
# enough times to cover about 4 MB of input.
measure = fn fun, model, input ->
  iterations = max(3, min(20_000, div(4_000_000, byte_size(input))))

  for _ <- 1..3 do
    {us, _} = :timer.tc(fn -> for _ <- 1..iterations, do: fun.(model, input) end)
    us / iterations
  end
  |> Enum.min()
end

label = fn
  size when size >= 1_048_576 -> "#{div(size, 1_048_576)} MB"
  size when size >= 1024 -> "#{div(size, 1024)} KB"
  size -> "#{size} B"
end

mbps = fn size, us -> size / us end

for {title, sizes} <- groups do
  headers =
    Enum.flat_map(sizes, fn size ->
      ["#{label.(size)} NIF", "#{label.(size)} Pure", "#{label.(size)} Diff"]
    end)

  IO.puts("**#{title}**\n")
  IO.puts("| Model | " <> Enum.join(headers, " | ") <> " |")
  IO.puts("|---|" <> String.duplicate("---|", length(headers)))

  for model <- models do
    cells =
      Enum.flat_map(sizes, fn size ->
        input = :crypto.strong_rand_bytes(size)
        # warm up the pure engine's cached tables before timing
        :crc_pure.calc(model, input)

        [nif, pure] =
          for {_name, fun} <- impls, do: mbps.(size, measure.(fun, model, input))

        diff = pure / nif
        diff_cell = :erlang.float_to_binary(diff, decimals: 2) <> "x"
        diff_cell = if diff >= 1, do: "**#{diff_cell}**", else: diff_cell
        [round(nif), round(pure), diff_cell]
      end)

    IO.puts("| #{model} | " <> Enum.join(cells, " | ") <> " |")
  end

  IO.puts("")
end

cpu =
  case :os.type() do
    {:unix, :darwin} ->
      System.cmd("sysctl", ["-n", "machdep.cpu.brand_string"]) |> elem(0) |> String.trim()

    {:unix, :linux} ->
      File.read!("/proc/cpuinfo")
      |> String.split("\n")
      |> Enum.find_value("unknown", fn line ->
        case String.split(line, ":", parts: 2) do
          [key, value] when key in ["model name\t", "Model\t\t"] -> String.trim(value)
          _ -> nil
        end
      end)

    _ ->
      "unknown"
  end

IO.puts(
  "Platform: #{cpu} (#{:erlang.system_info(:system_architecture)}), " <>
    "OTP #{System.otp_release()} (#{:erlang.system_info(:emu_flavor)}), Elixir #{System.version()}"
)
